// The H5 expression sidecar straight from an AnnData CSR matrix or from the
// slots of a genes x cells dgCMatrix, and group summaries straight from a
// sidecar, without a transposed copy of the matrix in R.
//
// An AnnData CSR matrix is stored cell by cell (cells x genes); the sidecar
// holds the same matrix gene by gene (the CSC of cells x genes) so that the
// Viewer reads one gene with one hyperslab. The arrays are read with the HDF5
// C API as stored (float32 values stay 4 bytes, 64-bit offsets stay 64-bit),
// transposed by a counting sort on several threads, and written with the
// datasets, chunks and maximum sizes that .writeTENxH5() writes; values stay
// float32 when they were, and at level 0 data and indices have no filters. R adds the shape and
// the names.

#include <Rcpp.h>
#include <hdf5.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <cstdint>
#include <string>
#include <vector>

using namespace Rcpp;

namespace {

const hsize_t DATA_CHUNK = 16384;
const hsize_t INDPTR_CHUNK = 4096;
const size_t GROUP_STATS_BLOCK = (size_t)1 << 26;

struct Handle {
  hid_t id;
  herr_t (*close)(hid_t);
  Handle(hid_t id, herr_t (*close)(hid_t)) : id(id), close(close) {}
  ~Handle() {
    if (id >= 0) close(id);
  }
  Handle(const Handle &) = delete;
  Handle &operator=(const Handle &) = delete;
};

hsize_t dataset_length(hid_t file, const std::string &name) {
  Handle d(H5Dopen2(file, name.c_str(), H5P_DEFAULT), H5Dclose);
  if (d.id < 0) stop("Cannot open dataset `" + name + "`.");
  Handle s(H5Dget_space(d.id), H5Sclose);
  return (hsize_t)H5Sget_simple_extent_npoints(s.id);
}

// a dataset's values are 4-byte floats (read as such) or anything else (read as double)
bool is_float32(hid_t file, const std::string &name) {
  Handle d(H5Dopen2(file, name.c_str(), H5P_DEFAULT), H5Dclose);
  if (d.id < 0) stop("Cannot open dataset `" + name + "`.");
  Handle t(H5Dget_type(d.id), H5Tclose);
  return H5Tget_class(t.id) == H5T_FLOAT && H5Tget_size(t.id) == 4;
}

// `count` values from `start` of a 1-d dataset, converted to `memtype`
template <class T>
void read_range(hid_t file, const std::string &name, hid_t memtype, hsize_t start, hsize_t count, T *out) {
  if (count == 0) return;
  Handle d(H5Dopen2(file, name.c_str(), H5P_DEFAULT), H5Dclose);
  if (d.id < 0) stop("Cannot open dataset `" + name + "`.");
  Handle fs(H5Dget_space(d.id), H5Sclose);
  H5Sselect_hyperslab(fs.id, H5S_SELECT_SET, &start, NULL, &count, NULL);
  Handle ms(H5Screate_simple(1, &count, NULL), H5Sclose);
  if (H5Dread(d.id, memtype, ms.id, fs.id, H5P_DEFAULT, out) < 0) stop("Cannot read dataset `" + name + "`.");
}

template <class T>
void read_all(hid_t file, const std::string &name, hid_t memtype, std::vector<T> &out) {
  out.resize(dataset_length(file, name));
  read_range(file, name, memtype, 0, out.size(), out.data());
}

// a chunked 1-d dataset as rhdf5::h5createDataset(chunk, level, maxdims) makes
// it; `filtered` = false leaves out the shuffle and deflate filters at level
// 0, where they store the values as they are and only cost time on every
// write and read
void write_all(hid_t file, const std::string &name, hid_t filetype, hid_t memtype, const void *values, hsize_t n,
               hsize_t maxn, hsize_t chunk, int level, bool filtered) {
  if (chunk > maxn) chunk = maxn;
  Handle space(H5Screate_simple(1, &n, &maxn), H5Sclose);
  Handle dcpl(H5Pcreate(H5P_DATASET_CREATE), H5Pclose);
  H5Pset_chunk(dcpl.id, 1, &chunk);
  if (filtered) {
    H5Pset_shuffle(dcpl.id);
    H5Pset_deflate(dcpl.id, (unsigned)level);
  }
  Handle d(H5Dcreate2(file, name.c_str(), filetype, space.id, H5P_DEFAULT, dcpl.id, H5P_DEFAULT), H5Dclose);
  if (d.id < 0) stop("Cannot create dataset `" + name + "`.");
  if (n > 0 && H5Dwrite(d.id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, values) < 0) {
    stop("Cannot write dataset `" + name + "`.");
  }
}

int thread_count(int threads) {
#ifdef _OPENMP
  return threads < 1 ? 1 : threads;
#else
  (void)threads;
  return 1;
#endif
}

// Per gene of [g0, g1) and group: the sum of the values and the number of
// values above zero, each gene's cells added in order. `cell` and `value`
// start at gene g0's first value; `code` gives every cell's group from 0, or a
// negative number for none. FALSE for a cell outside the matrix.
template <class O>
bool add_group_stats(const int64_t *gene_ptr, int g0, int g1, const uint32_t *cell, const O *value,
                     const int *code, int n_cells, int n_groups, int n_genes, double *total, double *positive,
                     int threads) {
  bool bad = false;
  const int64_t base = gene_ptr[g0];
#ifdef _OPENMP
#pragma omp parallel for num_threads(threads) schedule(dynamic, 64) reduction(|| : bad)
#endif
  for (int g = g0; g < g1; g++) {
    for (int64_t k = gene_ptr[g] - base; k < gene_ptr[g + 1] - base; k++) {
      uint32_t c = cell[k];
      if (c >= (uint32_t)n_cells) {
        bad = true;
        continue;
      }
      int grp = code[c];
      if (grp < 0 || grp >= n_groups) continue;
      const double v = (double)value[k];
      total[(size_t)grp * n_genes + g] += v;
      if (v > 0) positive[(size_t)grp * n_genes + g] += 1;
    }
  }
  return !bad;
}

// The sidecar of a matrix stored cell by cell: `ptr` (n_cells + 1 offsets)
// over `ind` (each value's gene, from 0) and `x`. It is transposed to gene
// order, checked for genes listed twice, summarised for `groupings` and
// written to `out` with values of type O (float for float sources, which loses
// nothing, else double). `label` names the matrix in errors.
template <class V, class O>
List transpose_to_sidecar(const int64_t *ptr, size_t n_ptr, const int32_t *ind, const V *x, size_t nnz,
                          const std::string &label, const std::string &out, int n_cells, int n_genes, int threads,
                          int level, List groupings, IntegerVector group_sizes) {
  const std::string inconsistent = label + " has inconsistent indptr/indices/data lengths.";
  if (n_ptr != (size_t)n_cells + 1 || ptr[0] != 0 || (size_t)ptr[n_cells] != nnz) stop(inconsistent);
  for (int c = 0; c < n_cells; c++) {
    if (ptr[c + 1] < ptr[c]) stop(inconsistent);
  }
  if (nnz == 0) return List::create(_["written"] = false, _["reason"] = "empty");
  const int T = thread_count(threads);
  int32_t lowest = ind[0], highest = ind[0];
#ifdef _OPENMP
#pragma omp parallel for num_threads(T) reduction(min : lowest) reduction(max : highest)
#endif
  for (size_t k = 0; k < nnz; k++) {
    if (ind[k] < lowest) lowest = ind[k];
    if (ind[k] > highest) highest = ind[k];
  }
  if (lowest < 0 || highest >= n_genes) stop(inconsistent);

  // cells cut into one block per thread with about the same number of values;
  // a gene receives the blocks' cells block after block, so in cell order
  std::vector<int> first(T + 1);
  for (int b = 0, c = 0; b <= T; b++) {
    size_t target = (size_t)((double)nnz * b / T);
    while (c < n_cells && (size_t)ptr[c] < target) c++;
    first[b] = (b == T) ? n_cells : c;
  }
  std::vector<int64_t> next((size_t)T * n_genes, 0);
#ifdef _OPENMP
#pragma omp parallel for num_threads(T)
#endif
  for (int b = 0; b < T; b++) {
    int64_t *count = &next[(size_t)b * n_genes];
    for (int64_t k = ptr[first[b]]; k < ptr[first[b + 1]]; k++) count[ind[k]]++;
  }
  std::vector<int64_t> gene_ptr((size_t)n_genes + 1);
  int64_t total = 0;
  for (int g = 0; g < n_genes; g++) {
    gene_ptr[g] = total;
    for (int b = 0; b < T; b++) {
      int64_t n = next[(size_t)b * n_genes + g];
      next[(size_t)b * n_genes + g] = total;
      total += n;
    }
  }
  gene_ptr[n_genes] = total;
  std::vector<uint32_t> cell(nnz);
  std::vector<O> value(nnz);
#ifdef _OPENMP
#pragma omp parallel for num_threads(T)
#endif
  for (int b = 0; b < T; b++) {
    int64_t *slot = &next[(size_t)b * n_genes];
    for (int c = first[b]; c < first[b + 1]; c++) {
      for (int64_t k = ptr[c]; k < ptr[c + 1]; k++) {
        int64_t d = slot[ind[k]]++;
        cell[d] = (uint32_t)c;
        value[d] = (O)x[k];
      }
    }
  }
  std::vector<int64_t>().swap(next);

  // a gene listed twice for one cell: left to the R reader, which sums them
  bool duplicated = false;
#ifdef _OPENMP
#pragma omp parallel for num_threads(T) reduction(|| : duplicated)
#endif
  for (int g = 0; g < n_genes; g++) {
    for (int64_t k = gene_ptr[g] + 1; k < gene_ptr[g + 1]; k++) {
      if (cell[k] == cell[k - 1]) duplicated = true;
    }
  }
  if (duplicated) return List::create(_["written"] = false, _["reason"] = "duplicated");

  // group summaries while the values are in memory: no second pass over the file
  List stats(groupings.size());
  for (R_xlen_t j = 0; j < groupings.size(); j++) {
    IntegerVector codes = groupings[j];
    const int n_groups = group_sizes[j];
    if (codes.size() != n_cells) stop("Group codes must give one code per cell.");
    NumericMatrix sums(n_genes, n_groups), positive(n_genes, n_groups);
    add_group_stats(gene_ptr.data(), 0, n_genes, cell.data(), value.data(), codes.begin(), n_cells, n_groups, n_genes,
                    sums.begin(), positive.begin(), T);
    stats[j] = List::create(_["total"] = sums, _["positive"] = positive);
  }

  Handle o(H5Fcreate(out.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT), H5Fclose);
  if (o.id < 0) stop("Cannot create `" + out + "`.");
  Handle group(H5Gcreate2(o.id, "expression", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT), H5Gclose);
  hsize_t max_nnz = (hsize_t)n_cells * (hsize_t)n_genes;
  const bool single = sizeof(O) == 4;
  write_all(o.id, "expression/data", single ? H5T_IEEE_F32LE : H5T_IEEE_F64LE,
            single ? H5T_NATIVE_FLOAT : H5T_NATIVE_DOUBLE, value.data(), nnz, max_nnz, DATA_CHUNK, level, level > 0);
  write_all(o.id, "expression/indices", H5T_STD_U32LE, H5T_NATIVE_UINT32, cell.data(), nnz, max_nnz, DATA_CHUNK, level,
            level > 0);
  write_all(o.id, "expression/indptr", H5T_STD_I64LE, H5T_NATIVE_INT64, gene_ptr.data(), (hsize_t)n_genes + 1,
            (hsize_t)n_genes + 1, INDPTR_CHUNK, 0, true);
  return List::create(_["written"] = true, _["reason"] = "", _["group_stats"] = stats);
}

template <class V>
List h5ad_to_sidecar(hid_t in, const std::string &matrix, const std::string &out, int n_cells, int n_genes,
                     int threads, int level, hid_t value_memtype, List groupings, IntegerVector group_sizes) {
  std::vector<int64_t> ptr;
  std::vector<int32_t> ind;
  std::vector<V> x;
  read_all(in, matrix + "/indptr", H5T_NATIVE_INT64, ptr);
  read_all(in, matrix + "/indices", H5T_NATIVE_INT32, ind);
  read_all(in, matrix + "/data", value_memtype, x);
  if (ind.size() != x.size()) stop("`" + matrix + "` has inconsistent indptr/indices/data lengths.");
  return transpose_to_sidecar<V, V>(ptr.data(), ptr.size(), ind.data(), x.data(), x.size(), "`" + matrix + "`", out,
                                 n_cells, n_genes, threads, level, groupings, group_sizes);
}

}  // namespace

// Write the sidecar arrays for the CSR matrix at `matrix` of `h5ad` to a new
// file `out`. `groupings` holds group codes per cell (from 0, negative for
// none) and `group_sizes` their numbers of groups; each gets its summaries in
// `group_stats`, as sidecarGroupStatsCpp() returns them. Returns
// list(written, reason, group_stats); written = FALSE (and no file) for an
// empty matrix or a gene listed twice for one cell.
// [[Rcpp::export]]
List h5adCsrToSidecarCpp(std::string h5ad, std::string matrix, std::string out, int n_cells, int n_genes,
                         int threads, int level, List groupings, IntegerVector group_sizes) {
  if (groupings.size() != group_sizes.size()) stop("Every grouping needs its number of groups.");
  Handle in(H5Fopen(h5ad.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT), H5Fclose);
  if (in.id < 0) stop("Cannot open `" + h5ad + "`.");
  if (is_float32(in.id, matrix + "/data")) {
    return h5ad_to_sidecar<float>(in.id, matrix, out, n_cells, n_genes, threads, level, H5T_NATIVE_FLOAT, groupings,
                                  group_sizes);
  }
  return h5ad_to_sidecar<double>(in.id, matrix, out, n_cells, n_genes, threads, level, H5T_NATIVE_DOUBLE, groupings,
                                 group_sizes);
}

// Write the sidecar arrays of a genes x cells dgCMatrix, given by its slots
// (`i`, `p`, `x`), to a new file `out`: the matrix is already stored cell by
// cell, so it goes through the same transpose without a copy of its values.
// Returns list(written, reason) as h5adCsrToSidecarCpp() does.
// [[Rcpp::export]]
List dgcToSidecarCpp(IntegerVector i, IntegerVector p, NumericVector x, int n_genes, int n_cells, std::string out,
                     int threads, int level) {
  if (i.size() != x.size()) stop("The expression matrix has inconsistent indptr/indices/data lengths.");
  std::vector<int64_t> ptr(p.begin(), p.end());
  return transpose_to_sidecar<double, double>(ptr.data(), ptr.size(), i.begin(), x.begin(), (size_t)x.size(),
                                      "The expression matrix", out, n_cells, n_genes, threads, level, List(),
                                      IntegerVector());
}

// Per gene (sidecar column) and group: the sum of the values and the number of
// values above zero, each gene's cells added in order. `codes` gives every
// cell's group from 0, or a negative number for none. Read in blocks of genes.
// [[Rcpp::export]]
List sidecarGroupStatsCpp(std::string file, std::string group, IntegerVector codes, int n_groups, int threads) {
  Handle h(H5Fopen(file.c_str(), H5F_ACC_RDONLY, H5P_DEFAULT), H5Fclose);
  if (h.id < 0) stop("Cannot open `" + file + "`.");
  std::vector<int64_t> ptr;
  read_all(h.id, group + "/indptr", H5T_NATIVE_INT64, ptr);
  const int n_genes = (int)ptr.size() - 1;
  const int n_cells = codes.size();
  NumericMatrix total(n_genes, n_groups), positive(n_genes, n_groups);
  const int T = thread_count(threads);
  // cell numbers read at the 4 bytes the sidecar stores them in
  std::vector<uint32_t> cell;
  std::vector<double> value;
  int g0 = 0;
  while (g0 < n_genes) {
    int g1 = g0 + 1;
    while (g1 < n_genes && (size_t)(ptr[g1 + 1] - ptr[g0]) <= GROUP_STATS_BLOCK) g1++;
    hsize_t start = (hsize_t)ptr[g0], count = (hsize_t)(ptr[g1] - ptr[g0]);
    cell.resize(count);
    value.resize(count);
    read_range(h.id, group + "/indices", H5T_NATIVE_UINT32, start, count, cell.data());
    read_range(h.id, group + "/data", H5T_NATIVE_DOUBLE, start, count, value.data());
    if (!add_group_stats(ptr.data(), g0, g1, cell.data(), value.data(), codes.begin(), n_cells, n_groups, n_genes,
                         total.begin(), positive.begin(), T)) {
      stop("`" + file + "` lists a cell outside the matrix.");
    }
    g0 = g1;
  }
  return List::create(_["total"] = total, _["positive"] = positive);
}
