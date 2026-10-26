## Layout of one dataset in an H5 sidecar's `expression` group: extent,
## chunks, number of filters and value type, which the Viewer's hyperslab
## reads depend on. Shared by the sidecar writer and h5ad tests.
h5_writer_layout <- function(file, dataset) {
  handle <- rhdf5::H5Fopen(file, flags = "H5F_ACC_RDONLY")
  on.exit(rhdf5::H5Fclose(handle), add = TRUE)
  dset <- rhdf5::H5Dopen(handle, paste0("expression/", dataset))
  on.exit(rhdf5::H5Dclose(dset), add = TRUE)
  space <- rhdf5::H5Dget_space(dset)
  on.exit(rhdf5::H5Sclose(space), add = TRUE)
  plist <- rhdf5::H5Dget_create_plist(dset)
  on.exit(rhdf5::H5Pclose(plist), add = TRUE)
  type <- rhdf5::H5Dget_type(dset)
  extent <- rhdf5::H5Sget_simple_extent_dims(space)
  list(
    dim = extent$size,
    maxdim = extent$maxsize,
    chunk = rhdf5::H5Pget_chunk(plist),
    n_filters = rhdf5::H5Pget_nfilters(plist),
    type_class = rhdf5::H5Tget_class(type),
    type_size = rhdf5::H5Tget_size(type),
    type_precision = rhdf5::H5Tget_precision(type)
  )
}
