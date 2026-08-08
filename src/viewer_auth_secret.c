#include <R.h>
#include <Rinternals.h>

#include <stdint.h>
#include <string.h>

#define CEREBRO_SECRET_LIMIT 107

static void cerebro_secret_zero(unsigned char *bytes, size_t length) {
  volatile unsigned char *cursor = bytes;
  while (length-- > 0) {
    *cursor++ = 0;
  }
}

static SEXP cerebro_secret_result(
    const unsigned char *bytes,
    size_t length,
    double device_id,
    double inode,
    double size,
    int mode,
    double uid) {
  const char *names[] = {
    "raw", "device_id", "inode", "size", "mode", "uid", ""
  };
  SEXP result = PROTECT(Rf_mkNamed(VECSXP, names));
  SEXP raw = PROTECT(Rf_allocVector(RAWSXP, (R_xlen_t)length));
  memcpy(RAW(raw), bytes, length);
  SET_VECTOR_ELT(result, 0, raw);
  SET_VECTOR_ELT(result, 1, Rf_ScalarReal(device_id));
  SET_VECTOR_ELT(result, 2, Rf_ScalarReal(inode));
  SET_VECTOR_ELT(result, 3, Rf_ScalarReal(size));
  SET_VECTOR_ELT(result, 4, Rf_ScalarInteger(mode));
  SET_VECTOR_ELT(result, 5, Rf_ScalarReal(uid));
  UNPROTECT(2);
  return result;
}

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static int cerebro_secret_same_handle(
    const BY_HANDLE_FILE_INFORMATION *left,
    const BY_HANDLE_FILE_INFORMATION *right) {
  return left->dwVolumeSerialNumber == right->dwVolumeSerialNumber &&
    left->nFileIndexHigh == right->nFileIndexHigh &&
    left->nFileIndexLow == right->nFileIndexLow &&
    left->nFileSizeHigh == right->nFileSizeHigh &&
    left->nFileSizeLow == right->nFileSizeLow;
}

static int cerebro_secret_read(
    HANDLE handle,
    unsigned char *bytes,
    DWORD *length) {
  LARGE_INTEGER start;
  start.QuadPart = 0;
  if (!SetFilePointerEx(handle, start, NULL, FILE_BEGIN)) {
    return 0;
  }
  return ReadFile(handle, bytes, CEREBRO_SECRET_LIMIT, length, NULL) != 0;
}

SEXP C_cerebro_read_pinned_secret(SEXP path_sexp) {
  HANDLE handle = INVALID_HANDLE_VALUE;
  HANDLE current = INVALID_HANDLE_VALUE;
  unsigned char first[CEREBRO_SECRET_LIMIT] = {0};
  unsigned char second[CEREBRO_SECRET_LIMIT] = {0};
  DWORD first_length = 0;
  DWORD second_length = 0;
  BY_HANDLE_FILE_INFORMATION opened_info;
  BY_HANDLE_FILE_INFORMATION current_info;
  wchar_t *wide_path = NULL;
  SEXP result = R_NilValue;

  if (
    TYPEOF(path_sexp) != STRSXP || XLENGTH(path_sexp) != 1 ||
      STRING_ELT(path_sexp, 0) == NA_STRING
  ) {
    return R_NilValue;
  }
  const char *path = Rf_translateCharUTF8(STRING_ELT(path_sexp, 0));
  int wide_length = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path,
                                        -1, NULL, 0);
  if (wide_length <= 0) {
    return R_NilValue;
  }
  wide_path = (wchar_t *)R_alloc((size_t)wide_length, sizeof(wchar_t));
  if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1,
                          wide_path, wide_length) <= 0) {
    return R_NilValue;
  }

  handle = CreateFileW(
    wide_path,
    GENERIC_READ,
    FILE_SHARE_READ,
    NULL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OPEN_REPARSE_POINT,
    NULL
  );
  if (handle == INVALID_HANDLE_VALUE ||
      !GetFileInformationByHandle(handle, &opened_info) ||
      (opened_info.dwFileAttributes &
       (FILE_ATTRIBUTE_DIRECTORY | FILE_ATTRIBUTE_REPARSE_POINT)) != 0 ||
      !cerebro_secret_read(handle, first, &first_length) ||
      !cerebro_secret_read(handle, second, &second_length) ||
      first_length != second_length ||
      memcmp(first, second, first_length) != 0) {
    goto cleanup;
  }

  current = CreateFileW(
    wide_path,
    GENERIC_READ,
    FILE_SHARE_READ,
    NULL,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL | FILE_FLAG_OPEN_REPARSE_POINT,
    NULL
  );
  if (current == INVALID_HANDLE_VALUE ||
      !GetFileInformationByHandle(current, &current_info) ||
      !cerebro_secret_same_handle(&opened_info, &current_info)) {
    goto cleanup;
  }
  if (!CloseHandle(current)) {
    goto cleanup;
  }
  current = INVALID_HANDLE_VALUE;
  if (!CloseHandle(handle)) {
    goto cleanup;
  }
  handle = INVALID_HANDLE_VALUE;

  result = cerebro_secret_result(
    first,
    (size_t)first_length,
    (double)opened_info.dwVolumeSerialNumber,
    (double)(((uint64_t)opened_info.nFileIndexHigh << 32) |
             opened_info.nFileIndexLow),
    (double)(((uint64_t)opened_info.nFileSizeHigh << 32) |
             opened_info.nFileSizeLow),
    0,
    0
  );

cleanup:
  if (current != INVALID_HANDLE_VALUE) {
    CloseHandle(current);
  }
  if (handle != INVALID_HANDLE_VALUE) {
    CloseHandle(handle);
  }
  cerebro_secret_zero(first, sizeof(first));
  cerebro_secret_zero(second, sizeof(second));
  return result;
}

#else

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

static int cerebro_secret_same_stat(
    const struct stat *left,
    const struct stat *right) {
  return left->st_dev == right->st_dev &&
    left->st_ino == right->st_ino &&
    left->st_size == right->st_size &&
    left->st_mode == right->st_mode &&
    left->st_uid == right->st_uid;
}

static int cerebro_secret_read(
    int descriptor,
    unsigned char *bytes,
    size_t *length) {
  size_t used = 0;
  if (lseek(descriptor, 0, SEEK_SET) == (off_t)-1) {
    return 0;
  }
  while (used < CEREBRO_SECRET_LIMIT) {
    ssize_t count = read(
      descriptor,
      bytes + used,
      CEREBRO_SECRET_LIMIT - used
    );
    if (count == 0) {
      break;
    }
    if (count < 0) {
      if (errno == EINTR) {
        continue;
      }
      return 0;
    }
    used += (size_t)count;
  }
  *length = used;
  return 1;
}

SEXP C_cerebro_read_pinned_secret(SEXP path_sexp) {
  int descriptor = -1;
  unsigned char first[CEREBRO_SECRET_LIMIT] = {0};
  unsigned char second[CEREBRO_SECRET_LIMIT] = {0};
  size_t first_length = 0;
  size_t second_length = 0;
  struct stat opened;
  struct stat confirmed;
  struct stat current;
  SEXP result = R_NilValue;

  if (
    TYPEOF(path_sexp) != STRSXP || XLENGTH(path_sexp) != 1 ||
      STRING_ELT(path_sexp, 0) == NA_STRING
  ) {
    return R_NilValue;
  }
#ifndef O_NOFOLLOW
  return R_NilValue;
#else
  const char *path = Rf_translateCharUTF8(STRING_ELT(path_sexp, 0));
  int flags = O_RDONLY | O_NOFOLLOW | O_NONBLOCK;
#ifdef O_CLOEXEC
  flags |= O_CLOEXEC;
#endif
  descriptor = open(path, flags);
  if (descriptor < 0) {
    goto cleanup;
  }
#ifndef O_CLOEXEC
  if (fcntl(descriptor, F_SETFD, FD_CLOEXEC) == -1) {
    goto cleanup;
  }
#endif
  if (fstat(descriptor, &opened) != 0 || !S_ISREG(opened.st_mode) ||
      (opened.st_mode & 07777) != 0600 || opened.st_uid != geteuid() ||
      !cerebro_secret_read(descriptor, first, &first_length) ||
      !cerebro_secret_read(descriptor, second, &second_length) ||
      first_length != second_length ||
      memcmp(first, second, first_length) != 0 ||
      fstat(descriptor, &confirmed) != 0 ||
      !cerebro_secret_same_stat(&opened, &confirmed) ||
      lstat(path, &current) != 0 || !S_ISREG(current.st_mode) ||
      !cerebro_secret_same_stat(&opened, &current)) {
    goto cleanup;
  }
  if (close(descriptor) != 0) {
    descriptor = -1;
    goto cleanup;
  }
  descriptor = -1;
  result = cerebro_secret_result(
    first,
    first_length,
    (double)opened.st_dev,
    (double)opened.st_ino,
    (double)opened.st_size,
    (int)(opened.st_mode & 07777),
    (double)opened.st_uid
  );

cleanup:
  if (descriptor >= 0) {
    close(descriptor);
  }
  cerebro_secret_zero(first, sizeof(first));
  cerebro_secret_zero(second, sizeof(second));
  return result;
#endif
}

#endif
