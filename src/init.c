#include <R.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rinternals.h>

extern SEXP C_cerebro_read_pinned_secret(SEXP);

static const R_CallMethodDef call_methods[] = {
  {"C_cerebro_read_pinned_secret",
   (DL_FUNC)&C_cerebro_read_pinned_secret,
   1},
  {NULL, NULL, 0}
};

void attribute_visible R_init_CerebroNexus(DllInfo *dll) {
  R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
