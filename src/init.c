#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(doshade)(void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"doshade", (DL_FUNC) &F77_NAME(doshade), 6},
    {NULL, NULL, 0}
};


/* .Call calls */
extern SEXP R_doshade2_fixed(SEXP dem_sexp, SEXP sunvector_sexp,
                             SEXP cols_sexp, SEXP rows_sexp,
                             SEXP dl_sexp, SEXP max_threads_n_sexp);

static const R_CallMethodDef CallEntries[] = {
    {"R_doshade2_fixed", (DL_FUNC) &R_doshade2_fixed, 6},
    {NULL, NULL, 0}
};

void R_init_insol2(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
