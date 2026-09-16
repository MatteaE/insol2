// doshade2_R.cpp
//
// R .Call()-compatible wrapper around doshade2_fixed() (defined in
// doshade2_fixed.cpp).
// This file owns all R <-> C++ marshalling: input validation, SEXP
// unwrapping, output allocation, and the CRAN check-farm thread-limit
// safety net. It does not duplicate or alter the shading algorithm itself.
//
// Exposed to R as a single .Call() entry point: R_doshade2_fixed().
// See R/doshade2.R for the user-facing R function that calls this.

#include <R.h>
#include <Rinternals.h>
#include <cstring>
#include <cstdlib>

// Declaration of the verified C++ core (defined in doshade2_fixed.cpp,
// compiled and linked as part of the same package).
extern "C" void doshade2_fixed(const double* dem, const double* sunvector,
                               int cols, int rows, double dl,
                               double* sombra, int max_threads_n);

namespace {

// CRAN Repository Policy: "If running a package uses multiple threads/cores
// it must never use more than two simultaneously" during checks, since the
// check farm is a shared, heavily loaded resource. The standard convention
// (used by RcppParallel, secr, VIM, eulerr, and others) is to detect the
// R_R_CHECK_LIMIT_CORES_ environment variable that R itself sets during
// `R CMD check` and throttle accordingly there, while leaving normal
// interactive/production use (this variable is unset in an ordinary R
// session) free to use the full auto-selected thread count.
//
// This does not change max_threads_n's documented semantics for ordinary
// use -- it only clamps the effective ceiling when R CMD check is detected,
// so a package that is never checked (or is checked outside this variable
// being set) behaves exactly as specified.
int cran_check_thread_cap() {
    const char* limit_cores = std::getenv("_R_CHECK_LIMIT_CORES_");
    if (limit_cores != nullptr && limit_cores[0] != '\0') {
        // Any non-empty value (TRUE, warn, or otherwise) signals the check
        // farm's restriction is active; CRAN policy allows at most 2.
        return 2;
    }
    return 0; // no cap beyond what doshade2_fixed already applies
}

} // namespace

extern "C" SEXP R_doshade2_fixed(SEXP dem_sexp, SEXP sunvector_sexp,
                                 SEXP cols_sexp, SEXP rows_sexp,
                                 SEXP dl_sexp, SEXP max_threads_n_sexp) {
    // --- Type/shape validation. Fail loudly and clearly rather than let
    // malformed input reach the C++ core, which trusts its inputs and does
    // no bounds-checking of its own on dem/sunvector contents. ---

    if (TYPEOF(dem_sexp) != REALSXP)
        Rf_error("doshade2: 'dem' must be a numeric (double) vector");
    if (TYPEOF(sunvector_sexp) != REALSXP || Rf_length(sunvector_sexp) != 3)
        Rf_error("doshade2: 'sunvector' must be a numeric vector of length 3");

    int cols = Rf_asInteger(cols_sexp);
    int rows = Rf_asInteger(rows_sexp);
    double dl = Rf_asReal(dl_sexp);
    int max_threads_n = Rf_asInteger(max_threads_n_sexp);

    if (cols == NA_INTEGER || rows == NA_INTEGER)
        Rf_error("doshade2: internal error -- 'cols'/'rows' must not be NA");
    if (cols <= 0 || rows <= 0)
        Rf_error("doshade2: internal error -- 'cols' and 'rows' must be positive (got cols=%d, rows=%d)", cols, rows);

    R_xlen_t expected_len = static_cast<R_xlen_t>(cols) * static_cast<R_xlen_t>(rows);
    if (Rf_xlength(dem_sexp) != expected_len)
        Rf_error("doshade2: internal error -- length(dem) = %lld does not match cols*rows = %lld",
                 (long long)Rf_xlength(dem_sexp), (long long)expected_len);

    if (!R_FINITE(dl) || dl <= 0.0)
        Rf_error("doshade2: 'dl' must be a finite positive number");

    const double* dem = REAL(dem_sexp);
    const double* sunvector = REAL(sunvector_sexp);

    // Note: unlike an early draft of this wrapper, we do NOT error on
    // non-finite dem values here. The package's established convention
    // (see R/doshade2.R) is to substitute NA -> -999 before this call and
    // -999 -> NA on the way back out, matching the original Fortran
    // routine's behavior exactly. -999 is a perfectly finite double, so it
    // passes through the arithmetic in doshade2_fixed() same as any other
    // elevation value would. We only reject actual infinities here (which
    // should never arise given the sentinel substitution upstream, but this
    // is a public entry point so we check defensively).
    for (R_xlen_t k = 0; k < expected_len; k++) {
        if (std::isinf(dem[k])) {
            Rf_error("doshade2: 'dem' contains an infinite value at index %lld (1-based); "
                     "infinite elevations are not physically meaningful", (long long)(k + 1));
        }
    }
    for (int k = 0; k < 3; k++) {
        if (!R_FINITE(sunvector[k]))
            Rf_error("doshade2: 'sunvector' contains NA/NaN/Inf");
    }

    if (max_threads_n == NA_INTEGER || max_threads_n < 0) max_threads_n = 0; // treat as "auto"

    int check_cap = cran_check_thread_cap();
    if (check_cap > 0) {
        // Under R CMD check: honor CRAN's 2-thread ceiling regardless of
        // what the caller/default requested.
        if (max_threads_n == 0 || max_threads_n > check_cap) {
            max_threads_n = check_cap;
        }
    }

    SEXP sombra_sexp = PROTECT(Rf_allocVector(REALSXP, expected_len));
    double* sombra = REAL(sombra_sexp);

    doshade2_fixed(dem, sunvector, cols, rows, dl, sombra, max_threads_n);

    UNPROTECT(1);
    return sombra_sexp;
}
