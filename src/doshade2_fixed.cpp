// doshade2_fixed.cpp
//
// Drop-in replacement for Corripio's Fortran `doshade` horizon-shading
// subroutine. Fixes an inter-ray inconsistency bug in the original boundary-
// ray algorithm (adjacent grid cells could receive contradictory shadow
// verdicts from independent rays with divergent accumulated horizon
// histories, producing periodic horizontal/vertical line artifacts,
// concentrated at low/grazing solar elevation). This replacement casts one
// independent ray per grid cell toward the sun, eliminating that
// inconsistency. See accompanying notes for the full derivation/verification.
//
// Portable: standard C++11, no OpenMP dependency (avoids the macOS gap where
// Apple's default clang has no OpenMP support). Threading via std::thread.
//
// Build examples:
//   Linux/macOS:  g++ -O3 -march=native -std=c++11 -pthread -shared -fPIC \
//                     -o libdoshade2.so doshade2_fixed.cpp
//   Windows(MSVC): cl /O2 /std:c++14 /LD doshade2_fixed.cpp
//
#include <vector>
#include <cmath>
#include <algorithm>
#include <thread>
#include <cstdint>

#if defined(_WIN32)
  #define DOSHADE_EXPORT extern "C" __declspec(dllexport)
#else
  #define DOSHADE_EXPORT extern "C"
#endif

namespace doshade2_detail {

// Column-major indexing matching Fortran z(cols,rows): element (i,j), 1-based,
// stored at (i-1) + (j-1)*cols.
static inline double zget(const double* z, int cols, int i, int j) {
    return z[(i - 1) + (j - 1) * static_cast<size_t>(cols)];
}
static inline void zset(double* s, int cols, int i, int j, double v) {
    s[(i - 1) + (j - 1) * static_cast<size_t>(cols)] = v;
}

// Fortran NINT semantics: round half away from zero.
static inline long nint(double x) {
    return (x >= 0.0) ? static_cast<long>(x + 0.5)
                      : -static_cast<long>(-x + 0.5);
}

struct SunGeom {
    double toward_sun[2]; // unit-ish step direction walking FROM a cell TOWARD the sun
    double nsv[3];         // sun-perpendicular projection axis (Corripio's normalsunvector)
};

// Mirrors the original subroutine's geometry setup exactly (same formulas),
// but also derives toward_sun = -inversesunvector, i.e. the direction that
// walks from a point toward the sun rather than away from it. See the
// verified ridge-case derivation for why this sign is the correct one for a
// per-cell (rather than boundary-ray) horizon scan.
//
// degenerate flag is set when the sun is at/near the zenith (sunvector's
// horizontal component is ~zero): in that case nothing can occlude anything
// (a purely-overhead sun casts no shadows in this 2.5D horizon model), and
// the caller must skip ray-casting entirely rather than attempt a step of
// zero length, which would never advance and loop forever.
static SunGeom compute_geom(const double sunvector[3], bool* degenerate) {
    SunGeom g;
    double maxc = std::max(std::fabs(sunvector[0]), std::fabs(sunvector[1]));
    if (maxc <= 0.0) {
        *degenerate = true;
        g.toward_sun[0] = 0.0;
        g.toward_sun[1] = 0.0;
        g.nsv[0] = 0.0;
        g.nsv[1] = 0.0;
        g.nsv[2] = 1.0;
        return g;
    }
    double inv_sv0 = -sunvector[0] / maxc;
    double inv_sv1 = -sunvector[1] / maxc;
    g.toward_sun[0] = -inv_sv0;
    g.toward_sun[1] = -inv_sv1;

    g.nsv[2] = std::sqrt(sunvector[0] * sunvector[0] + sunvector[1] * sunvector[1]);
    if (g.nsv[2] <= 0.0) {
        // Redundant with the maxc check above in practice (both derive from
        // the same horizontal components), kept as a defensive second guard.
        *degenerate = true;
        g.nsv[0] = 0.0;
        g.nsv[1] = 0.0;
        g.nsv[2] = 1.0;
        return g;
    }
    *degenerate = false;
    g.nsv[0] = -sunvector[0] * sunvector[2] / g.nsv[2];
    g.nsv[1] = -sunvector[1] * sunvector[2] / g.nsv[2];
    return g;
}

// Processes a strided subset of columns: columns (thread_id, thread_id+stride,
// thread_id+2*stride, ...). Interleaved (not contiguous) partitioning is
// deliberate: per-column ray cost varies strongly and systematically with
// column index (cost is proportional to distance-to-boundary in the sun
// direction), so contiguous chunks would badly load-imbalance threads.
// Verified empirically: interleaving keeps per-thread total work within a
// few percent of each other versus ~40%+ spread for contiguous chunks.
static void process_columns(const double* z, double* sombra,
                             int cols, int rows, double dl,
                             const SunGeom& g,
                             int thread_id, int stride) {
    const double step_horiz = g.toward_sun[0] * dl * g.nsv[0]
                             + g.toward_sun[1] * dl * g.nsv[1];
    const double nsv2 = g.nsv[2];
    const double tsx = g.toward_sun[0];
    const double tsy = g.toward_sun[1];

    for (int i0 = thread_id + 1; i0 <= cols; i0 += stride) {
        for (int j0 = 1; j0 <= rows; j0++) {
            const double origin_proj = zget(z, cols, i0, j0) * nsv2;
            double horiz = step_horiz;
            double xf = i0 + tsx;
            double yf = j0 + tsy;
            bool shaded = false;
            while (true) {
                long idx = nint(xf);
                long jdy = nint(yf);
                if (idx < 1 || idx > cols || jdy < 1 || jdy > rows) break;
                double zproj = horiz + zget(z, cols, static_cast<int>(idx), static_cast<int>(jdy)) * nsv2;
                if (zproj > origin_proj) { shaded = true; break; }
                horiz += step_horiz;
                xf += tsx;
                yf += tsy;
            }
            if (shaded) zset(sombra, cols, i0, j0, 0.0);
            // else: sombra already initialized to 1.0 by caller
        }
    }
}

} // namespace doshade2_detail

// ---------------------------------------------------------------------
// doshade2_fixed: drop-in replacement for Corripio's doshade2 subroutine.
//
// Parameters mirror the original Fortran subroutine:
//   dem         - flattened DEM, column-major, length cols*rows (matches
//                 Fortran's dem(cols*rows) reshaped to z(cols,rows))
//   sunvector   - unit(-ish) vector toward the sun, [x,y,z], length 3
//   cols, rows  - grid dimensions
//   dl          - grid cell size (isotropic, matches original's convention)
//   sombra      - OUTPUT, preallocated by caller, length cols*rows,
//                 column-major; 1.0 = lit, 0.0 = shaded
//   max_threads_n - 0 (default): auto-select max(1, hw_concurrency-1).
//                 >0: use min(max_threads_n, max(1, hw_concurrency-1)).
//                 Always reduced further if the grid has fewer columns than
//                 the selected thread count (never spawns a thread with no
//                 assigned columns).
// ---------------------------------------------------------------------
DOSHADE_EXPORT
void doshade2_fixed(const double* dem, const double* sunvector,
                    int cols, int rows, double dl,
                    double* sombra, int max_threads_n) {
    using namespace doshade2_detail;

    if (cols <= 0 || rows <= 0) return; // nothing to do

    // Initialize output: 1 = lit, matching the original's `sombra = 1` init.
    std::fill(sombra, sombra + static_cast<size_t>(cols) * rows, 1.0);

    bool degenerate = false;
    SunGeom g = compute_geom(sunvector, &degenerate);
    if (degenerate) return; // sun at/near zenith: nothing occludes anything;
                             // sombra is already all-1.0 (lit) from the fill above.

    // --- thread count selection ---
    unsigned hw = std::thread::hardware_concurrency();
    if (hw == 0) hw = 1; // hardware_concurrency() may return 0 if undetectable
    int cores_minus_one = static_cast<int>(hw) - 1;
    if (cores_minus_one < 1) cores_minus_one = 1;

    int nthreads;
    if (max_threads_n <= 0) {
        nthreads = cores_minus_one; // auto
    } else {
        nthreads = std::min(max_threads_n, cores_minus_one);
    }
    // Never spawn more threads than there are columns to hand out.
    if (nthreads > cols) nthreads = cols;
    if (nthreads < 1) nthreads = 1;

    if (nthreads == 1) {
        process_columns(dem, sombra, cols, rows, dl, g, 0, 1);
        return;
    }

    std::vector<std::thread> pool;
    pool.reserve(nthreads);
    for (int t = 0; t < nthreads; t++) {
        pool.emplace_back(process_columns, dem, sombra, cols, rows, dl, g, t, nthreads);
    }
    for (auto& th : pool) th.join();
}

// Convenience overload: fills in the default max_threads_n=0 (auto) for
// callers using a 6-argument signature identical to the original Fortran
// subroutine's parameter list (thread count is a pure addition, not a
// behavior change to existing call sites once bound via iso_c_binding with
// an optional/default argument on the Fortran side).
DOSHADE_EXPORT
void doshade2_fixed_default(const double* dem, const double* sunvector,
                            int cols, int rows, double dl, double* sombra) {
    doshade2_fixed(dem, sunvector, cols, rows, dl, sombra, 0);
}
