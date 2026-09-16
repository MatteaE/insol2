#' Compute cast-shadow grid from a DEM and sun position
#'
#' Corrected, multi-threaded replacement for the package's original Fortran
#' \code{doshade} routine. Casts an independent ray from every grid cell
#' toward the sun (rather than only from grid-boundary cells, as the
#' original algorithm did), which fixes a bug where adjacent cells could
#' receive inconsistent shadow verdicts -- visible as periodic
#' horizontal/vertical line artifacts, most noticeable at low/grazing solar
#' elevation.
#'
#' This function is a drop-in replacement for the original: the call
#' signature, argument order, \code{SpatRaster} support, and \code{NA}
#' handling are unchanged. The only addition is the optional
#' \code{max_threads_n} argument.
#'
#' @param dem A DEM, either a numeric matrix (\code{rows} x \code{cols}, R's
#'   usual matrix orientation) or a \code{terra::SpatRaster}.
#' @param sv Numeric vector of length 3: unit(-ish) vector pointing toward
#'   the sun, \code{c(x, y, z)}.
#' @param dl Grid cell size. Required if \code{dem} is a plain matrix;
#'   ignored (taken from the raster's resolution instead) if \code{dem} is a
#'   \code{SpatRaster}.
#' @param sombra Unused; retained only for signature compatibility with the
#'   original function.
#' @param max_threads_n Integer. Maximum number of threads to use for the
#'   computation. \code{0} (the default) auto-selects a sensible number
#'   (logical cores minus one, at least 1). A positive value requests that
#'   many threads, capped at (logical cores minus one). The thread count is
#'   further capped automatically if the grid has fewer columns than the
#'   requested/selected count, and is capped to 2 when running under
#'   \code{R CMD check} regardless of this argument, per CRAN policy.
#'
#' @return If \code{dem} was a matrix: a numeric matrix of the same shape,
#'   \code{1} = lit, \code{0} = shaded. Grid cells where the input \code{dem}
#'   was \code{NA} are \code{NA} in the output (the original routine
#'   computed a shading value for such cells using an internal -999
#'   sentinel elevation and never actually restored \code{NA} in its output;
#'   this version replaces the output with \code{NA} at those positions
#'   instead, since callers are expected to pre-filter \code{NA} from their
#'   DEM in practice and this makes the rare case unambiguous rather than
#'   silently wrong). If \code{dem} was a \code{SpatRaster}: a
#'   \code{SpatRaster} with the same CRS and extent.
#'
#' @examples
#' \dontrun{
#' dem <- matrix(rnorm(200 * 200, mean = 2000, sd = 200), nrow = 200, ncol = 200)
#' sunvector <- c(0.55, 0.05, 0.12)
#' shade <- doshade2(dem, sunvector, dl = 30)
#'
#' # SpatRaster input, using 4 threads:
#' # shade_r <- doshade2(my_spatraster, sunvector, max_threads_n = 4)
#' }
#'
#' @useDynLib insol2, .registration = TRUE
#' @export
doshade2 <- function(dem, sv, dl = 0, sombra = dem, max_threads_n = 0L) {
    if (nargs() < 2) {
        cat("USAGE: doshade2(dem,sunvector,dl) \n")
        return(invisible(NULL))
    }

    switchdem <- 0
    if ("SpatRaster" %in% class(dem)) {
        switchdem <- 1
        dproj <- terra::crs(dem)
        dext <- terra::ext(dem)
        dl <- terra::res(dem)[1]
        dem <- terra::as.matrix(dem)
    }

    cols <- ncol(dem)
    rows <- nrow(dem)

    if (dl == 0) {
        cat("Input data is not a SpatRaster, then I need the DEM resolution dl \n")
        return(invisible(NULL))
    }

    # Same NA -> sentinel convention as the original routine: the compiled
    # core does plain double-precision arithmetic with no NA concept, so we
    # replace NA elevations with -999 (an arbitrary, implausibly-low
    # sentinel) before the call, run the shading computation as normal, and
    # then overwrite the OUTPUT with NA at those same grid positions
    # afterward (see `sombra[na_mask] <- NA` below). na_mask is captured now,
    # from dem in its original (pre-sentinel, pre-transpose) orientation, so
    # it can be applied directly to the returned sombra matrix at the end
    # without any extra reshaping bookkeeping.
    na_mask <- is.na(dem)
    dem[na_mask] <- -999

    # Match the original's exact flattening convention: t(dem) is cols x
    # rows (dem itself is R's usual rows x cols), flattened column-major --
    # i.e. this vector is ordered exactly as Fortran's z(cols,rows) expected,
    # and as doshade2_fixed()'s column-major indexing still expects.
    dem_flat <- as.double(t(dem))

    sombra_flat <- .Call(
        "R_doshade2_fixed",
        dem_flat,
        as.double(sv),
        as.integer(cols),
        as.integer(rows),
        as.double(dl),
        as.integer(max_threads_n),
        PACKAGE = "insol2"
    )

    sombra <- t(matrix(sombra_flat, nrow = cols))
    sombra[na_mask] <- NA  # apply while still a plain matrix -- avoids any
                            # ambiguity around bracket-assignment semantics
                            # on a SpatRaster object

    if (switchdem) {
        sombra <- terra::rast(sombra, crs = dproj)
        terra::ext(sombra) <- dext
    }

    return(sombra)
}
