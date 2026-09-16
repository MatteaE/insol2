#' Compute cast-shadow grid from a DEM and sun position
#'
## Corrected, multi-threaded replacement for the package's original Fortran
## doshade routine. Casts an independent ray from every grid cell toward
## the sun (rather than only from grid-boundary cells, as the original
## algorithm did), which fixes a bug where adjacent cells could receive
## inconsistent shadow verdicts -- visible as periodic horizontal/vertical
## line artifacts, most noticeable at low/grazing solar elevation.
##
## Drop-in replacement for the original: call signature, argument order,
## SpatRaster support, and NA handling are unchanged. The only addition is
## the optional max_threads_n argument. See man/doshade.Rd for full
## user-facing documentation.
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
