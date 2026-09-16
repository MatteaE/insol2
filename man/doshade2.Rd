\name{doshade2}
\alias{doshade2}
\title{ Cast shadows }
\description{
Calculates cast shadows over  matrix or SpatRaster DEM for a given illumination direction.
}
\usage{
doshade2(dem, sv, dl = 0, sombra = dem, max_threads_n = 0L)
}

\arguments{
  \item{dem}{
Digital elevation model, a matrix or SpatRaster representing terrain elevation on a regular grid.
}
  \item{sv}{
Unit vector in the direction of the sun.
}
  \item{dl}{
Grid spacing. Not needed if dem is a SpatRaster.
}
  \item{sombra}{
Returned matrix or SpatRaster, no input needed for this argument.
}
  \item{max_threads_n}{
Maximum number of threads to use for the computation. The default, 0,
auto-selects a sensible number of threads (logical cores minus one, at
least 1). A positive value requests that many threads, capped at (logical
cores minus one). The thread count is further capped automatically if the
grid has fewer columns than the requested or auto-selected count, and is
capped to 2 threads when running under \code{R CMD check}, per CRAN policy,
regardless of this argument.
}
}
\details{
\code{doshade2} scans the DEM casting one ray per grid cell in the direction
of the sun, and compares the projection of grid cells on a plane
perpendicular to the sun to determine whether they are in the sun or in the
shadow of a previous cell along that ray. See Figure 6 of reference for
more details.

This implementation supersedes an earlier version that scanned the DEM in
lines parallel to the sun direction starting only from the grid boundary
cells; that approach could, at low or grazing solar elevation, give
neighbouring grid cells inconsistent shadow verdicts, visible as periodic
horizontal or vertical line artifacts in the output. Casting a ray from
every cell removes that inconsistency. The computation is multi-threaded
(see \code{max_threads_n}) to offset the added cost of the per-cell
approach.
}
\value{
Return an object of the same class the the input dem (either a matrix o a SpatRaster),
with values 0 for shaded or 1 for not shaded. Grid cells where \code{dem}
is \code{NA} are \code{NA} in the returned object.
}
\references{
Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters from DEMs
and the position of the sun for solar radiation modelling in mountainous terrain, \emph{International
Journal of Geographical Information Science} 17(1), 1-23.
}
\author{
Javier G. Corripio
}

\examples{

# define the sun vector: northwest at 15 degrees elevation
sv = normalvector(75,315)

## create a pyramid 100 units by side and 50 nunits tall
m = matrix(0,nrow=100,ncol=100)
for (i in 1:100){ for (j in 1:100){
m[i,j] = 50-max(abs(i-50),abs(j-50)) }}

## place it on a large flat expanse
mm = matrix(0,nrow=500,ncol=500)
mm[201:300,201:300] = m

## calulate and plot the cast shadows from the sun 
sh = doshade2(mm,sv,1)
image(t(sh[nrow(sh):1,]),col=grey(1:100/100))
contour(mm,add=TRUE,col='sienna1',nlevels=25)
## (mm is symmetrical, no need to rotate as for shadows)

\dontrun{
## plot cast shadows on mountain terrain, sun at NW, 25 degrees elevation
zipfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.asc.zip",zipfile)
header = read.table(unz(zipfile,'dempyrenees.asc'),nrows=6)
dem = read.table(unz(zipfile,'dempyrenees.asc'),skip=6)
dem = as.matrix(dem)
unlink(zipfile)
cellsize=header[5,2]
sv = normalvector(65,315)
sh = doshade2(dem,sv,cellsize)
image(t(sh[nrow(sh):1,]),col=grey(1:100/100))

## add intensity of illumination in this case sun at NW 45 degrees elevation
sv = normalvector(45,315)
grd = cgrad(dem,cellsize)
hsh = grd[,,1]*sv[1]+grd[,,2]*sv[2]+grd[,,3]*sv[3]
## remove negative incidence angles (self shading) 
hsh = (hsh+abs(hsh))/2
sh = doshade2(dem,sv,cellsize)
hshsh = hsh*sh
image(t(hshsh[nrow(sh):1,]),col=grey(1:100/100))


## plot cast shadows on mountain terrain using terra
sv = normalvector(65,315)
require(terra)
demfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.tif",demfile)
dem = rast(demfile)
sh = doshade2(dem,sv)
plot(sh,col=grey(0:1),legend=FALSE)
contour(dem,add=TRUE,col='sienna1',lwd=.5,nlevels=50)

## add intensity of illumination in this case sun at NW 45 degrees elevation
sv = normalvector(45,315)
grd = cgrad(dem)
hsh = grd[,,1]*sv[1]+grd[,,2]*sv[2]+grd[,,3]*sv[3]
## remove negative incidence angles (self shading) 
hsh = (hsh+abs(hsh))/2
## convert back to raster as dem and add shadows
hsh = rast(hsh,crs=crs(dem))
ext(hsh) = ext(dem)
sh = doshade2(dem,sv)
plot(hsh*sh,col=grey(1:100/100),legend=FALSE)
unlink(demfile) 

## using 4 threads explicitly instead of the auto-selected default
sh = doshade2(dem,sv,max_threads_n=4)
}

}
