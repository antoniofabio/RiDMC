\name{Sets and raster data}
\alias{setMap}
\alias{setDiff}
\alias{setIntersect}
\alias{Raster}
\title{
  Basic (moslty bivariate) sets and raster data handling
}
\usage{
setMap(A, f)
Raster(xlim, ylim, data, xres=ncol(A), yres=nrow(A))
rasterXres(raster)
rasterYres(raster)
rasterXlim(raster)
rasterYlim(raster)
rasterXrange(raster)
rasterYrange(raster)
rasterXeps(raster)
rasterYeps(raster)
as.matrix.Raster(x, ...)

rasterSetAll(raster, value)
rasterSetPoints(raster, pts, value)
plot.Raster(x, y,
           xlim=rasterXlim(x),
           ylim=rasterYlim(x),
           pch='.',
           ...)
lines.Raster(x, y, ...)
setDiscretize(A,
              xlim=range(A[,1]), xres=100,
              ylim=range(A[,2]), yres=100)
set2Raster(A, raster=Raster(range(A[,1]), range(A[,2])),value=1)
raster2Set(raster, value=1)
rasterUnion(a, b)
setNormalize(A, raster=Raster(range(A[,1]), range(A[,2])))
setInv(B, f, raster)
setIntersect(A, B)
setDiff(A, B)
}
\arguments{
  \item{A, B}{sets of Rn points}
  \item{f}{function}
  \item{raster}{a \code{Raster} object}
  \item{xlim, ylim}{ranges}
  \item{xres, yres}{resolutions}
  \item{pch}{see \code{plot}}
  \item{data}{raw raster matrix data}
}
\description{
  Basic (moslty bivariate) sets and raster data handling
}
\details{
  Basic (moslty bivariate) sets and raster data handling
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ internal }
\examples{
f <- function(x) x^2

A <- Raster(c(0, 1), c(0, 1))
Br <- rasterSetAll(Raster(c(0.25, 0.75), c(0.25, 0.75), ,70, 70), 1)
B <- raster2Set(Br)
B1 <- setInv(B, f, A)
  
plot(rasterSetAll(A, 0))
points(B1, pch=16)
lines(Raster(c(0.25, 0.75), c(0.25, 0.75), ,10, 10), lty=2)

C <- raster2Set(set2Raster(B, A) * set2Raster(B1, A))
points(C, pch=16, col="red")
}
