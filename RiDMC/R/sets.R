##
##Sets of R^n points utilities
##

##Map function f to set A
setMap <- function(A, f) {
  d <- dim(A)[2]
  B <- t(apply(A, 1, f))
  stopifnot(nrow(A) == nrow(B))
  return(B)
}

set2Raster <- function(A, raster=Raster(range(A[,1]), range(A[,2])), value=1)
  rasterSetPoints(rasterFill(raster, 0), A, value)

setDiscretize <- function(A,
                          xlim=range(A[,1]), xres=100,
                          ylim=range(A[,2]), yres=100) {
  xeps <- diff(xlim) / xres
  yeps <- diff(ylim) / yres
  Ax <- floor((A[,1] - xlim[1]) / xeps)
  Ay <- floor((A[,2] - ylim[1]) / yeps)
  Ax[Ax < 0] <- NA
  Ay[Ay < 0] <- NA
  Ax[Ax >= xres] <- NA
  Ay[Ay >= yres] <- NA
  return(cbind(x=Ay, y=Ax) + 1)
}
