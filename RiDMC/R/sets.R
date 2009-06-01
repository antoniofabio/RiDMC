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

##Raster data: a grid of numerical values.
## Encapsulates horiz. and vert. ranges infos
Raster <- function(xlim, ylim, xres=100, yres=xres, data) {
  if(!missing(data)) {
    stopifnot(xres == NCOL(data))
    stopifnot(yres == NROW(data))
  } else {
    data <- matrix(0, xres, yres)
  }
  stopifnot(is.matrix(data))
  structure(data,
            xlim=xlim,
            ylim=ylim,
            class=c("Raster", "matrix"))
}
rasterXres <- function(raster) ncol(raster)
rasterYres <- function(raster) nrow(raster)
rasterXlim <- function(raster) attr(raster, "xlim")
rasterYlim <- function(raster) attr(raster, "ylim")
rasterXrange <- function(raster) diff(rasterXlim(raster))
rasterYrange <- function(raster) diff(rasterYlim(raster))
rasterXeps <- function(raster) rasterXrange(raster) / rasterXres(raster)
rasterYeps <- function(raster) rasterYrange(raster) / rasterYres(raster)
as.matrix.Raster <- function(x, ...) x

rasterFill <- function(raster, value=1) {
  raster[,] <- value
  raster
}
rasterFillRect <- function(raster, x0, y0, x1, y1, value=1) {
  xlim <- rasterXlim(raster)
  ylim <- rasterYlim(raster)
  stopifnot(x0 >= xlim[1] && x1 <= xlim[2])
  stopifnot(y0 >= ylim[1] && y1 <= ylim[2])
  ids <- setDiscretize(matrix(c(x0, x1, y0, y1), 2, 2),
                       xlim=xlim, xres=rasterXres(raster),
                       ylim=ylim, yres=rasterYres(raster))
  i0 <- ids[1,1]
  i1 <- ids[2,1]
  j0 <- ids[1,2]
  j1 <- ids[2,2]
  raster[seq(j0, j1), seq(i0, i1)] <- value
  return(raster)
}
rasterMap <- function(raster, FUN, value=1, outvalue=value) {
  set2Raster(setMap(raster2Pts(raster, value=value), match.fun(FUN)),
             raster, value=outvalue)
}
rasterInvMap <- function(raster, FUN, value=1, outvalue=value) {
  A <- raster2Pts(rasterFill(raster, value=value), value=value)
  FA <- setMap(A, match.fun(FUN))
  iA <- A[rasterCheckPoints(raster, FA, value=value),]
  rasterSetPoints(rasterFill(raster, 0), iA)
}
rasterContains <- function(raster, pts) {
  xlim <- rasterXlim(raster)
  ylim <- rasterYlim(raster)
  return(pts[,1] >= xlim[1] & pts[,1] <= xlim[2] &
         pts[,2] >= ylim[1] & pts[,2] <= ylim[2])
}

rasterCheckPoints <- function(raster, pts, value=1) {
  stopifnot(ncol(pts) == 2)
  ids <- setDiscretize(pts,
                       rasterXlim(raster), rasterXres(raster),
                       rasterXlim(raster), rasterXres(raster))
  apply(ids, 1, function(x) all(is.finite(x))) & (raster[ids] == value)
}
rasterSetPoints <- function(raster, pts, value=1) {
  stopifnot(ncol(pts) == 2)
  pts <- pts[rasterContains(raster, pts), ]
  ids <- setDiscretize(pts,
                       rasterXlim(raster), rasterXres(raster),
                       rasterXlim(raster), rasterXres(raster))
  raster[ids] <- value
  raster
}

lines.Raster <- function(x, y, ...) {
  xlim <- rasterXlim(x)
  ylim <- rasterYlim(x)
  grid.lines(x=c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]),
             y=c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1]),
             default.units="native",
             gp=gpar(...))
}

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
  return(cbind(x=Ax, y=Ay) + 1)
}

set2Raster <- function(A, raster=Raster(range(A[,1]), range(A[,2])), value=1)
  rasterSetPoints(rasterFill(raster, 0), A, value)

raster2Pts <- function(raster, value=1) {
  xyd <- which(raster==value, TRUE)
  cbind(x=(xyd[,1] - 1)* rasterXeps(raster) + rasterXlim(raster)[1] + rasterXeps(raster)/2,
        y=(xyd[,2] - 1)* rasterYeps(raster) + rasterYlim(raster)[1] + rasterYeps(raster)/2)
}

plot.Raster <- function(x, y,
                        palette=gray.colors(length(unique(c(as.vector(x), 1)))),
                        xlab="x", ylab="y",
                        axes=TRUE,
                        mar=c(4,4,2,2),
                        ...,
                        add=FALSE) {
  xlim <- rasterXlim(x)
  xeps <- rasterXeps(x)
  ylim <- rasterYlim(x)
  yeps <- rasterYeps(x)
  xseq <- seq(xlim[1] + xeps/2, xlim[2] - xeps/2, length=dim(x)[1])
  yseq <- seq(ylim[1] + yeps/2, ylim[2] - yeps/2, length=dim(x)[2])
  x <- as.matrix(x)
  levs <- -sort(-unique(c(as.vector(x), 1)))
  palette <- palette[seq_along(levs)]
  col <- matrix(palette[match(as.vector(x), levs)], dim(x)[1], dim(x)[2])
  gr <- imageGrob(col, xlim=xlim, ylim=ylim, respect=FALSE,
            name="raster")
  pG <- plotGrob(gr, xlab=xlab, ylab=ylab, axes=axes, mar=mar, ...)
  if(!add)
    grid.newpage()
  grid.draw(pG)
  invisible(pG)
}
