##
##Raster data manipulation
##

##Raster data: a grid of numerical values.
## Encapsulates horiz. and vert. ranges infos
Raster <- function(xlim, ylim, xres=100, yres=xres, data,
                   xName='x', yName='y') {
  stopifnot(length(xlim) == 2)
  stopifnot(length(ylim) == 2)
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
            xName=xName,
            yName=yName,
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
rasterXname <- function(raster) attr(raster, "xName")
rasterYname <- function(raster) attr(raster, "yName")
.rasterValuesSequence <- function(lim, howMany) {
  lim <- range(lim)
  eps <- diff(lim)/howMany
  return(seq(lim[1]+eps/2, lim[2]-eps/2, length=howMany))
}
rasterXvalues <- function(raster)
  .rasterValuesSequence(rasterXlim(raster), rasterXres(raster))
rasterYvalues <- function(raster)
  .rasterValuesSequence(rasterYlim(raster), rasterYres(raster))
as.matrix.Raster <- function(x, ...) x

rasterFill <- function(raster, value=1) {
  raster[,] <- value
  raster
}
rasterFillRect <- function(raster, x0, y0, width, height=width, value=1) {
  xlim <- rasterXlim(raster)
  ylim <- rasterYlim(raster)
  x1 <- x0 + width
  y1 <- y0 - height
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

rasterFillLocus <- function(raster, FUN, value=1) {
  FUN <- match.fun(FUN)
  pts <- raster2Pts(rasterFill(raster, value=1), value=1)
  ids <- apply(pts, 1, FUN)
  stopifnot(all(is.logical(ids)))
  pts <- pts[ids,]
  return(rasterSetPoints(raster, pts, value=value))
}

rasterFillCircle <- function(raster, center, radius, value=1) {
  rasterFillLocus(raster, function(xy) {
    return(sqrt(sum((xy - center)^2)) < radius)
  }, value = value)
}

rasterFillPolygon <- function(raster, polygon, value=1) {
  rasterFillLocus(raster, function(pt) polyContainsPt(polygon, pt), value=value)
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
  pts <- pts[rasterContains(raster, pts), ,drop=FALSE]
  ids <- setDiscretize(pts,
                       rasterXlim(raster), rasterXres(raster),
                       rasterXlim(raster), rasterXres(raster))
  raster[ids] <- value
  raster
}

print.Raster <- function(x, labels=NULL, showContents=TRUE, ...) {
  cat(rasterXname(x), ': [', paste(rasterXlim(x), collapse="; "), ']\n', sep='')
  cat(rasterYname(x), ': [', paste(rasterYlim(x), collapse="; "), ']\n', sep='')
  cat('resolution: ', rasterXres(x), ' x ', rasterYres(x), '\n', sep='')
  if(showContents) {
    cat('contents (%):\n')
    freqs <- table(as.vector(x))
    freqs <- freqs * 100 / sum(freqs)
    if(!is.null(labels)) {
      if(is.null(names(labels))) {
        names(freqs) <- labels
      } else {
        names(freqs) <- labels[names(freqs)]
      }
    }
    freqs <- -sort(-freqs)
    print(freqs[freqs >= 1], ...)
    if(sum(freqs < 1) > 0) {
      cat('less than 1%:\n')
      print(names(freqs[freqs < 1]))
    }
  }
}

lines.Raster <- function(x, y, ...) {
  xlim <- rasterXlim(x)
  ylim <- rasterYlim(x)
  grid.lines(x=c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]),
             y=c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1]),
             default.units="native",
             gp=gpar(...))
}

raster2Pts <- function(raster, value=1) {
  xyd <- which(raster==value, arr.ind=TRUE)
  cbind(x=rasterXvalues(raster)[xyd[,2]],
        y=rasterYvalues(raster)[xyd[,1]])
}

as.grob.Raster <- function(x, ...) {
  rasterGrob(x, ...)
}

plot.Raster <- function(x, y, ..., add=FALSE) {
  pG <- as.grob(x, ...)
  if(!add)
    grid.newpage()
  grid.draw(pG)
  invisible(pG)
}
