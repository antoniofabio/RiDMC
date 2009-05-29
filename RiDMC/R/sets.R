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
Raster <- function(xlim, ylim, data, xres=ncol(A), yres=nrow(A)) {
  if(missing(data)) {
    if(missing(xres))
      xres <- 100
    if(missing(yres))
      yres <- 100
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

rasterSetAll <- function(raster, value) {
  raster[,] <- value
  raster
}

rasterSetPoints <- function(raster, pts, value) {
  stopifnot(ncol(pts) == 2)
  ids <- setDiscretize(pts,
                       rasterXlim(raster), rasterXres(raster),
                       rasterXlim(raster), rasterXres(raster))
  raster[ids] <- value
  raster
}

lines.Raster <- function(x, y, ...) {
  xlim <- rasterXlim(x)
  ylim <- rasterYlim(x)
  lines(c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]),
        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1]), ...)
}

setDiscretize <- function(A,
                          xlim=range(A[,1]), xres=100,
                          ylim=range(A[,2]), yres=100) {
  xeps <- diff(xlim) / xres
  yeps <- diff(ylim) / yres
  A <- A[A[,1] >= xlim[1] & A[,1] <= xlim[2],]
  A <- A[A[,2] >= ylim[1] & A[,2] <= ylim[2],]
  Ax <- floor((A[,1] - xlim[1]) / xeps)
  Ay <- floor((A[,2] - ylim[1]) / yeps)
  return(cbind(x=Ax, y=Ay) + 1)
}

set2Raster <- function(A, raster=Raster(range(A[,1]), range(A[,2])), value=1)
  rasterSetPoints(rasterSetAll(raster, 0), A, value)

raster2Set <- function(raster, value=1) {
  xyd <- which(raster==value, TRUE)
  cbind(x=(xyd[,1] - 1)* rasterXeps(raster) + rasterXlim(raster)[1] + rasterXeps(raster)/2,
        y=(xyd[,2] - 1)* rasterYeps(raster) + rasterYlim(raster)[1] + rasterYeps(raster)/2)
}

rasterUnion <- function(a, b) {
  xlim <- range(rasterXlim(a), rasterXlim(b))
  ylim <- range(rasterYlim(a), rasterYlim(b))
  xeps <- max(rasterXeps(a), rasterXeps(b))
  yeps <- max(rasterYeps(a), rasterYeps(b))
  ans <- Raster(xlim, ylim, xres=round(1/xeps), yres=round(1/yeps))
  a1 <- set2Raster(raster2Set(a, value=1), ans, value=1)
  b1 <- set2Raster(raster2Set(b, value=1), ans, value=1)
  a1 + b1 - (a1 * b1)
}

setNormalize <- function(A, raster=Raster(range(A[,1]), range(A[,2])))
  raster2Set(set2Raster(A, raster))

setInv <- function(B, f, raster) {
  domain <- raster2Set(rasterSetAll(raster, 1), 1)
  n <- nrow(domain)
  id <- rep(FALSE, n)
  xEps <- rasterXeps(raster)
  yEps <- rasterYeps(raster)
  for(i in seq_len(n)) {
    pt <- f(domain[i,])
    dist <- abs(sweep(B, 2, pt))
    id[i] <- any(dist[,1] <= xEps & dist[,2] <= yEps)
    if((i %% (n/20)) == 0)
      message(round(i*100/n, 2), "% done")
  }
  return(domain[id,])
}

setIntersect <- function(A, B) {
  A <- unique(A)
  B <- unique(B)
  if(nrow(A) > nrow(B)) {
    C <- A
    A <- B
    B <- C
  }
  ans <- matrix(0, nrow(A), 2)
  na <- 0
  for(i in seq_len(nrow(A))) {
    pt <- A[i,]
    dist <- apply(abs(sweep(B, 2, pt)), 1, max) == 0
    if(any(dist)) {
      na <- na+1
      ans[na,] <- pt
    }
  }
  return(ans[seq_len(na),])
}

setDiff <- function(A, B) {
  ans <- matrix(nrow(A), 0, 2)
  B <- setIntersect(A, B)
  na <- 0
  for(i in seq_len(nrow(A))) {
    pt <- A[i,]
    dist <- abs(sweep(B, 2, pt))
    if(all(dist > 0)) {
      na <- na+1
      ans[na,] <- pt
    }
  }
  return(ans[seq_len(na),])
}

plot.Raster <- function(x, y,
                        palette=rainbow(length(unique(as.vector(x)))),
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
  levs <- unique(as.vector(x))
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
