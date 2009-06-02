\name{Raster}
\alias{Raster}
\alias{rasterMap}
\alias{rasterInvMap}
\alias{rasterFill}
\alias{rasterFillRect}
\alias{rasterFillLocus}
\alias{rasterXres}
\alias{rasterYres}
\alias{rasterXlim}
\alias{rasterYlim}
\alias{rasterXrange}
\alias{rasterYrange}
\alias{rasterXeps}
\alias{rasterYeps}
\alias{rasterContains}
\alias{rasterCheckPoints}
\alias{rasterSetPoints}
\alias{raster2Pts}
\alias{plot.Raster}
\alias{lines.Raster}
\alias{as.matrix.Raster}
\title{
  Raster data handling
}
\usage{
Raster(xlim, ylim, xres=100, yres=xres, data)
rasterXres(raster)
rasterYres(raster)
rasterXlim(raster)
rasterYlim(raster)
rasterXrange(raster)
rasterYrange(raster)
rasterXeps(raster)
rasterYeps(raster)
rasterFill(raster, value=1)
rasterFillRect(raster, x0, y0, x1, y1, value=1)
rasterFillLocus(raster, FUN, value=1)
rasterFillPolygon(raster, polygon, value=1)
rasterMap(raster, FUN, value=1, outvalue=value)
rasterInvMap(raster, FUN, value=1, outvalue=value)
rasterContains(raster, pts)
rasterCheckPoints(raster, pts, value=1)
rasterSetPoints(raster, pts, value=1)
raster2Pts(raster, value=1)

\method{plot}{Raster}(x, y,
                      palette=gray.colors(length(unique(c(as.vector(x), 1)))),
                      xlab="x", ylab="y",
                      axes=TRUE,
                      mar=c(4,4,2,2),
                      ...,
                      add=FALSE)
\method{lines}{Raster}(x, y, ...)
\method{as.matrix}{Raster}(x, ...)
}
\arguments{
  \item{raster}{a \code{Raster} object}
  \item{xlim, ylim}{ranges}
  \item{xres, yres}{resolutions}
  \item{data}{raw raster matrix data}
  \item{x0, y0, x1, y1}{rectangle opposite corners}
  \item{x}{a Raster object}
  \item{y}{ignored}
  \item{value, outvalue}{reference integer value to be set/to look for into Raster
    cells. It can be safely ignored for standard usage}
  \item{xlab, ylab, axes, mar, add}{see \code{plotGrob}}
  \item{FUN}{for Map, InvMap: map function from R2 to R2. For FillLocus: boolean
                      check function, from R2 to {true, false}}
  \item{polygon}{ordered set of polygon vertices. Polygon is 'closed' automatically}
  \item{pts}{vertical matrix of 2d points}
  \item{palette}{image palette}
  \item{...}{arguments to and from other methods}
}
\description{
  Basic raster data handling
}
\details{
  Basic raster data handling
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\keyword{ manip }
\keyword{ utilities }
\examples{
A <- Raster(xlim=c(0, 100), ylim=c(0, 100), xres=100, yres=100)
B <- rasterFillRect(A, 25, 25, 75, 75)
plot(B)

F <- function(xy) xy + 10
C <- rasterMap(B, F)
plot(C)

plot(B*C)
plot(B + B*C)
plot(B + C + B*C)

F <- function(xy) xy * 1.2
D <- rasterMap(B, F)
plot(D)

iF <- function(xy) xy / 1.2
D1 <- rasterInvMap(B, iF)
plot(D1)

plot(D + D1)
}
