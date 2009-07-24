\name{Raster}
\alias{Raster}
\alias{rasterMap}
\alias{rasterInvMap}
\alias{rasterFill}
\alias{rasterFillRect}
\alias{rasterFillLocus}
\alias{rasterFillCircle}
\alias{rasterFillPolygon}
\alias{rasterXres}
\alias{rasterYres}
\alias{rasterXlim}
\alias{rasterYlim}
\alias{rasterXrange}
\alias{rasterYrange}
\alias{rasterXeps}
\alias{rasterYeps}
\alias{rasterXname}
\alias{rasterYname}
\alias{rasterXvalues}
\alias{rasterYvalues}
\alias{rasterContains}
\alias{rasterCheckPoints}
\alias{rasterSetPoints}
\alias{raster2Pts}
\alias{print.Raster}
\alias{plot.Raster}
\alias{lines.Raster}
\alias{as.matrix.Raster}
\alias{as.grob.Raster}
\title{
  Raster data handling
}
\usage{
Raster(xlim, ylim, xres=100, yres=xres, data, xName="x", yName="y")
rasterXres(raster)
rasterYres(raster)
rasterXlim(raster)
rasterYlim(raster)
rasterXrange(raster)
rasterYrange(raster)
rasterXeps(raster)
rasterYeps(raster)
rasterXname(raster)
rasterYname(raster)
rasterXvalues(raster)
rasterYvalues(raster)
rasterFill(raster, value=1)
rasterFillRect(raster, x0, y0, width, height=width, value=1)
rasterFillCircle(raster, center, radius, value=1)
rasterFillLocus(raster, FUN, value=1)
rasterFillPolygon(raster, polygon, value=1)
rasterMap(raster, FUN, value=1, outvalue=value)
rasterInvMap(raster, FUN, value=1, outvalue=value)
rasterContains(raster, pts)
rasterCheckPoints(raster, pts, value=1)
rasterSetPoints(raster, pts, value=1)
raster2Pts(raster, value=1)

\method{print}{Raster}(x, labels=NULL, showContents=TRUE, ...)
\method{plot}{Raster}(x, y, ..., add=FALSE)
\method{lines}{Raster}(x, y, ...)
\method{as.matrix}{Raster}(x, ...)
\method{as.grob}{Raster}(x, ...)
}
\arguments{
  \item{raster}{a \code{Raster} object}
  \item{xlim, ylim}{ranges}
  \item{xres, yres}{resolutions}
  \item{data}{raw raster matrix data}
  \item{xName, yName}{(optional) \code{x} and \code{y} variables names}
  \item{x0, y0}{top-left rectangle corner}
  \item{width, height}{rectangle width and height}
  \item{x}{a Raster object}
  \item{labels}{labels for the raster numerical values}
  \item{showContents}{should raster contents table be printed on screen?}
  \item{y}{ignored}
  \item{value, outvalue}{reference integer value to be set/to look for into Raster
    cells. It can be safely ignored for standard usage}
  \item{add}{see \code{plotGrob}}
  \item{FUN}{for Map, InvMap: map function from R2 to R2. For FillLocus: boolean
                      check function, from R2 to (true, false)}
  \item{polygon}{ordered set of polygon vertices. Polygon is 'closed'
automatically}
  \item{center, radius}{circle parameters}
  \item{pts}{vertical matrix of 2d points}
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
B <- rasterFillRect(A, x0=25, y0=75, width=50)
B
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

circleLocus <- function(center, radius) {
  function(xy) {
    return(sqrt(sum((xy - center)^2)) <= radius)
  }
}
B <- rasterFillLocus(A, circleLocus(c(50, 50), 15))

A <- Raster(xlim=c(0, 100), ylim=c(0, 100), xres=100)
B <- rasterFillCircle(A, c(50, 50), 15)

poly <- matrix(c(20, 20,
                 60, 30,
                 80, 90),
                 ncol=2,
                 byrow=TRUE)
C <- rasterFillPolygon(A, poly)
plot(B + C)

poly <- matrix(c(20, 20,
                 60, 30,
                 20, 60),
                 ncol=2,
                 byrow=TRUE)
C <- rasterFillPolygon(A, poly)
}
