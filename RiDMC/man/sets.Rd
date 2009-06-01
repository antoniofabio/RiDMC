\name{Set data}
\alias{setMap}
\alias{setDiscretize}
\alias{set2Raster}
\title{
  Basic (moslty bivariate) sets data handling
}
\usage{
setMap(A, f)
setDiscretize(A,
              xlim=range(A[,1]), xres=100,
              ylim=range(A[,2]), yres=100)
set2Raster(A, raster=Raster(range(A[,1]), range(A[,2])), value=1)
}
\arguments{
  \item{A, B}{sets of Rn points}
  \item{f}{function}
  \item{raster}{a \code{Raster} object}
  \item{xlim, ylim}{ranges}
  \item{xres, yres}{resolutions}
  \item{value}{value to be filled into raster cells}
  \item{...}{arguments to and from other methods}
}
\description{
  Basic (moslty bivariate) sets data handling
}
\details{
  Basic (moslty bivariate) sets data handling
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ internal }
