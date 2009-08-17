\name{makePalette}
\alias{makePalette}
\title{
  Colors palette
}
\usage{
makePalette(values, howMany=7, paletteFun=topo.colors)
}
\arguments{
  \item{values}{raster image values}
  \item{howMany}{how many (max) different color do you want in the
    palette}
  \item{paletteFun}{palette function}
}
\description{
  Make a convenient colors palette from a matrix-like image object.
}
\details{
  Make a convenient colors palette from a matrix-like image object.
  Colors are generated using the specified \code{paletteFun} function.
  You can fix a max number of different colors which should make the
  palette. If there are more distinct values in the image, all the extra
  values get merged in one common extra color.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\keyword{ dplot }
\keyword{ color }
