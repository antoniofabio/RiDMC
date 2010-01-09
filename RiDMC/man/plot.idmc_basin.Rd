\name{plot.idmc_basin}
\alias{plot.idmc_basin}
\alias{basinGrob}
\alias{makeBasinsPalette}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
\method{plot}{idmc_basin}(x, y, ..., add=FALSE)

basinGrob(idmc_basin,
  rasterObj=idmc_basin$raster,
  palette=makeBasinsPalette(idmc_basin),
  labels.attractors=NULL, labels.basins=NULL,
  label.infinity="infinity",
  legend=TRUE,
  attractorPoints=TRUE,
  pch=16, cex=1,
  main=getModelName(idmc_basin$model),
  xlab=rasterXname(rasterObj),
  ylab=rasterYname(rasterObj),
  axes=TRUE, mar=NULL, bty=FALSE, ...)

makeBasinsPalette(rasterObj,
  color.attractors=NULL,
  color.basins=NULL,
  color.infinity='black',
  paletteFun=rainbow)
}
\arguments{
 \item{x, idmc_basin}{iDMC basin object}
 \item{y}{unused}
 \item{rasterObj}{a \code{\link{Raster}} object, typically obtained from
   \code{\link{Basin}}}
 \item{palette}{raster image palette}
 \item{labels.attractors, labels.basins, label.infinity}{labels for attractors, basins and infinity (only used in legend)}
 \item{main, xlab, ylab, axes, mar, bty}{see \code{\link{plotGrob}}}
 \item{legend}{should a legend be showed? (TRUE or FALSE)}
 \item{attractorPoints}{should attractor trajectories be superposed on plot?}
 \item{pch, cex}{attractor trajectories point type and relative size. Ignored if \code{attractorPoints==FALSE}}
 \item{add}{add to existing plot}
 \item{...}{In \code{plot}: arguments passed to \code{basinGrob}. In
   \code{basinGrob}: arguments passed to \code{\link{grob}}}
 \item{color.attractors}{vector of attractor colors}
 \item{color.basins}{vector of basin colors}
 \item{color.infinity}{color for basin of infinity}
 \item{paletteFun}{default palette function}
}
\description{
Plots basins of attraction of 2-dim. discrete dynamical system
}
\details{
Plots basins of attraction of 2-dim. discrete dynamical system.

You can optionally add a colors legend (\code{legend=TRUE}) and superpose attractors trajectories
(attractorPoints=TRUE).
}
\value{
A \code{basinGrob} object. For \code{plot}, as a side effect, a (grid) plot is produced on current graphical device
}
\author{
Antonio, Fabio Di Narzo
}
\seealso{
\code{\link{Basin}}
}
\examples{
m1 <- Model(exModelFile('henon'))
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 50, c(-2, 2), 50, 100, 100)
plot(b1)

##
##ex-post plot editing
##
grid.edit("basinGrob", grep=TRUE,
  color.infinity="violet",
  color.attractors="black",
  color.basins="white")

grid.edit("basinGrob", grep=TRUE,
  attractorPoints=FALSE)
}
\keyword{hplot}
