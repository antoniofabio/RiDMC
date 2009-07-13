\name{plot.idmc_basin}
\alias{plot.idmc_basin}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
\method{plot}{idmc_basin}(x, y,
                          color.attractors=NULL, color.basins=NULL, color.infinity='black',
                          default.palette=rainbow,
                          labels.attractors=NULL, labels.basins=NULL, label.infinity='infinity',
                          main = getModelName(getBasinModel(x)),
                          legend = TRUE, attractorPoints=FALSE,
                          pch=16, cex=0.2, add=FALSE, ...)
}
\arguments{
 \item{x}{iDMC model object}
 \item{y}{unused}
 \item{color.attractors}{vector of attractor colors}
 \item{color.basins}{vector of basin colors}
 \item{color.infinity}{color for basin of infinity}
 \item{default.palette}{default palette function}
 \item{labels.attractors, labels.basins, label.infinity}{labels for attractors, basins and infinity (only used in legend)}
 \item{main}{plot title (set to NULL for disabling)}
 \item{legend}{should a legend be showed? (TRUE or FALSE)}
 \item{attractorPoints}{should attractor trajectories be superposed on plot?}
 \item{pch, cex}{attractor trajectories point type and relative size. Ignored if \code{attractorPoints==FALSE}}
 \item{add}{add to existing plot}
 \item{...}{arguments to and from other methods}
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
none. As a side effect, a (grid) plot is produced on current graphical device
}
\author{
Antonio, Fabio Di Narzo
}
\seealso{
\code{\link{Basin}}
}
\examples{
##
##See 'Basin' examples
##
}
\keyword{hplot}
