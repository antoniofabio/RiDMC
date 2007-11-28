\name{plot.idmc_basin}
\alias{plot.idmc_basin}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
\method{plot}{idmc_basin}(x, y, color.attractors, color.basins, 
  color.infinity, labels.attr, labels.bas, label.infty='infinity',
  main = getModelName(getBasinModel(x)),
  xlab = getModelVarNames(getBasinModel(x))[1],
  ylab = getModelVarNames(getBasinModel(x))[2],
  axes=TRUE, legend=FALSE, ...)
}
\arguments{
 \item{x}{iDMC model object}
 \item{y}{unused}
 \item{color.attractors}{vector of attractor colors}
 \item{color.basins}{vector of basin colors}
 \item{color.infinity}{color for basin of infinity}
 \item{main}{plot title (set to NULL for disabling)}
 \item{xlab, ylab}{x and y axes labels (set to NULL for disabling)}
 \item{labels.attr, labels.bas, label.infty}{labels for attractors, basins and infinity (only used in legend)}
 \item{axes}{should axes be showed? (TRUE or FALSE)}
 \item{legend}{should a legend be showed? (TRUE or FALSE)}
 \item{...}{arguments to and from other methods}
}
\description{
Plots basins of attraction of 2-dim. discrete dynamical system
}
\details{
Plots basins of attraction of 2-dim. discrete dynamical system.

You can optionally add a colors legend (\code{legend=TRUE}).
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
