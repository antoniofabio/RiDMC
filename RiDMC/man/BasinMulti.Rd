\name{BasinMulti}
\alias{BasinMulti}
\alias{print.idmc_basin_multi}
\title{
Computes slice of basins of attraction for a >2-dim. discrete dynamical system
}
\usage{
BasinMulti(model, parms, xlim, xres=100, ylim, yres=100, attractorLimit, attractorIterations,
	startValues, xvar=0, yvar=1, ntries=20, eps=getOption("ts.eps"), seed)
\method{print}{idmc_basin_multi}(x, ...)
}
\arguments{
 \item{model}{iDMC model object}
 \item{parms}{model parameter values}
 \item{xlim,ylim}{x and y limits}
 \item{xres, yres}{x and y resolution}
 \item{attractorLimit}{transient}
 \item{attractorIterations}{max number of attractor iterations}
 \item{startValues}{starting values for all model variables (\code{xvar} and \code{yvar} will be ignored)}
 \item{xvar, yvar}{x and y variables (as 0-based index)}
 \item{ntries}{number of tries for finding attractors}
 \item{eps}{neighborhood window}
 \item{seed}{RNG seed (only for 'slow' method)}
 \item{...}{arguments to and from other methods}
}
\description{
Computes slice of basins of attraction for a >2-dim. discrete dynamical system
}
\details{
Computes slice of basins of attraction for a >2-dim. discrete dynamical system.

The specified rectangular domain is partitioned in a regular grid of
\code{xres} by \code{yres} cells.
Each cell is then coloured by iterating the model starting from that position
and observing on which attractor the trajectory falls.

There is a preliminary search of map attractors by iterating from \code{ntries} random starting positions in the grid.

Note: use the \code{seed} argument to get reproducible results.
}
\value{
A \code{idmc_basin} object, which can be subsequently plotted by calling \code{plot(\dots)} on it.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{as.matrix.idmc_basin}}, \code{\link{Model}}
}
\examples{
##Simple model
b1 <- BasinMulti(m1, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 100, 100)
plot(b1)

##A more complex model:
b2 <- BasinMulti(m2, 1.33, c(-1.2, 1.2), 300, c(-1.2, 1.2), 300, 1000, 1000)
plot(b2)
##A plot with no annotations:
plot(b2, axes=FALSE, xlab=NULL, ylab=NULL, main=NULL)

##Customizing
#A simple case with just 1 attractor:
plot(b1, color.attractors='blue', color.basins='green', color.infinity='white')
#Improve plot resolution, then add trajectory points:
#b1 <- BasinMulti(m1, c(1.42,0.3), c(-2,2), 500, c(-2, 2), 500, 100, 100)
#plot(b1, legend=TRUE, attractorPoints=TRUE, pch=1, cex=1)
}
