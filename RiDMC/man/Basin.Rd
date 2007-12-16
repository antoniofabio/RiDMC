\name{Basin}
\alias{Basin}
\alias{print.idmc_basin}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
Basin(model, parms, xlim, xres=100, ylim, yres=100, 
	attractorLimit, attractorIterations, 
	method=c('fast', 'slow'), ntries, seed)
\method{print}{idmc_basin}(x, ...)
}
\arguments{
 \item{model,x}{iDMC model object}
 \item{parms}{model parameter values}
 \item{xlim,ylim}{x and y limits}
 \item{xres, yres}{x and y resolution}
 \item{attractorLimit}{transient}
 \item{attractorIterations}{max number of attractor iterations}
 \item{method}{algorithm type}
 \item{ntries}{number of tries for finding attractors (only for 'slow' method)}
 \item{seed}{RNG seed (only for 'slow' method)}
 \item{...}{arguments to and from other methods}
}
\description{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\details{
Computes basins of attraction for a 2-dim. discrete dynamical system.

The specified rectangular domain is partitioned in a regular grid of
\code{xres} by \code{yres} cells.
Each cell is then coloured by iterating the model starting from that position
and observing on which attractor the trajectory falls.

In the fast algorithm, attractors are identified 'on the fly'.

In the slow algorithm, there is a preliminary search of map attractors by iterating from \code{ntries} random starting positions in the grid. In some cases, this gives
more accurate results.

Note: with the slow algorithm, use the \code{seed} argument to get reproducible results.
}
\value{
\code{Basin} returns a \code{idmc_basin} object, which can be subsequently plotted
by calling \code{plot(\dots)} on it.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}
}
\examples{
##Using the fast algoritm for a simple model:
m1 <- Model(exModelFile('henon'))
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 100, 100)
plot(b1)

##Using the slow algorithm for a more complex model:
m2 <- Model(exModelFile('cremona'))
b2 <- Basin(m2, 1.33, c(-1.2, 1.2), 300, c(-1.2, 1.2), 300, 1000, 1000, method='slow', ntries=1000, seed=123)
grid.newpage()
plot(b2)
##A plot with no annotations:
grid.newpage()
plot(b2, axes=FALSE, xlab=NULL, ylab=NULL, main=NULL)

##Customizing
#A simple case with just 1 attractor:
grid.newpage()
plot(b1, color.attractors='blue', color.basins='green', color.infinity='white')
#Improve plot resolution, then add trajectory points:
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 500, c(-2, 2), 500, 100, 100)
grid.newpage()
plot(b1, legend=TRUE, attractorPoints=TRUE, pch=1, cex=1)

}
