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
m <- Model(exModelFile('henon'))
system.time(
	b1 <- Basin(m, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 1000, 1000)
)
plot(b1, legend=TRUE)

##Using the slow algorithm for a more complex model:
m <- Model(exModelFile('cremona'))
system.time(
	b2 <- Basin(m, 1.33, c(-1.2, 1.2), 300, 
	c(-1.2, 1.2), 300, 1000, 1000, method='slow', ntries=1000, seed=123)
)##This takes about 8sec on my Turion-1800MHz
plot(b2)
##A plot with no annotations:
plot(b2, axes=FALSE, xlab=NULL, ylab=NULL, main=NULL)

##Customizing palette
#A simple case with just 1 attractor:
plot(b1, color.attractors='blue', color.basins='green')

#A more complex case with many attractors
color.attractors <- rgb(1:60/60, 0, 0)
color.basins <- rgb(0, 60:1/60, 0)
plot(b2, color.attractors=color.attractors, color.basins=color.basins)
}
