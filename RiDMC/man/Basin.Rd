\name{Basin}
\alias{Basin}
\alias{as.matrix.idmc_basin}
\alias{print.idmc_basin}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
Basin(model, parms, xlim, xres=100, ylim, yres=100, 
	attractorLimit, attractorIterations, 
	method=c('fast', 'slow'), ntries, seed)
\method{as.matrix}{idmc_basin}(x, ...)
\method{print}{idmc_basin}(x, ...)
}
\arguments{
 \item{model,x}{iDMC model object}
 \item{parms}{named vector of model parameter values}
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

In the slow algorithm, there is a preliminary search of map attractors by iterating from \code{ntries} random starting positions in the grid. In some cases, this gives more accurate results.

Note: with the slow algorithm, use the \code{seed} argument to get reproducible results.
}
\value{
\code{Basin} returns a \code{idmc_basin} object, which can be subsequently plotted
by calling \code{plot(\dots)} on it.

The \code{as.matrix} method converts the \code{idmc_basin} object in a plain matrix of integers.
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
m1 <- Model('henon')
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 100, 100)
plot(b1)

##Using the slow algorithm for a more complex model:
m2 <- Model('cremona')
\dontrun{
b2 <- Basin(m2, 1.33, xlim=c(-1.2, 1.2), xres=300,
  ylim=c(-1.2, 1.2), yres=300,
  attractorLimit=1000, attractorIterations=1000,
  method='slow', ntries=1000, seed=123)
}
\dontshow{
b2 <- Basin(m2, 1.33, xlim=c(-1.2, 1.2), xres=50,
  ylim=c(-1.2, 1.2), yres=50,
  attractorLimit=1000, attractorIterations=1000,
  method='slow', ntries=1000, seed=123)
}
plot(b2)
##A plot with no annotations:
plot(b2, legend=FALSE, axes=FALSE, xlab=NULL, ylab=NULL, main=NULL)

##Customizing
#A simple case with just 1 attractor:
plot(b1, color.attractors='blue', color.basins='green', color.infinity='white')
#Improve plot resolution, then add trajectory points:
\dontrun{
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 500, c(-2, 2), 500, 100, 100)
}
plot(b1, legend=TRUE, attractorPoints=TRUE, pch=1, cex=1)

\dontshow{
m1 <- Model(exModelFile('henon'))
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 20, c(-2, 2), 20, 10, 10)
mat <- as.matrix(b1)
stopifnot(inherits(mat, 'matrix'))
}

}
