\name{Basin}
\alias{Basin}
\alias{print.idmc_basin}
\alias{plot.idmc_basin}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
Basin(model, parms, xlim, xres=100, ylim, yres=100, attractorLimit, attractorIterations)
\method{print}{idmc_basin}(x, ...)
\method{plot}{idmc_basin}(x, y, palette=rainbow, xlab, ylab, ...)
}
\arguments{
 \item{model,x}{iDMC model object}
 \item{parms}{model parameter values}
 \item{xlim,ylim}{x and y limits}
 \item{xres, yres}{x and y resolution}
 \item{attractorLimit}{transient}
 \item{attractorIterations}{max number of attractor iterations}
 \item{y}{unused}
 \item{palette}{palette functions for generating image colors}
 \item{xlab, ylab}{x and y axes labels}
 \item{...}{arguments to and from other methods}
}
\description{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\details{
TODO
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}
}
\examples{
m <- Model(exModelFile('henon'))
system.time(
	b <- Basin(m, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 1000, 1000)
)
plot(b)
}
