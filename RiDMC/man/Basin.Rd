\name{Basin}
\alias{Basin}
\alias{print.idmc_basin}
\alias{plot.idmc_basin}
\title{
Computes basins of attraction for a 2-dim. discrete dynamical system
}
\usage{
Basin(model, parms, xlim, xres=100, ylim, yres=100, 
	attractorLimit, attractorIterations, 
	method=c('fast', 'slow'), ntries, seed)
\method{print}{idmc_basin}(x, ...)
\method{plot}{idmc_basin}(x, y, color.attractors, color.basins, 
	color.infinity, main, xlab, ylab, ...)
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
 \item{y}{unused}
 \item{color.attractors}{vector of attractor colors}
 \item{color.basins}{vector of basin colors}
 \item{color.infinity}{color for basin of infinity}
 \item{main}{plot title}
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
##Using the fast algoritm for a simple model:
m <- Model(exModelFile('henon'))
system.time(
	b1 <- Basin(m, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 1000, 1000)
)
plot(b1)

##Using the slow algorithm for a more complex model:
m <- Model(exModelFile('cremona'))
system.time(
	b2 <- Basin(m, 1.33, c(-1.2, 1.2), 300, 
	c(-1.2, 1.2), 300, 1000, 1000, method='slow', ntries=100, seed=123)
)##This takes 5.5sec on my Turion-1800MHz
plot(b2)

##Customizing palette
#A simple case with just 1 attractor:
plot(b1, color.attractors='blue', color.basins='green')

#A more complex case with many attractors
color.attractors <- rgb(1:50/50, 0, 0)
color.basins <- rgb(0, 50:1/50, 0)
plot(b2, color.attractors=color.attractors, color.basins=color.basins)
}
