\name{Bifurcation}
\alias{Bifurcation}
\alias{plot.idmc_bifurcation}
\title{
Bifurcation plot
}
\usage{
Bifurcation(idmc_model, which.var.store=1, par, var, 
	which.par.vary=1, par.min, par.max, par.howMany=100, 
	transient=100, max.period=50)
\method{plot}{idmc_bifurcation}(x,y, type='p', pch=16, main, xlab, ylab, ...)
}
\arguments{
 \item{idmc_model}{a model object, as returned by \code{\link{Model}}}
 \item{which.var.store}{which variable is to be stored?}
 \item{par}{a valid set of *all* model parameter values}
 \item{var}{starting values}
 \item{which.par.vary}{which parameter is to be varied?}
 \item{par.min, par.max, par.howMany}{specification of range of values for the selected parameter}
 \item{transient}{transient length}
 \item{max.period}{max period for a cycle}
 \item{x}{an \code{idmc_bifurcation} object}
 \item{type, pch, main, xlab, ylab}{usual plot options}
 \item{y}{currently unused}
 \item{...}{arguments to and from other methods}
}
\description{
Computes bifurcation plot.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}, \code{\link{LyapunovExponents}}
}
\examples{
m <- Model(exModelFile('logistic'))
bf <- Bifurcation(m, par=1, var=0.3, par.min=2.5, par.max=4.0)
plot(bf, cex=0.5)

par(mfrow=c(2,1))
le <- LyapunovExponents(m, par=1, var=0.3, par.min=2.5, par.max=4.0, time=500)
plot(le)
abline(h=0, col="gray")
plot(bf, cex=0.15)
}
