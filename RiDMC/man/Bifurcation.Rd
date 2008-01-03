\name{Bifurcation}
\alias{Bifurcation}
\alias{as.matrix.idmc_bifurcation}
\alias{plot.idmc_bifurcation}
\title{
Bifurcation plot
}
\usage{
Bifurcation(idmc_model, which.var.store=1, par, var, 
	which.par.vary=1, par.min, par.max, par.howMany=100, 
	transient=100, max.period=50)
\method{as.matrix}{idmc_bifurcation}(x, ...)
\method{plot}{idmc_bifurcation}(x, y, pch=16, cex=0.2,
  size = unit(cex, 'char'),
  main = getModelName(x$model),
  xlab = x$which.par,
  ylab = x$which.var,
  axes=TRUE, bty=TRUE, mar=NULL, ...)
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
 \item{main, xlab, ylab}{usual plot options (see \code{\link{plotGrob}})}
 \item{pch, cex}{see \code{\link{par}}}
 \item{size}{points size in an arbitrary grid \code{\link[grid]{unit}} (alternative to \code{cex})}
 \item{y}{currently unused}
 \item{axes, bty, mar, ...}{see \code{\link{plotGrob}}}
}
\description{
Computes a bifurcation diagram.

\code{as.matrix} converts it into a plain matrix object.

\code{plot} plots it in a convenient manner.
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
grid.newpage()
plot(bf, cex=0.3)

grid.newpage()

##Setup: 2 rows, 1 column:
pushViewport(viewport(layout=grid.layout(2, 1)))
##Go to second row:
pushViewport(viewport(layout.pos.row=2))
##Plot bifurcation diagram:
plot(bf, cex=0.15, main=NULL)
##go back to 1st row:
popViewport()
pushViewport(viewport(layout.pos.row=1))
##plot Lyap.exp. diagram:
le <- LyapunovExponents(m, par=1, var=0.3, par.min=2.5, par.max=4.0, time=500)
plot(le, main='Logistic map', xlab=NULL)
##add horizontal line to main plot area:
downViewport('plotArea')
grid.lines(c(2.5,4.0), c(0,0), default.units='native', gp=gpar(col='gray'))
##Go back to 'root' viewport
popViewport(0)

\dontshow{
m <- Model(exModelFile('logistic'))
bf <- Bifurcation(m, par=1, var=0.3, par.min=2.5, par.max=4.0, par.howMany=10, transient=10, max.period=10)
stopifnot(inherits(as.matrix(bf), 'matrix'))
}
}
