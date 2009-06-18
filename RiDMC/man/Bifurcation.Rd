\name{Bifurcation}
\alias{Bifurcation}
\alias{as.matrix.idmc_bifurcation}
\alias{plot.idmc_bifurcation}
\alias{BifurcationMap}
\alias{as.matrix.idmc_bifurcation_map}
\alias{print.idmc_bifurcation_map}
\alias{plot.idmc_bifurcation_map}
\title{
Bifurcation diagram
}
\usage{
Bifurcation(idmc_model, which.var.store=1, par, var, 
	which.par.vary=1, par.min, par.max, par.howMany=100, 
	transient=100, max.period=50)

BifurcationMap(idmc_model,
  par, var,
  which.par.x=1, par.x.range, par.x.howMany=100,
  which.par.y=2, par.y.range, par.y.howMany=100,
  transient=100, max.period=50,
  eps=1e-3)
\method{as.matrix}{idmc_bifurcation}(x, ...)
\method{as.matrix}{idmc_bifurcation_map}(x, ...)
\method{plot}{idmc_bifurcation}(x, y, pch=16, cex=0.2,
  size = unit(cex, 'char'),
  main = getModelName(x$model),
  xlab = x$which.par,
  ylab = x$which.var,
  axes=TRUE, bty=TRUE, mar=NULL, add=FALSE, ...)
}
\method{plot}{idmc_bifurcation_map}(x, y, ...)
\arguments{
 \item{idmc_model}{a model object, as returned by \code{\link{Model}}}
 \item{which.var.store}{which variable is to be stored?}
 \item{par}{fixed model parameter values}
 \item{var}{starting values}
 \item{which.par.vary}{which parameter is to be varied?}
 \item{par.min, par.max, par.howMany}{specification of range of values for the selected parameter}
 \item{which.par.x, which.par.y}{horiz. and vertical axes parameters}
 \item{par.x.range, par.y.range}{varying parameters ranges}
 \item{par.x.howMany, par.y.howMany}{number of different \code{x} and \code{y}
  params. values}
 \item{transient}{transient length}
 \item{max.period}{max period for a cycle}
 \item{eps}{points proximity threshold}
 \item{x}{an \code{idmc_bifurcation} object}
 \item{main, xlab, ylab}{usual plot options (see \code{\link{plotGrob}})}
 \item{pch, cex}{see \code{\link{par}}}
 \item{size}{points size in an arbitrary grid \code{\link[grid]{unit}} (alternative to \code{cex})}
 \item{y}{currently unused}
 \item{axes, bty, mar}{see \code{\link{plotGrob}}}
 \item{add}{add to existing plot}
 \item{...}{arguments to and from other methods}
}
\description{
  Compute bifurcation diagram (\code{Bifurcation}) and bifurcation map
  (\code{BifurcationMap}).

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
plot(bf, cex=0.3)

#start a new grid plot
grid.newpage()
##Setup: 2 rows, 1 column:
pushViewport(viewport(layout=grid.layout(2, 1)))
##Go to second row:
pushViewport(viewport(layout.pos.row=2))
##Plot bifurcation diagram:
plot(bf, cex=0.15, main=NULL, add=TRUE)
##go back to 1st row:
popViewport()
pushViewport(viewport(layout.pos.row=1))
##plot Lyap.exp. diagram:
le <- LyapunovExponents(m, par=1, var=0.3, par.min=2.5, par.max=4.0, time=500)
plot(le, main='Logistic map', xlab=NULL, add=TRUE)
##add horizontal line to main plot area:
downViewport('plotArea')
grid.lines(c(2.5,4.0), c(0,0), default.units='native', gp=gpar(col='gray'))
##Go back to 'root' viewport
popViewport(0)

##
##Bifurcation map
##
m <- Model(text='
name = "multiplier accelerator model"
description = "Samuelson model with heterogeneous expectations, Jacobian available"
type = "D"
parameters = {"i", "b", "gamma", "mu1", "mu2", "k"}
variables = {"Y", "Z"}

function f(i, b, gamma, mu1, mu2, k, Y, Z)
	eq = i/(1-b)

	devY = (Y - eq)/eq
	devZ = (Z - eq)/eq

	e1Y = Y + mu1 * (Y - eq)
	e2Y = Y + mu2 * (eq - Y)

	e1Z = Z + mu1 * (Z - eq)
	e2Z = Z + mu2 * (eq - Z)

	wY = 1 / (1 + gamma * devY^2)
	wZ = 1 / (1 + gamma * devZ^2)

	Y1 = i + b*(1+k) * (wY * e1Y + (1-wY) * e2Y) - k*b*(wZ * e1Z + (1-wZ) * e2Z)
	Z1 = Y

	return  Y1, Z1
end
')
par <- c(b=0.8, gamma=10, mu2=0.5, i=1000)
k.range <- c(1, 2.3)
mu1.range <- c(0, 1.6)

##The following can take a little while to complete...
\dontrun{
bifMap <- BifurcationMap(m,
                         par=par,
                         var=c(Y=3500, Z=3500),
                         which.par.x='k',
                         par.x.range=k.range, par.x.howMany=50,
                         which.par.y='mu1',
                         par.y.range=mu1.range, par.y.howMany=50,
                         transient=5000,
                         max.period=28,
                         eps=0.1)
plot(bifMap)
}

\dontshow{
m <- Model(exModelFile('logistic'))
bf <- Bifurcation(m, par=1, var=0.3, par.min=2.5, par.max=4.0, par.howMany=10, transient=10, max.period=10)
stopifnot(inherits(as.matrix(bf), 'matrix'))
}
}
