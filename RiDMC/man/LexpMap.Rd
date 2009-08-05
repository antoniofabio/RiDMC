\name{Lyapunov exponents map}
\alias{LyapunovExponentsMap}
\alias{as.matrix.idmc_lexp_map}
\alias{plot.idmc_lexp_map}
\alias{print.idmc_lexp_map}
\title{
Lyapunov exponents map in parameter space for continuous and discrete-time dynamical systems
}
\usage{
LyapunovExponentsMap(idmc_model, par, var, time, eps,
  par.x, par.x.range, par.x.howMany=100, par.y, par.y.range, par.y.howMany=100,
  eps.zero=sqrt(.Machine$double.eps))
\method{print}{idmc_lexp_map}(x, ...)
\method{as.matrix}{idmc_lexp_map}(x, ...)
\method{plot}{idmc_lexp_map}(x, y,
  main = getModelName(x$model),
  legend=TRUE, palette, ...)
}
\arguments{
  \item{idmc_model}{idmc\_model object as returned by \code{\link{Model}}}
  \item{var, time, eps}{see \code{\link{LyapunovExponents}}}
  \item{par}{if \code{idmc\_model} has >2 parms, named vector of fixed
    values for the non-varying parameters}
  \item{par.x}{parameter name for x-axis variation}
  \item{par.x.range, par.x.howMany}{x-axis values}
  \item{par.y}{parameter name for y-axis variation}
  \item{par.y.range, par.y.howMany}{y-axis values}
  \item{eps.zero}{threshold for discriminating 0-value exponents}
  \item{x}{an \code{idmc_lexp_map} object}
  \item{y}{unused}
  \item{main}{see \code{\link{plotGrob}}}
  \item{legend}{should a legend be added?}
  \item{palette}{(optional) color palette}
  \item{...}{arguments to/from other methods}
}
\description{
Lyapunov exponents for continuous and discrete-time dynamical systems in parameters space
}
\details{
Computes numerically Lyapunov exponents for continuous and discrete-time dynamical systems
in a bi-dimensional parameters space.

The output of \code{LyapunovExponentsMap} can be directly plotted
with \code{plot} or converted to a regular R matrix with \code{as.matrix} for subsequent
manipulation.

With continuous time models, you should supply the integration step \code{eps} for numerical 
ODE solving. If not supplied, \code{getOption('ts.eps')} is used by default.

With discrete time models (maps), if not supplied, map jacobian is numerically approximated
by finite differencing.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}, \code{\link{Bifurcation}}
}
\examples{
##Consider the SHED discrete time model:
m <- Model(text='
name = "SCED"
description = "none"
type = "D"
parameters = {"phi0", "phi1"}
variables = {"d"}

function f(phi0, phi1, d)
       a=(1-phi0)
       b=(1-phi1)
       if (d <= phi0) then
               d1 = (2/phi0) * d * (1-(1/(2*phi0))*d)
       end
       if (d > phi0) then
               d1 = (1/(phi0*a))*d*(1-phi0^2*b-(1-phi0*b)*d)
       end
       return d1
end
')

ly <- LyapunovExponentsMap(m, var=0.8, time=100,
  par.x.range=c(0.01, 0.99), par.x.howMany=50,
  par.y.range=c(0.01, 0.99), par.y.howMany=50)

plot(ly)
##change colors:
plot(ly, palette=c('green', 'red', 'blue'))

\dontshow{
stopifnot(inherits(as.matrix(ly), 'matrix'))
}
}
