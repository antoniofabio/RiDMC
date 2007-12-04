\name{Lyapunov exponents map}
\alias{LyapunovExponentsMap}
\alias{plot.idmc_lexp_map}
\alias{print.idmc_lexp_map}
\title{
Lyapunov exponents map in parameter space for continuous and discrete-time dynamical systems
}
\usage{
LyapunovExponentsMap(idmc_model, par, var, time, eps,
  par.x = 1, par.x.range, par.x.howMany=100, par.y = 2, par.y.range, par.y.howMany=100,
  eps.zero=sqrt(.Machine$double.eps))
\method{print}{idmc_lexp_map}(x, ...)
\method{plot}{idmc_lexp_map}(x, y, colors, labels,
  main = getModelName(x$model),
  xlab, ylab, axes=TRUE, mar=NULL, legend=TRUE, ...)
}
\arguments{
\item{idmc_model}{idmc\_model object as returned by \code{\link{Model}}}
\item{par, var, time, eps}{see \code{\link{LyapunovExponents}}}
\item{par.x}{which parameter for x-axis variation?}
\item{par.x.range, par.x.howMany}{x-axis values}
\item{par.y}{which parameter for y-axis variation?}
\item{par.y.range, par.y.howMany}{y-axis values}
\item{eps.zero}{threshold for discriminating 0-value exponents}
 \item{x}{a \code{idmc_lexp_map} object}
 \item{y}{currently unused}
 \item{colors}{(optional) vector of colors (for each combination of lyap.exp. signs!)}
 \item{labels}{(optional) legend labels for each color}
 \item{main, xlab, ylab, mar, axes}{see \code{\link{plotGrob}}}
 \item{legend}{should a legend be plotted?}
 \item{...}{arguments to/from other methods}
}
\description{
Lyapunov exponents for continuous and discrete-time dynamical systems
}
\details{
Computes numerically Lyapunov exponents for continuous and discrete-time dynamical systems.

\code{lexp} computes Lyapunov exponents spectrum for a single set of parameter values, while 
\code{LyapunovExponents} computes Lyapunov exponents spectrum for a range of values
of one selected parameter. The output of \code{LyapunovExponents} can be directly plotted
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
model <- Model(exModelFile('lorenz'))
ly <- LyapunovExponentsMap(model, c(10, 28, 2.667), c(1.0, 2.0, 1.0), 12.0, 0.1,
  par.x = 2, par.x.range=c(27,29), par.x.howMany=10, par.y = 1, par.y.range=c(9,11), par.y.howMany=10)
plot(ly)
}
