\name{Lyapunov exponents}
\alias{lexp}
\alias{LyapunovExponents}
\alias{as.matrix.idmc_lexp_diagram}
\alias{plot.idmc_lexp_diagram}
\alias{print.idmc_lexp_diagram}
\title{
Lyapunov exponents for continuous and discrete-time dynamical systems
}
\usage{
lexp(idmc_model, par, var, time, eps)
LyapunovExponents(idmc_model, par, var, time, eps,
	which.par.vary=1, par.min, par.max, par.howMany=100)
\method{as.matrix}{idmc_lexp_diagram}(x, ...)
\method{print}{idmc_lexp_diagram}(x, ...)
\method{plot}{idmc_lexp_diagram}(x, y, col, lty,
  main = getModelName(x$model),
  xlab = x$which.par,
  ylab = 'Lyapunov exponent',
	xlim = NULL,
  ylim = NULL,
  mar = NULL,
  axes=TRUE,
  bty=TRUE,
  add=FALSE,
  ...)
}
\arguments{
\item{idmc_model}{idmc\_model object as returned by \code{\link{Model}}}
\item{par}{model parameters values}
\item{var}{model variables values}
\item{time}{time span}
\item{eps}{integration step (ignored for discrete time models)}
\item{which.par.vary}{which parameter is to be varied?}
\item{par.min, par.max, par.howMany}{specification of range of values for the selected parameter}
 \item{x}{a \code{idmc_lexp_diagram} object}
 \item{y}{currently unused}
 \item{col, lty}{vector of line colours and line types (see \code{\link{par}}) }
 \item{main, xlab, ylab, xlim, ylim}{usual plot options}
 \item{mar, axes, bty}{see \code{\link{plotGrob}}}
 \item{add}{add to existing plot}
 \item{...}{further \code{\link[grid]{gpar}} lines options}
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
\code{\link{Model}}, \code{\link{Bifurcation}}, \code{\link{LyapunovExponentsMap}}
}
\examples{
model <- Model(exModelFile('lorenz'))
(ly <- lexp(model, c(10, 28, 2.667), c(1.0, 2.0, 1.0), 100.0, 0.005))

ly <- LyapunovExponents(model, c(10, 28, 2.667), c(1.0, 2.0, 1.0), 
	200.0, 0.05, which.par=2, par.min=27, par.max=29)
plot(ly)
}
