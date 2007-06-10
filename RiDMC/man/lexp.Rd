\name{Lyapunov exponents}
\alias{lexp}
\alias{lexp_ode}
\alias{LyapunovExponents}
\alias{as.matrix.lexp_diagram}
\alias{plot.lexp_diagram}
\title{
Lyapunov exponents for continuous and discrete-time dynamical systems
}
\usage{
lexp_ode(idmc_c_model, par, var, time, eps)
lexp(idmc_d_model, par, var, time)
LyapunovExponents(idmc_model, par, var, time, eps,
	which.par.variate=1, par.min, par.max, par.howMany=100)
\method{as.matrix}{lexp_diagram}(x, ...)
\method{plot}{lexp_diagram}(x,y, type='l', col, lty, main, xlab, ylab, ylim, ...)
}
\arguments{
\item{idmc_c_model}{continuous idmc\_model object as returned by \code{\link{Model}}}
\item{idmc_d_model}{discrete idmc\_model object as returned by \code{\link{Model}}}
\item{idmc_model}{idmc\_model object as returned by \code{\link{Model}}}
\item{par}{model parameters values}
\item{var}{model variables values}
\item{time}{time span}
\item{eps}{integration step}
\item{which.par.variate}{which parameter is to be variated}
\item{par.min, par.max, par.howMany}{specification of range of values for the selected parameter}
 \item{x}{a \code{lexp_diagram} object}
 \item{col, lty}{vector of line colours and line types (see \code{\link{par}}) }
 \item{type, main, xlab, ylab, ylim}{usual plot options}
 \item{y}{currently unused}
 \item{...}{arguments to and from other methods}
}
\description{
Lyapunov exponents for continuous and discrete-time dynamical systems
}
}
\details{
TODO
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
(ly <- lexp_ode(model, c(10, 28, 2.667), c(1.0, 2.0, 1.0), 100.0, 0.005))

ly <- LyapunovExponents(model, c(10, 28, 2.667), c(1.0, 2.0, 1.0), 
	200.0, 0.05, which.par=2, par.min=27, par.max=29)
plot(ly)
}

