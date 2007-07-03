\name{cycles}
\alias{cycles}
\alias{cycles_find}
\title{
Periodic cycles
}
\usage{
cycles_find(idmc_model, par, var, period, eps, max.iter=100)
}
\arguments{
\item{idmc_model}{idmc\_model object as returned by \code{\link{Model}}}
\item{par}{model parameters values}
\item{var}{starting point}
\item{period}{cycle period}
\item{eps}{error criterion}
\item{max.iter}{max number of iterations}
}
\description{
Find periodic cycles by Newton method
}
\details{
Find periodic cycles by Newton method.

Convergence criterion is \code{eps} with max-norm.
}
\value{
	If a valid cycle is found, a list with components:
	\item{result}{one sample cycle point}
	\item{eigvals}{modulus of jacobian eigenvalues associated with the cycle},
	\code{NULL} otherwise.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}
}
\examples{
model <- Model(exModelFile('logistic'))
cycles_find(model, 3.25, 0.5, 2, 1e-6)
}
