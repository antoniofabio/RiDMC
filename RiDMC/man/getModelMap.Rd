\name{getModelMap}
\alias{getModelMap}
\title{getModelMap}
\description{Get main model function 'f' with fixed parameters values}
\usage{
getModelMap(model, par)
}
\arguments{
  \item{model}{an \code{idmc_model} object, as returned by
    \code{\link{Model}}}
  \item{par}{(fixed) parameter values}
}
\value{
  An R function which evaluates the main model function.
}
\details{
  Get main model function 'f' as a plain R function with fixed parameter
  values.
}
\seealso{
  \code{\link{Model}}, \code{\link{getModelF}}
}
\examples{
model <- Model(exModelFile('logistic'))
F <- getModelMap(model, par=c(mu=0.5))
args(F)

F(0.8)
}
\author{Antonio, Fabio Di Narzo}
\keyword{misc}
