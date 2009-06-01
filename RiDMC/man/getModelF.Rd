\name{getModelF}
\alias{getModelF}
\title{getModelF}
\description{Get main model function 'f' as a plain R function}
\usage{
getModelF(model)
}
\arguments{
  \item{model}{an \code{idmc_model} object, as returned by
    \code{\link{Model}}}
}
\value{
  An R function which evaluates the main model function.
}
\details{
  Get main model function 'f' as a plain R function. I.e., the returned
  function has the same arguments as the LUA function, in the same order.
}
\seealso{
  \code{\link{Model}}
}
\examples{
model <- Model(exModelFile('logistic'))
F <- getModelF(model)
args(F)

F(mu = 0.5, x = 0.8)
}
\author{Antonio, Fabio Di Narzo}
\keyword{misc}
