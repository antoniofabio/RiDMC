\name{makeFunPower}
\alias{makeFunPower}
\title{makeFunPower}
\description{Make power function out of a single-argument function}
\usage{
makeFunPower(fun, k=2)
}
\arguments{
  \item{fun}{a function of 1 argument}
  \item{k}{number of desired iterates of the function}
}
\value{
  A function which evaluates the k-th iterate of the input function.
}
\seealso{
  \code{\link{getModelF}}, \code{\link{getModelMap}}
}
\examples{
square <- function(x) x*x
square3 <- makeFunPower(square, k=3)
square3(3) == square(square(square(3)))
#[1] TRUE
}
\author{Antonio, Fabio Di Narzo}
\keyword{misc}
