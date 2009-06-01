\name{Domain}
\alias{Domain}
\alias{openRectDomain}
\title{
Basic domain handling
}
\usage{
openRectDomain(varnames, mins, maxs)
}
\arguments{
  \item{varnames}{variables names}
  \item{mins}{vector of lower bounds (perhaps \code{-Inf})}
  \item{maxs}{vector of higher bounds (perhaps \code{Inf})}
}
\description{
  Basic domain handling.
}
\details{
  Basic domain handling.

  \code{openRectDomain} makes an open, rectangular domain.
}
\value{
  A list of 2 items: a \code{sample} function and an \code{inside} function.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ utilities }
\examples{
dd <- openRectDomain(c("x", "y"), rep(-Inf, 2), rep(Inf, 2))
set.seed(12345)
ss <- dd$sample(1000)
stopifnot(all(apply(ss, 1, dd$inside)))

dd <- openRectDomain(c("x", "y"), rep(-Inf, 2), rep(-3, 2))
set.seed(12345)
ss <- dd$sample(1000)
stopifnot(all(apply(ss, 1, dd$inside)))

dd <- openRectDomain(c("x", "y"), rep(-Inf, 2), rep(-3, 2))
set.seed(12345)
ss <- dd$sample(1000)
stopifnot(all(apply(ss, 1, dd$inside)))

dd <- openRectDomain(c("x", "y"), rep(3, 2), rep(Inf, 2))
set.seed(12345)
ss <- dd$sample(1000)
stopifnot(all(apply(ss, 1, dd$inside)))

dd <- openRectDomain(c("x", "y"), rep(3, 2), rep(4, 2))
set.seed(12345)
ss <- dd$sample(1000)
stopifnot(all(apply(ss, 1, dd$inside)))

dd <- openRectDomain(c("x", "y", "z", "w"), c(-Inf, -1, 1, -Inf), c(-1, Inf, 2, Inf))
set.seed(12345)
ss <- dd$sample(1000)
stopifnot(all(apply(ss, 1, dd$inside)))
}
