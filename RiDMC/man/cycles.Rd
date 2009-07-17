\name{cycles}
\alias{Cycles}
\alias{cycles}
\alias{cycles_find}
\alias{as.matrix.periodic_cycle}
\alias{print.periodic_cycle}
\alias{print.idmc_cycles}
\alias{plot.idmc_cycles}
\title{
Periodic cycles
}
\usage{
cycles_find(idmc_model, par, var, period=1,
   eps=sqrt(.Machine$double.eps), max.iter=100)

Cycles(idmc_model, par, period=1,
       eps=sqrt(.Machine$double.eps),
       ntries=100,
       max.iter=100,
       varMin=-Inf, varMax=Inf, varList)

\method{as.matrix}{periodic_cycle}(x, ...)
\method{print}{periodic_cycle}(x, ...)
\method{print}{idmc_cycles}(x, ...)
\method{plot}{idmc_cycles}(x, y, legend=TRUE, ...)
}

\arguments{
  \item{idmc_model}{idmc\_model object as returned by
    \code{\link{Model}}}
  \item{par}{model parameters values}
  \item{var}{starting point}
  \item{period}{cycle period}
  \item{eps}{error criterion}
  \item{max.iter}{max number of iterations}
  \item{ntries}{number of starting points to try}
  \item{varMin, varMax}{model variables ranges}
  \item{varList}{optional: explicit 'vertical' matrix of starting points to try}
  \item{x}{an idmc\_cycle object}
  \item{y}{ignored}
  \item{legend}{should a legend be added to the plot?}
  \item{...}{arguments to and from other methods}
}

\description{ Find periodic cycles by Newton method }

\details{ Find periodic cycles by Newton method. A list of multiple
                   starting points can be specified.

Convergence criterion is \code{eps} with max-norm.
}
\value{
  \code{find_cycles} returns the found cycle, or \code{NULL} if none
  found.

  \code{Cycles} returns the list of found cycles (if any).
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}
}
\examples{
model <- Model('logistic')
Cycles(model, 3.25, period=2, varMin=0, varMax=1)

model <- Model('henon')
cyc <- Cycles(model, par=c(a=1, b=0.3), period=4, varMin=-2, varMax=2)
plot(cyc)

##Customizing the plot
plot(cyc, pch=16, legend=FALSE,
     xlim=c(-2, 2), ylim=c(-2, 2),
     main=NULL, mar=c(4,4,2,2))
grid.edit("xy.points", gp=gpar(col="black", cex=0.6))
}
