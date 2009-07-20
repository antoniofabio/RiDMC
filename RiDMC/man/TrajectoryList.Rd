\name{TrajectoryList}
\alias{TrajectoryList}
\alias{plot.idmc_trajectoryList}
\title{
List of trajectories from one model
}
\usage{
TrajectoryList(idmc_model, n=2, par, var, time=1, transient=0,
  seed, eps=getOption("ts.eps"), integrator=2)
\method{plot}{idmc_trajectoryList}(x, y, vars=1:2,
  type=if(getModelType(x[[1]]$model) == "C") 'l' else 'p',
  colors, pch=1, gp=gpar(),
  main = getModelName(getTrajectoryModel(x[[1]])), xlab, ylab,
  mar = NULL, axes=TRUE, bty=TRUE, legend=FALSE, labels, add=FALSE, ...)
}
\arguments{
 \item{idmc_model, time, transient, seed, eps, integrator}{see \code{\link{Trajectory}}}
 \item{n}{how many different trajectories?}
 \item{par, var}{model parameters values and starting point value (see details on how to specify multiple values)}
 \item{x}{an \code{idmc_trajectoryList} object}
 \item{y}{currently unused}
 \item{vars}{variables to be plotted}
 \item{type, main, xlab, ylab}{usual plot options}
 \item{colors}{vector of trajectory colors, one per trajectory}
 \item{pch}{vector \code{pch} values (see
   \code{\link{plot.idmc_trajectory}}), one per trajectory}
 \item{gp}{graphical parameters common to all the trajectories}
 \item{mar, axes, bty}{see \code{\link{plotGrob}}}
 \item{legend}{should a legend be showed?}
 \item{labels}{legend labels}
 \item{add}{add to existing plot}
 \item{...}{arguments to be passed to \code{\link{plotGrob}}}
}
\description{
Computes (and plots) a list of continuous and discrete trajectories from given model (\code{idmc_model}),
parameters (\code{par}) and starting values (\code{var}).
}
\details{
The argument \code{n} controls how many trajectory are to be specified.

You can specify an aritmetic sequence of different parameters/starting point values
by passing them as a list: \code{list(initial.values, final.values)}. See examples.

The \code{plot} functions accepts the usual plotting options, plus a vector of colors
for the different trajectories.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}, \code{\link{Trajectory}}
}
\examples{
m <- Model(exModelFile('Economic/solow'))

##A list of different starting values, fixed parameters:
param <- c(s=0.2, alpha=0.5, n=0.01, g=0.01, delta=0.04)
initial.value <- list(k=4, k=13)
tr <- TrajectoryList(m, n=5, param, initial.value, time=1000, eps=0.2)
plot(tr)

##A list of different parameters, fixed starting values:
value <- 4
initial.param <- c(s=0.2, alpha=0.5, n=0.01, g=0.01, delta=0.04)
final.param <- c(s=0.2, alpha=0.5, n=0.01, g=0.01, delta=0.04)
final.param['alpha'] <- 0.8
tr <- TrajectoryList(m, n=5, list(initial.param, final.param), value, time=1000, eps=0.2, seed=1234)
plot(tr)
}
