\name{Trajectory}
\alias{Trajectory}
\alias{CTrajectory}
\alias{DTrajectory}
\alias{setTrajectorySeed}
\alias{print.idmc_ctrajectory}
\alias{print.idmc_dtrajectory}
\alias{as.matrix.idmc_trajectory}
\alias{stepTrajectory}
\alias{as.ts.idmc_trajectory}
\alias{plot.idmc_trajectory}
\title{
Model trajectories
}
\usage{
CTrajectory(idmc_model, par, var, eps, integrator=0, nsteps=1, transient=0, seed)
DTrajectory(idmc_model, par, var, nsteps=1, transient=0, seed)
stepTrajectory(idmc_trajectory, nsteps=1)
setTrajectorySeed(idmc_trajectory, seed)
\method{print}{idmc_ctrajectory}(x, ...)
\method{print}{idmc_dtrajectory}(x, ...)
\method{as.matrix}{idmc_trajectory}(x, ...)
\method{as.ts}{idmc_trajectory}(x, ...)
\method{plot}{idmc_trajectory}(x, y, vars=1:2, type='l', main, xlab, ylab, ...)
}
\arguments{
 \item{idmc_model}{a model object, as returned by \code{\link{Model}}}
 \item{idmc_ctrajectory}{an already created \code{idmc_ctrajectory} object}
 \item{idmc_trajectory}{an already created \code{idmc_trajectory} object}
 \item{seed}{RNG seed}
 \item{par, var}{model parameters values and starting point value}
 \item{eps}{integration step}
 \item{integrator}{an integer code between 0 and 9 indicating what integrator to use}
 \item{nsteps}{number of steps (after transient)}
 \item{transient}{transient length}
 \item{x}{an \code{idmc_trajectory} object}
 \item{vars}{variables to be plotted}
 \item{type, main, xlab, ylab}{usual plot options}
 \item{y}{currently unused}
 \item{...}{arguments to and from other methods}
}
\description{
Computes continuous and discrete trajectories from given model (\code{idmc_model}),
parameters (\code{par}) and starting values (\code{var}).
}
\details{
Computes continuous and discrete trajectories from given model (\code{idmc_model}),
parameters (\code{par}) and starting values (\code{var}).

With \code{stepTrajectory} you can enlarge an already-existing trajectory.

The trajectory object can be directly plotted with the standard \code{plot} function,
converted to a vertical numerical matrix, or converted to a regular time series object.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}
}
\examples{
##Load model:
model <- Model(exModelFile('lorenz'))
##Compute trajectory:
trajectory <- CTrajectory(model, 
	c(10, 28, 2.667), c(1.0, 2.0, 1.0), 
	0.005, 0, nsteps=20000, transient=1000 )
##See basic informations:
trajectory
##Plot it:
plot(trajectory)
##Re-plot as regular multivariate time series:
plot(as.ts(trajectory))

##Now a bi-variate random walk:
text <- readLines(textConnection(
'name = "rndWalk"
description = "none"
type = "D"
parameters = {"a"}
variables = {"x", "y"}

function f(a, x, y)
        x1 = x + a * rnorm()
        y1 = y + a * rnorm()
        return x1, y1
end
'))
m <- Model(text=text)
tr <- DTrajectory(m, 0.5, c(0,0), 100, seed=123)
plot(tr)
}
