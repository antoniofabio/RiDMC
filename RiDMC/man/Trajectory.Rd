\name{Trajectory}
\alias{Trajectory}
\alias{setTrajectorySeed}
\alias{as.matrix.idmc_trajectory}
\alias{stepTrajectory}
\alias{as.ts.idmc_trajectory}
\alias{plot.idmc_trajectory}
\title{
Model trajectories
}
\usage{
Trajectory(idmc_model, par, var, time=1, transient=0, seed, eps=getOption("ts.eps"), integrator=2)
stepTrajectory(idmc_trajectory, time=1)
setTrajectorySeed(idmc_trajectory, seed)
\method{as.matrix}{idmc_trajectory}(x, ...)
\method{as.ts}{idmc_trajectory}(x, ...)
\method{plot}{idmc_trajectory}(x, y, vars=1:2,
  type=if(getModelType(x$model) == "C") 'l' else 'p',
  main = getModelName(getTrajectoryModel(x)), xlab, ylab,
  mar = NULL, axes=TRUE, bty=TRUE, add=FALSE, ...)
}
\arguments{
 \item{idmc_model}{a model object, as returned by \code{\link{Model}}}
 \item{par, var}{model parameters values and starting point value, to be
   specified as named vectors}
 \item{time}{time span after transient (in time units)}
 \item{transient}{transient length (in time units)}
 \item{seed}{RNG seed (optional)}
 \item{eps}{integration step (ignored for discrete time systems)}
 \item{integrator}{an integer code between 0 and 8 indicating what integrator to use (see details)}
 \item{idmc_trajectory}{an already created \code{idmc_trajectory} object}
 \item{x}{an \code{idmc_trajectory} object}
 \item{y}{currently unused}
 \item{vars}{variables to be plotted}
 \item{type, main, xlab, ylab}{usual plot options}
 \item{mar, axes, bty}{see \code{\link{plotGrob}}}
 \item{add}{add to existing plot}
 \item{...}{arguments to and from other methods.
   For \code{plot.idmc_trajectory}, arguments to \code{\link[grid]{gpar}}}
}
\description{
Computes continuous and discrete trajectories from given model (\code{idmc_model}),
parameters (\code{par}) and starting values (\code{var}).
}
\details{
Computes continuous and discrete trajectories from given model (\code{idmc_model}),
parameters (\code{par}) and starting values (\code{var}).
Only these 3 arguments are mandatory in \code{Trajectory}.

With \code{stepTrajectory} you can extend an already computed trajectory.

The trajectory object can be directly plotted with the standard \code{plot} function,
converted to a vertical numerical matrix, or converted to a regular time series object.

If the \code{idmc_model} object has continuous time (ODE), you can specify the last 2 arguments:
the integration step \code{eps} and the ODE numerical integrator. The following integrators are available:

\tabular{rl}{
0 \tab embedded Runge-Kutta (2, 3) method \cr
1 \tab 4th order (classical) Runge-Kutta \cr
2 \tab embedded Runge-Kutta-Fehlberg (4, 5) method \cr
3 \tab embedded Runge-Kutta Cash-Karp (4, 5) method \cr
4 \tab embedded Runge-Kutta Prince-Dormand (8,9) method \cr
5 \tab implicit 2nd order Runge-Kutta at Gaussian points \cr
6 \tab implicit 4th order Runge-Kutta at Gaussian points \cr
7 \tab M=1 implicit Gear method \cr
8 \tab M=2 implicit Gear method
}

}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{Model}}, \code{\link{TrajectoryList}}
}
\examples{
##Load model:
model <- Model(exModelFile('lorenz'))
##Compute trajectory:
trajectory <- Trajectory(model, 
	par=c(10, 28, 2.667), var=c(1.0, 2.0, 1.0), 
	time=200, transient=100, eps=0.005 )
##See basic informations:
trajectory
##Plot it:
plot(trajectory)
##A trajectory object can be converted into a regular time series:
trajectory.ts <- as.ts(trajectory)
plot(trajectory.ts)
##or into a 'vertical' matrix:
trajectory.matrix <- as.matrix(trajectory)
dim(trajectory.matrix)

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
tr <- Trajectory(m, 0.5, c(0,0), time=100, seed=123)
plot(tr)
plot(as.ts(tr))
}
