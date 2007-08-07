\name{RiDMC-package}
\alias{RiDMC-package}
\alias{RiDMC}
\docType{package}
\title{
RiDMC: the iDMC->R interface
}
\description{
RiDMC: the iDMC->R interface
}
\details{
\code{RiDMC} is an R interface to the \code{idmclib} library for the 
numerical analysis of dynamical systems.
}
\author{
Antonio, Fabio Di Narzo

Maintainer: Antonio, Fabio Di Narzo <antonio.fabio@gmail.com>
}
\keyword{ package }
\seealso{
\code{\link{Model}}, \code{\link{Trajectory}}, \code{\link{Basin}},
\code{\link{LyapunovExponents}}, \code{\link{Bifurcation}}, \code{\link{cycles_find}}
}
\examples{
##There are many sample models to start with
exModelFile()

##Load and init an univariate discrete time model
model <- Model(exModelFile('logistic'))
summary(model)
##Compute trajectory
tr <- Trajectory(model, par = 3.9, var = 0.6, time = 20)
tr
plot(tr)

##Load and init a bivariate discrete time model
model <- Model(exModelFile('cremona'))
##Compute basins of attraction
basin <- Basin(model, 1.33, c(-1.2, 1.2), 300, 
  c(-1.2, 1.2), 300, 1000, 1000,
  method='slow', ntries=1000, seed=123)
plot(basin)

##Load the logistic map
model <- Model(exModelFile('logistic'))
##Draw bifurcation plot
bifurcation <- Bifurcation(model, par=1, var=0.3, par.min=2.5, par.max=4.0)
plot(bifurcation, cex=0.5)
##Compute Lyapunov exponents
le <- LyapunovExponents(model, par=1, var=0.3, par.min=2.5, par.max=4.0, time=500)
plot(le, col="red")
abline(h=0, col="gray")
##Find period-2 cycles
cycles_find(model, 3.25, 0.5, 2, 1e-6) #stable cycle (eigval modulus < 1)
cycles_find(model, 3.68, 0.5, 2, 1e-6) #unstable cycle (eigval modulus > 1)
}
