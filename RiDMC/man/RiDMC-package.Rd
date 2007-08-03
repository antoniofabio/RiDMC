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
\code{\link{Model}} for a starting point.
}
\examples{
##There are many example models to start with
exModelFile()

##Load and init an univariate discrete time model
model <- Model(exModelFile('logistic'))
summary(model)

##Compute trajectory
tr <- Trajectory(model, par = 3.9, var = 0.6, time = 20)
tr
plot(tr)
}
