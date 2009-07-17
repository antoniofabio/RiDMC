\name{Model}
\alias{Model}
\alias{print.idmc_model}
\alias{summary.idmc_model}
\alias{print.summary.idmc_model}
\title{
Load a model
}
\usage{
Model(filename=NULL, text = readLines(filename))
\method{summary}{idmc_model}(object, ...)
}
\arguments{
\item{filename}{file name to be opened}
\item{text}{char vector containing lua model specification}
\item{object}{\code{idmc_model} object}
 \item{...}{arguments to and from other methods}
}
\description{
Loads a new model from its LUA description
}
\value{
An \code{idmc_model} object, which can be printed, summarized and used as argument
in some other package functions.
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{ misc }
\seealso{
\code{\link{exModelFile}}, \code{\link{Basin}}, \code{\link{Bifurcation}}, \code{\link{cycles}}, 
\code{\link{Trajectory}}, \code{\link{LyapunovExponents}}
}
\examples{
model <- Model(exModelFile('logistic'))
model
summary(model)

##The use of 'exModelFile' is implicit if the filename does not have a
# '.lua' extension, and the file cannot be found in the current working
# directory:
Model('logistic')
}
