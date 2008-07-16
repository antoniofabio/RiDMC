\name{makePlotGrobViewports}
\alias{makePlotGrobViewports}
\title{
Make viewports for a plotGrob object.
}
\usage{
makePlotGrobViewports(xlim, ylim, respect, mar)
}
\arguments{
 \item{xlim, ylim}{x and y ranges}
 \item{respect}{should aspect ratio be respected?}
 \item{mar}{plot margins in 'number of lines' (see \code{\link{par}})}
}
\description{
Make viewports for a \code{plotGrob} object.
}
\details{
Make viewports for a \code{plotGrob} object. This is used internally by the \code{plotGrob} constructor.
}
\value{
A \code{viewport} object.
}
\seealso{
\code{\link{plotGrob}}
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{hplot}
\keyword{aplot}
\keyword{internal}
