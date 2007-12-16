\name{plotGrob}
\alias{plotGrob}
\title{
Stadard annotated plot graphical object
}
\usage{
plotGrob(contents=NULL, main=NULL, xlab=NULL, ylab=NULL, 
  xlim=NULL, ylim=NULL, axes=FALSE, bty=TRUE, respect=NULL, mar=NULL, name=NULL, gp=NULL, vp=NULL)
}
\arguments{
 \item{contents}{main contents graphical object}
 \item{main}{plot title (set to NULL for disabling)}
 \item{xlab, ylab}{x and y axes labels (set to NULL for disabling)}
 \item{xlim, ylim}{x and y ranges}
 \item{axes}{should axes be showed? (TRUE or FALSE)}
 \item{bty}{draw box around plot contents}
 \item{respect}{should aspect ratio be respected?}
 \item{mar}{plot margins in 'number of lines' (see \code{\link{par}})}
 \item{name, gp, vp}{see \code{\link[grid]{grob}}}
}
\description{
Stadard annotated plot graphical object
}
\details{
Stadard annotated plot graphical object
}
\value{
A \code{plotGrob} object
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{hplot}
\keyword{aplot}
