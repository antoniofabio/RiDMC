\name{colorLegendGrob}
\alias{colorLegendGrob}
\title{
  A classic color legend
}
\usage{
colorLegendGrob(colors, labels,
  x=unit(0,'npc'), y=unit(0, 'npc'),
  numColumns=1,
  name=NULL, gp=NULL, vp=NULL)
}
\arguments{
  \item{colors}{vector of colors}
  \item{labels}{vector of labels}
  \item{x, y}{top-left corner legend position}
  \item{numColumns}{the legend will span that many columns}
  \item{name, gp, vp}{see \code{\link{grob}}}
}
\description{
A classic color legend graphical object.
}
\value{
A \code{colorLegendGrob} object.
}
\seealso{
\code{\link{plotGrob}}
}
\examples{
clg <- colorLegendGrob(colors=rainbow(3), labels=letters[1:3])
pG <- plotGrob(pointsGrob(), mar=c(0,0,0,8), legendObj=clg)
grid.newpage()
grid.draw(pG)
##edit the legend afterwards: change labels
grid.edit('colorLegend', grep=TRUE,
  labels=c('first', 'second', 'third'))
##edit the legend afterwards: change number of columns
grid.edit('colorLegend', grep=TRUE, numColumns=2)
}
\author{
Antonio, Fabio Di Narzo
}
\keyword{hplot}
\keyword{aplot}
\keyword{internal}
