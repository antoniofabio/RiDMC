library(RiDMC)
model <- Model(exModelFile('lorenz'))
trajectory <- Trajectory(model,
	par=c(10, 28, 2.667), var=c(1.0, 2.0, 1.0),
	time=200, transient=100, eps=0.05)
trG <- plot(trajectory)
stopifnot(inherits(trG, 'grob'))

##Add a legend object
clg <- colorLegendGrob(colors=rainbow(3),
                       labels=letters[1:3])
trG <- plotGrob(as.grob(trajectory), legendObj=clg)
grid.draw(trG)

##Test editing
trG <- plot(trajectory)
grid.edit(trG$name, main="bau")
grid.edit(trG$name, xlab="a", ylab="b")
grid.edit(trG$name, axes=FALSE)
grid.edit(gPath(trG$name, "xy", "xy.lines"), gp=gpar(lwd=2, col="red"))
grid.edit(trG$name, legendObj=clg)

##Test color legend:
grid.newpage()
clg <- colorLegendGrob(rainbow(19), labels=letters[1:19], numColumns=2)
pushViewport(viewport(width=unit(4, "lines"), height=unit(4, "lines")))
grid.draw(clg)
grid.edit("colorLegend", grep=TRUE, numColumns=1)
grid.edit("colorLegend", grep=TRUE, numColumns=3)
grid.edit("colorLegend", grep=TRUE, numColumns=4)

##Test editing color legend in a plotGrob:
grid.newpage()
clg <- colorLegendGrob(colors=rainbow(7), labels=letters[1:7])
pG <- plotGrob(as.grob(trajectory), legendObj=clg)
grid.draw(pG)
grid.edit("plotGrob", grep=TRUE, mar=c(0,0,0,6))
grid.edit("colorLegend", grep=TRUE, numColumns=2)
