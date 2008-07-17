library(RiDMC)
model <- Model(exModelFile('lorenz'))
trajectory <- Trajectory(model,
	par=c(10, 28, 2.667), var=c(1.0, 2.0, 1.0),
	time=200, transient=100, eps=0.05 )
trG <- plot(trajectory)
grid.edit(trG$name, main="bau")
grid.edit(trG$name, xlab="a", ylab="b")
grid.edit(trG$name, axes=FALSE)
grid.edit(gPath(trG$name, "xy", "xy.lines"), gp=gpar(lwd=2, col="red"))
