library(RiDMC)
m <- Model(exModelFile('Economic/solow'))

##A list of different starting values, fixed parameters:
param <- c(s=0.2, alpha=0.5, n=0.01, g=0.01, delta=0.04)
initial.value <- list(k=4, k=13)
tr <- TrajectoryList(m, n=5, param, initial.value, time=1000, eps=0.2)
trG <- plot(tr)
trG <- plot(tr, type='p')
trG <- plot(tr, type='p', pch=16, gp=gpar(cex=0.1))
stopifnot(identical(trG, grid.get(trG$name)))
