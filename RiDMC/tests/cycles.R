##
##Periodic cycles scanning
##
library(RiDMC)

model <- Model(exModelFile('logistic'))
cyc <- cycles_find(model, 3.25, 0.5, 2, 1e-6)
cyc2 <- cyc
cyc2$pt <- as.matrix(cyc)[2,]

cyc
cyc2

stopifnot(!is.null(.cycleAlign(cyc2, cyc)))

.cycleAlign(cyc2, cyc)
.cycleAlign(cyc, cyc2)

stopifnot(.cycleCompare(cyc2, cyc))
stopifnot(.cycleCompare(cyc, cyc2))

cyc <- Cycles(model, par=3.25, period=2, eps=1e-4,
              varMin=0, varMax=1)
cyc
