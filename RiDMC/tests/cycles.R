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

cyc <- Cycles(model, 3.25, as.list(seq(0.01, 0.99, length=100)), 2, 1e-4)
cyc
