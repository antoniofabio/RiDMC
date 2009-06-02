library(RiDMC)

pl <- matrix(c(1, 1, 2, 1, 2, 2, 1, 2),
             ncol=2,
             byrow=TRUE)
stopifnot(!polyContainsPt(pl, c(0.2, 0.2)))
stopifnot(!polyContainsPt(pl, c(0.6, 1.2)))
stopifnot(polyContainsPt(pl, c(1.6, 1.2)))
