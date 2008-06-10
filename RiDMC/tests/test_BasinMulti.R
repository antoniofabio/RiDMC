source('init.R')

##A simple model:
m1 <- Model(exModelFile('henon'))
b1 <- BasinMulti(m1, c(1.42,0.3), c(-2,2), 20, c(-2, 2), 20, 100, 100)
b1

##A more complex model:
m2 <- Model(exModelFile('cremona'))
b2 <- Basin(m2, 1.33, c(-1.2, 1.2), 30, c(-1.2, 1.2), 30, 1000, 1000)
b2

##Methods:
mat <- as.matrix(b1)
stopifnot(inherits(mat, 'matrix'))
stopifnot(all(dim(mat) == c(20, 20)))
