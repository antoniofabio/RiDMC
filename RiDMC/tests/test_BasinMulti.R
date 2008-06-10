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

############
##Plotting##
############

##Simple model
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 200, c(-2, 2), 200, 100, 100)
plot(b1)

##A more complex model:
b2 <- Basin(m2, 1.33, c(-1.2, 1.2), 300, c(-1.2, 1.2), 300, 1000, 1000)
plot(b2)
##A plot with no annotations:
plot(b2, axes=FALSE, xlab=NULL, ylab=NULL, main=NULL)

##Customizing
#A simple case with just 1 attractor:
plot(b1, color.attractors='blue', color.basins='green', color.infinity='white')
#Improve plot resolution, then add trajectory points:
b1 <- Basin(m1, c(1.42,0.3), c(-2,2), 500, c(-2, 2), 500, 100, 100)
plot(b1, legend=TRUE, attractorPoints=TRUE, pch=1, cex=1)
