##Test raster grob
library(RiDMC)
rr <- Raster(0:1, 0:1,
             xres=5, yres=5,
             data=matrix(sample(5, size=25, repl=TRUE), 5))
pg <- rasterGrob(rr, legend=TRUE)
grid.newpage()
grid.draw(pg)
grid.edit("raster", palette=rainbow(5))
grid.edit("raster", legend=FALSE)
grid.edit("raster", legend=TRUE)
labels <- pg$labels
labels["27"] <- "mah"
grid.edit("raster", labels=labels)
pg <- rasterGrob(rr, legend=TRUE, mar=c(4,4,2,2))
grid.newpage()
grid.draw(pg)
grid.edit("raster", mar=c(4,4,0,2))
