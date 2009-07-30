Basin <- function(model, parms, xlim, xres=100, ylim,
                  yres=100, attractorLimit, attractorIterations,
                  method=c("fast", "slow"), ntries, seed) {
  checkModelDiscrete(model, deparse(substitute(model)))
  parms <- .sanitizeNamedVector(parms, getModelParNames(model))
  checkModelParVar(model, parms, txt=deparse(substitute(model)))
  checkPositiveScalar(xres)
  checkPositiveScalar(yres)
  checkPositiveScalar(attractorLimit)
  checkPositiveScalar(attractorIterations)
  if(length(getModelVarNames(model))!=2)
    stop("'model' should have exactly 2 variables")
  method <- match.arg(method)
  ans <- list()
  ans$model <- model
  ans$attractorLimit <- attractorLimit
  ans$attractorIterations <- attractorIterations
  ans$method <- method
  if(method=="fast") {
    basin <- .Call("ridmc_basin_alloc", model$model, as.double(parms),
      as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
      as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
      as.integer(attractorLimit), as.integer(attractorIterations) , 
      PACKAGE='RiDMC')
    while(.Call("ridmc_basin_finished", basin, PACKAGE='RiDMC')==0)
      .Call("ridmc_basin_step", basin, PACKAGE='RiDMC')
    values <- .Call("ridmc_basin_getData", basin, PACKAGE='RiDMC')
  } else {
    if(missing(ntries))
      stop("'ntries' is mandatory with 'slow' method")
    checkPositiveScalar(ntries)
    ans$ntries <- as.integer(ntries)
    basin <- .Call("ridmc_basin_slow_alloc", model$model, as.double(parms),
      as.double(xlim[1]), as.double(xlim[2]), as.integer(xres),
      as.double(ylim[1]), as.double(ylim[2]), as.integer(yres),
      as.integer(attractorLimit), as.integer(attractorIterations), 
      as.integer(ntries), PACKAGE='RiDMC')
    if(!missing(seed))
      .Call("ridmc_basin_slow_setGslRngSeed", basin, 
        as.integer(seed), PACKAGE='RiDMC')
    while(.Call("ridmc_basin_slow_finished", basin, PACKAGE='RiDMC')==0)
      .Call("ridmc_basin_slow_step", basin, PACKAGE='RiDMC')
    values <- .Call("ridmc_basin_slow_getData", basin, PACKAGE='RiDMC')
  }
  values <- t(values)
  values <- values[rev(seq_len(nrow(values))),]
  ans$raster <- Raster(xlim=xlim, ylim=ylim, xres=xres, yres=yres,
                       data=values,
                       xName=getModelVarNames(model)[1],
                       yName=getModelVarNames(model)[2])
  class(ans) <- "idmc_basin"
  return(ans)
}
getBasinModel <- function(obj, ...)
  obj$model
as.matrix.idmc_basin <- function(x, ...)
  as.matrix(x$raster)
.getBasinAttractors <- function(obj, ...) UseMethod(".getBasinAttractors")
.getBasinAttractors.Raster <- function(obj, ...) {
  attrCodes <- .basinRasterAttractorLevels(obj)
  na <- length(attrCodes)
  ans <- list()
  for(i in seq_along(attrCodes)) { ##for each attractor
    ans[[as.character(attrCodes[i])]] <- raster2Pts(obj, value=attrCodes[i])
  }
  return(ans)
}
.getBasinAttractors.idmc_basin <- function(obj, ...) {
  .getBasinAttractors(obj$raster)
}

.getBasinLevels <- function(basin)
  sort(unique(as.vector(as.matrix(basin$raster))))

print.idmc_basin <- function(x, ...){
  mdl <- getBasinModel(x)
  cat('= iDMC basins of attraction =\n')
  cat('Model: ', getModelName(mdl), '\n')
  cat('transient: ', x$attractorLimit, '\n')
  cat('attractor iterations: ', x$attractorIterations, '\n')
  print(x$raster, showContents=FALSE)
  freqs <- table(as.vector(x$raster))
  freqs <- freqs * 100 / sum(freqs)
  freqs <- freqs[(as.numeric(names(freqs)) %% 2) == 1]
  labels <- makeBasinsLabels(x)
  names(freqs) <- labels[names(freqs)]
  cat('contents (%):\n')
  print(freqs)
}

.basinRasterAttractorLevels <- function(rasterObj) {
  allLevels <- sort(unique(as.vector(as.matrix(rasterObj))))
  allLevels[allLevels > 1 & ((allLevels %% 2) == 0)]
}

.basinRasterBasinLevels <- function(rasterObj) {
  allLevels <- sort(unique(as.vector(as.matrix(rasterObj))))
  allLevels[allLevels > 1 & ((allLevels %% 2) == 1)]
}

makeBasinsPalette <- function(rasterObj,
                              color.attractors=NULL,
                              color.basins=NULL,
                              color.infinity='black',
                              paletteFun=rainbow) {
  if(inherits(rasterObj, "idmc_basin"))
    rasterObj <- rasterObj$raster
  attrCodes <- .basinRasterAttractorLevels(rasterObj)
  basCodes <- .basinRasterBasinLevels(rasterObj)
  na <- length(attrCodes)
  nb <- length(basCodes)
  default.palette <- paletteFun(na*2)
  if(is.null(color.attractors))
    color.attractors <- default.palette[seq(1, by=2, length=na)]
  if(is.null(color.basins))
    color.basins <- default.palette[seq(2, by=2, length=nb)]
  if(length(color.attractors)<na)
    color.attractors <- c(color.attractors, default.palette[seq(1, by=2, length=na)])
  if(length(color.basins)<nb)
    color.basins <- c(color.basins, default.palette[seq(2, by=2, length=nb)])
  col <- numeric(0)
  if(any(1 == as.vector(as.matrix(rasterObj))))
    col["1"] <- color.infinity
  col[as.character(attrCodes)] <- color.attractors[seq_len(na)]
  col[as.character(basCodes)] <- color.basins[seq_len(nb)]
  return(col)
}

makeBasinsLabels <- function(rasterObj,
                             labels.attractors=NULL,
                             labels.basins=NULL,
                             label.infinity="infinity") {
  attrLevels <- .basinRasterAttractorLevels(rasterObj)
  basLevels <- .basinRasterBasinLevels(rasterObj)
  na <- length(attrLevels)
  nb <- length(basLevels)
  if(is.null(labels.attractors))
    labels.attractors <- paste('attractor', seq_len(na))
  if(is.null(labels.basins))
    labels.basins <- paste('basin', seq_len(na))
  if(length(labels.attractors)<na || length(labels.basins) < nb)
    stop('there are ', na, 'attractors and ', nb, 'basins to plot: not enough labels provided')
  labels <- character(0)
  labels["1"] <- label.infinity
  labels[as.character(attrLevels)] <- labels.attractors
  labels[as.character(basLevels)] <- labels.basins
  return(labels)
}

basinGrob <- function(idmc_basin,
                      rasterObj=idmc_basin$raster,
                      palette=makeBasinsPalette(idmc_basin),
                      labels.attractors=NULL, labels.basins=NULL,
                      label.infinity="infinity",
                      legend=TRUE,
                      attractorPoints=TRUE,
                      pch=16, cex=1,
                      main=getModelName(idmc_basin$model),
                      xlab=rasterXname(rasterObj),
                      ylab=rasterYname(rasterObj),
                      axes=TRUE, mar=NULL, bty=FALSE,
                      ...) {
  rG <- rasterGrob(rasterObj,
                   palette=palette,
                   labels=makeBasinsLabels(rasterObj,
                     labels.attractors=labels.attractors,
                     labels.basins=labels.basins,
                     label.infinity=label.infinity),
                   legend=legend,
                   main=main,
                   xlab=xlab, ylab=ylab,
                   axes=axes, mar=mar, bty=bty)
  if(attractorPoints) {
    attractors <- .getBasinAttractors(rasterObj)
    attractorLevels <- .basinRasterAttractorLevels(rasterObj)
    for(i in seq_along(attractors)) { ##for each attractor
      xx <- attractors[[i]]
      ptGr <- pointsGrob(xx[,1], xx[,2], pch=pch, size=unit(cex, 'char'),
                         name=paste("attractor", i),
                         default.unit='native',
                         gp=gpar(col=palette[as.character(attractorLevels[i])]),
                         vp=vpPath("plotLayout", "rootArea", "plotArea"))
      rG <- addGrob(rG, ptGr)
    }
  }
  gTree(palette=palette,
        labels.attractors=labels.attractors,
        labels.basins=labels.basins,
        label.infinity=label.infinity,
        attractorPoints=attractorPoints,
        pch=pch, cex=cex,
        children=gList(rG),
        ...,
        name="basinGrob", cl="basinGrob")
}

as.grob.idmc_basin <- function(x, ...) {
  return(basinGrob(x, ...))
}

plot.idmc_basin <- function(x, y, ..., add=FALSE) {
  pG <- as.grob(x, ...)
  if(!add)
    grid.newpage()
  grid.draw(pG)
  invisible(pG)
}
