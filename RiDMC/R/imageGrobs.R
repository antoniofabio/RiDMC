#############
##Image map #
#############
imageGrob <- function(colors, xlim=0:1, ylim=0:1, respect = FALSE, name=NULL, gp=NULL, vp=NULL) {
  nc <- NCOL(colors)
  nr <- NROW(colors)
  xx <- seq_len(nc)/nc
  yy <- seq_len(nr)/nr
  right <- rep(xx, each=nr)
  top <- rep(yy, nc)
  if(is.null(gp))
    gp <- gpar(col=NA, fill=as.vector(colors))
  cg <- contentsGrob(rectGrob(x=right, y=top, width=1/nc, height=1/nr, just=c('right','top'),
    gp=gp, name=name, vp=vp), xlim=xlim, ylim=ylim, respect=respect)
  ans <- extend(cg, 'image', colors=colors)
  return(ans)
}
editDetails.image <- function(x, specs){
  x <- imageGrob(specs$colors)
  .upgrade(x, specs)
}

imageScaleGrob <- function(values, breaks, palette, name=NULL, gp=NULL, vp=NULL) {
  colors <- array(palette[as.numeric(cut(values, breaks=breaks))], dim(values))
  extend(imageGrob(colors=colors, name=name, gp=gp, vp=vp), 'imageScale', values=values, breaks=breaks, palette=palette)
}
editDetails.imageScale <- function(x, specs){
  if(any(c('values','breaks','palette') %in% names(specs))) {
    values <- specs$values
    breaks <- specs$breaks
    palette <- specs$palette
    if(is.null(values))
      values <- getField(x,'values')
    if(is.null(breaks))
      breaks <- getField(x, 'breaks')
    if(is.null(palette))
      palette <- getField(x, 'palette')
    x <- imageScaleGrob(values=values, breaks=breaks, palette=palette)
  }
  .upgrade(x, specs)
}

###########################
##Complete image map grob##
###########################
##Has optional axes and legend. Space is accomodated accordingly
imageMapGrob <- function(values, breaks, palette, xlim=0:1, ylim=0:1, axes=TRUE,
                         legend=TRUE, respect=FALSE, name=NULL, gp=NULL, vp=NULL) {
  cg <- imageScaleGrob(values, breaks, palette)
  if(legend) {
    lg <- ribbonLegend(breaks, palette, n=10)
  } else {
    lg <- NULL
  }
  return(plotGrob(cg, axes=axes, mar=c(2,2,2,6), xlim=xlim, ylim=ylim,
                  respect=respect, legend=lg))
}
grid.imageMap <- function(...)
  grid.draw(imageMapGrob(...))

.defaultPalette <- function(n) {
  if(FALSE) {
    pp <- palette()
    if(length(pp) < n)
      pp[(length(pp)+1):n] <- "white"
    return(pp)
  }
  topo.colors(n)
}

makePalette <- function(values, howMany=7, paletteFun=topo.colors) {
  if(!inherits(values, "matrix"))
    values <- as.matrix(values)
  allValues <- table(values)
  topValues <- -sort(-allValues)[seq_len(howMany)]
  palette <- paletteFun(howMany)
  names(palette) <- names(topValues)
  if(!all(allValues %in% topValues))
    palette <- c(palette, other="white")
  return(palette)
}

############################################
##Indexed image plot, with optional legend##
############################################
rasterGrob <- function(raster, labels, palette,
                       xlab=rasterXname(raster),
                       ylab=rasterYname(raster),
                       axes=TRUE,
                       legend=FALSE,
                       ...,
                       name="raster", gp=NULL, vp=NULL) {
  stopifnot(inherits(raster, "Raster"))
  values <- as.matrix(raster)
  valuesTable <- as.character(sort(unique(as.vector(values))))
  if(missing(palette)) {
    palette <- .defaultPalette(length(valuesTable))
    names(palette) <- valuesTable
  }
  if(is.null(names(palette)))
    names(palette) <- valuesTable
  colors <- palette[as.character(as.vector(values))]
  dim(colors) <- dim(values)
  cg <- imageGrob(colors,
                  xlim = rasterXlim(raster), ylim = rasterYlim(raster),
                  name=name, gp=gp, vp=vp)
  if(missing(labels))
    labels <- valuesTable
  if(legend) {
    if(is.null(names(labels)))
      names(labels) <- valuesTable
    order.A <- valuesTable %in% names(palette)
    order.B <- !(names(palette) %in% valuesTable)
    order <- c(valuesTable[order.A], names(palette)[order.B])
    labels[names(palette)[order.B]] <- names(palette)[order.B]
    legendObj <- colorLegendGrob(palette[order],
                                 labels[order],
                                 name="rasterColorLegend")
  } else {
    legendObj <- NULL
  }
  return(extend(plotGrob(contents=cg, legendObj=legendObj,
                         xlab=xlab, ylab=ylab, axes=axes,
                         ...,
                         name=name, gp=gp, vp=vp),
                "rasterGrob",
                raster=raster, labels=labels, palette=palette,
                legend=legend))
}

editDetails.rasterGrob <- function(x, specs){
  labels <- getField(x, "labels")
  palette <- getField(x, "palette")
  if(!is.null(specs$palette)) {
    palette[names(specs$palette)] <- specs$palette
  }
  if(!is.null(specs$labels)) {
    labels[names(specs$labels)] <- specs$labels
  }
  legend <- specs$legend
  if(is.null(legend))
    legend <- getField(x, "legend")
  ans <- rasterGrob(getField(x, "raster"),
                    main=getField(x, "main"),
                    mar=getField(x, "mar"),
                    xlab=getField(x, "xlab"),
                    ylab=getField(x, "ylab"),
                    xlim=getField(x, "xlim"),
                    ylim=getField(x, "ylim"),
                    axes=getField(x, "axes"),
                    bty=getField(x, "bty"),
                    labels=labels,
                    palette=palette,
                    legend=legend)
  specs <- specs[setdiff(names(specs), c("labels", "palette", "legend"))]
  if(length(specs) > 0)
    ans <- editDetails.plotGrob(ans, specs)
  return(ans)
}
