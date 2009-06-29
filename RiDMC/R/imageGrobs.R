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
