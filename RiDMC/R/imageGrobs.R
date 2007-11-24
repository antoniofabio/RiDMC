#############
##Image map #
#############
mkImageRect <- function(nr, nc, colors, name='image', vp=NULL, gp=NULL) {
  xx <- seq_len(nc)/nc
  yy <- seq_len(nr)/nr
  right <- rep(xx, each=nr)
  top <- rep(yy, nc)
  cols <- colors
  if(is.null(gp))
    gp <- gpar(col=NA, fill=cols)
  rectGrob(x=right, y=top, width=1/nc, height=1/nr, just=c('right','top'),
    gp = gp, name=name, vp=vp)
}

mkIsoViewPort <- function(xlim, ylim) {
  vpStack(
    viewport(name='isoLay', layout=grid.layout(1,1, widths=diff(xlim), heights=diff(ylim), respect=TRUE)),
    viewport(name='isoVp', layout.pos.row=1, layout.pos.col=1, xscale=xlim, yscale=ylim, clip=FALSE)
  )
}

imageGrob <- function(values, colors, xlim=0:1, ylim=0:1, axes=FALSE, name=NULL, gp=NULL, vp=NULL){
  nc <- NCOL(values)
  nr <- NROW(values)
  values <- matrix(as.integer(values), nr, nc)
  rg <- mkImageRect(nr, nc, colors[values], vp=vpPath('isoLay','isoVp'))
  if(axes) {
    xAx <- xaxisGrob(at=pretty(xlim, n=10), label=TRUE, vp=vpPath('isoLay','isoVp'))
    yAx <- yaxisGrob(at=pretty(ylim, n=10), label=TRUE, vp=vpPath('isoLay','isoVp'))
    children <- gList(rg, rectGrob(vp=vpPath('isoLay','isoVp')), xAx, yAx)
  } else {
    children <- gList(rg, rectGrob(vp=vpPath('isoLay','isoVp')))
  }
  gTree(values=values, colors = colors, children=children, childrenvp = mkIsoViewPort(xlim, ylim),
    gp=gp, name=name, vp=vp, cl='imageGrob')
}
grid.imageGrob <- function(...)
  grid.draw(imageGrob(...))

##imageGrob + axes:
imagePlotGrob <- function(values, colors, xlim=0:1, ylim=0:1, axes=TRUE,
  name=NULL, gp=NULL, vp=NULL) {
  ig <- imageGrob(values, colors, xlim, ylim, axes=axes, name=name, gp=gp, vp=vpPath('plotVp','dataVp'))
  children <- gList(ig)
  if(axes) {
    childrenvp <- vpStack(plotViewport(margins=c(2,2,0,0), name='plotVp'), 
      dataViewport(xscale=xlim, yscale=ylim, name='dataVp'))
  } else {
    childrenvp <- vpStack(plotViewport(margins=c(0,0,0,0), name='plotVp'), 
      dataViewport(xscale=xlim, yscale=ylim, name='dataVp'))
  }
  gTree(values=values, colors = colors, 
    children=gList(ig), childrenvp = childrenvp,
    gp=gp, name=name, vp=vp, cl='imagePlotGrob')
}

##image grob for continuous variables:
imageScalePlotGrob <- function(values, breaks, colors, xlim=0:1, ylim=0:1, axes=TRUE,
  name=NULL, gp=NULL, vp=NULL) {
  values.orig <- values
  values <- array(as.numeric(cut(values, breaks=breaks)), dim(values))
  ig <- imagePlotGrob(values, colors, xlim, ylim, axes=axes, name=name, gp=gp)
  gTree(values=values.orig, breaks=breaks, colors = colors, 
    children=gList(ig), gp=gp, name=name, vp=vp, cl='imageScalePlotGrob')
}
grid.imageScalePlotGrob <- function(...)
  grid.draw(imageScalePlotGrob(...))

###########################
##Complete image map grob##
###########################
##Has optional axes and legend. Space is accomodated accordingly
imageMapGrob <- function(values, breaks, colors, xlim=0:1, ylim=0:1, axes=TRUE, legend=TRUE, name=NULL, gp=NULL, vp=NULL) {
  ig <- imageScalePlotGrob(values, breaks, colors, xlim=xlim, ylim=ylim, axes=axes, name='image', gp=gp, vp=vpPath('layoutVp','imageVp'))
  if(legend) {
    lg <- ribbonLegend(breaks, colors, vp=vpPath('layoutVp','legendVp'), name='legend')
    lg1 <- lg
    lg1$vp <- NULL
    ly <- grid.layout(1, 2, widths = unit(c(1,1), c('null','grobwidth'), list(NULL, lg1)), respect=TRUE)
    vp <- vpStack(
      viewport(layout=ly, name='layoutVp'),
      vpList(viewport(layout.pos.col=1, name='imageVp'), viewport(layout.pos.col=2, name='legendVp'))
    )
    children <- gList(ig, lg)
  } else {
    vp <- vpStack(viewport(name='layoutVp'), viewport(name='imageVp'))
    children <- gList(ig)
  }
  gTree(values=values, breaks=breaks, cols=cols, xlim=xlim, ylim=ylim, children=children, childrenvp=vp, cl='imageMapGrob')
}
grid.imageMap <- function(...)
  grid.draw(imageMapGrob(...))
