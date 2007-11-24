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
    viewport(name='isoVp', layout.pos.row=1, layout.pos.col=1, xscale=xlim, yscale=ylim, clip=TRUE)
  )
}

imageGrob <- function(values, breaks, colors, xlim=0:1, ylim=0:1, name=NULL, gp=NULL, vp=NULL){
  nc <- NCOL(values)
  nr <- NROW(values)
  cols <- colors[as.numeric(cut(values, breaks, include.lowest=TRUE))]
  rg <- mkImageRect(nr, nc, cols, vp=vpPath('isoLay','isoVp'))
  gTree(values=values, breaks=breaks, colors = colors, children=gList(rg, rectGrob(vp=vpPath('isoLay','isoVp'))),
    childrenvp = mkIsoViewPort(xlim, ylim),
    gp=gp, name=name, vp=vp, cl='imageGrob')
}
grid.imageGrob <- function(...)
  grid.draw(imageGrob(...))

mkMainAndAxesVp <- function(xlim, ylim) {
  ly <- grid.layout(3,3, 
    widths = unit(c(2,1,1), c('lines','null','lines')), heights = unit(c(1,1,2), c('lines','null', 'lines')),
      respect=TRUE)
  vpStack(
    viewport(layout=ly, name='layVp'),
    viewport(layout.pos.col=2, layout.pos.row=2, name='mainVp', xscale=xlim, yscale=ylim))
}
imagePlotGrob <- function(values, breaks, colors, xlim=0:1, ylim=0:1, 
  xAxGrob=NULL, yAxGrob=NULL, name=NULL, gp=NULL, vp=NULL) {
  if(is.null(xAxGrob)) {
    xAxGrob <- xaxisGrob(at=pretty(xlim, n=10), label=TRUE, vp=vpPath('layVp','mainVp'))
  }
  if(is.null(yAxGrob)) {
    yAxGrob <- yaxisGrob(at=pretty(ylim, n=10), label=TRUE, vp=vpPath('layVp','mainVp'))
  }
  ig <- imageGrob(values, breaks, colors, xlim, ylim, name, gp, vp=vpPath('layVp','mainVp'))
  gTree(values=values, breaks=breaks, colors = colors, children=gList(ig, xAxGrob, yAxGrob),
    childrenvp = mkMainAndAxesVp(xlim, ylim), 
    gp=gp, name=name, vp=vp, cl='imagePlotGrob')
}
grid.imagePlotGrob <- function(...)
  grid.draw(imagePlotGrob(...))

###########################
##Complete image map grob##
###########################
##Has optional axes and legend. Space is accomodated accordingly
imageMapGrob <- function(values, breaks, cols, xlim=0:1, ylim=0:1, axes=TRUE, legend=TRUE, name=NULL, gp=NULL, vp=NULL) {
  if(axes) {
    ig <- imagePlotGrob(values, breaks, cols, xlim, ylim, name='image', gp=gp, vp=vpPath('layVp','imageVp'))
  } else {
    ig <- imageGrob(values, breaks, cols, xlim, ylim, name='image', gp=gp, vp=vpPath('layVp', 'imageVp'))
  }
  if(legend) {
    lg <- ribbonLegend(breaks, cols, vp=vpPath('layVp','legendVp'), name='legend')
    lg1 <- lg
    lg1$vp <- NULL
    ly <- grid.layout(1, 2, widths = unit(c(1,1), c('null','grobwidth'), list(NULL, lg1)), respect=TRUE)
    vp <- vpStack(
      viewport(layout=ly, name='layVp'),
      vpList(viewport(layout.pos.col=1, name='imageVp'), viewport(layout.pos.col=2, name='legendVp'))
    )
    children <- gList(ig, lg)
  } else {
    vp <- vpStack(viewport(name='layVp'), viewport(name='imageVp'))
    children <- gList(ig)
  }
  gTree(values=values, breaks=breaks, cols=cols, xlim=xlim, ylim=ylim, children=children, childrenvp=vp, cl='imageMapGrob')
}
grid.imageMap <- function(...)
  grid.draw(imageMapGrob(...))

#######################
##Discrete image map###
#######################

imageDiscreteGrob <- function(values, cols, xlim=0:1, ylim=0:1, name=NULL, gp=NULL, vp=NULL) {
  od <- dim(values)
  values <- as.numeric(factor(values))
  dim(values) <- od
  nr <- NROW(values)
  nc <- NCOL(values)
  img <- mkImageRect(nr, nc, colors=cols[values], vp=vpPath('isoLay', 'isoVp'))
  gTree(values=values, colors = colors, children=gList(img, rectGrob(vp=vpPath('isoLay','isoVp'))),
    childrenvp = mkIsoViewPort(xlim, ylim),
    gp=gp, name=name, vp=vp, cl='imageDiscreteGrob')
}

grid.imageDiscrete <- function(...)
  grid.draw(imageDiscreteGrob(...))

imageDiscretePlotGrob <- function(values, colors, xlim=0:1, ylim=0:1, axes=TRUE,
  name=NULL, gp=NULL, vp=NULL) {
  if(axes) {
    xAxGrob <- xaxisGrob(at=pretty(xlim, n=10), label=TRUE, vp=vpPath('layVp','mainVp'))
    yAxGrob <- yaxisGrob(at=pretty(ylim, n=10), label=TRUE, vp=vpPath('layVp','mainVp'))
    children <- gList(imageDiscreteGrob(values, colors, xlim, ylim, 
      name='imageRect', gp, vp=vpPath('layVp','mainVp')), xAxGrob, yAxGrob)
    childrenvp <- mkMainAndAxesVp(xlim, ylim)
  } else {
    children <- gList(imageDiscreteGrob(values, colors, xlim, ylim, 
      name='imageRect', gp, vp=vpPath('isoLay','isoVp')))
    childrenvp <- mkIsoViewPort(xlim, ylim)
  }
  gTree(values=values, colors = colors, children=children,
    childrenvp = childrenvp, gp=gp, name=name, vp=vp, cl='imagePlotGrob')
}
grid.imageDiscretePlotGrob <- function(...)
  grid.draw(imageDiscretePlotGrob(...))
