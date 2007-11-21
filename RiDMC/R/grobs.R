##
##Definition of grid graphical classes
##

##x/y graph
xyGrob <- function(x, y, type='l', name=NULL, gp=NULL, vp=NULL) {
  lgr <- linesGrob(x, y, name=paste(name, 'lines', sep='.'), default.units='native')
  pgr <- pointsGrob(x, y, name=paste(name, 'points', sep='.'), default.units='native')
  if(type=='l')
    comps <- list(lgr)
  else if(type=='p')
    comps <- list(pgr)
  else if (type=='b')
    comps <- list(lgr, pgr)
  xlim <- range(x, na.rm=TRUE)
  ylim <- range(y, na.rm=TRUE)
  if(is.null(vp))
    vp <- viewport(xscale=xlim, yscale=ylim)
  gTree(x=x, y=y, type=type, children=do.call(gList, comps),
    name=name, gp=gp, vp=vp, cl='xyGrob')
}

tsGrob <- function(x, name=NULL, gp=NULL, vp=NULL) {
  x <- ts(x)
  xyGrob(time(x), x, type='l', name=name, gp=gp, vp=vp)
}

xyPlotGrob <- function(x, y, type='l', name=NULL, gp=NULL, vp=NULL) {
  xyg <- xyGrob(x, y, type, name=paste(name,'xyGrob', sep='.'))
  box <- rectGrob()
  
  gTree(x=x, y=y, type=type, children=gList(xyg, box))
}

#################
##Ribbon legend:#
#################
ribbonVps <- function(breaks, margin, scale) {
  breaks <- format(signif(breaks, 3))
  vpTree(
    viewport(name='layout', layout=grid.layout(3,4, 
      widths=unit.c(margin, unit(1, 'lines'), max(unit(0.8, 'lines') + stringWidth(breaks), margin)),
      heights=unit.c(margin, unit(1,'null'), margin))),
      vpList(viewport(layout.pos.col=2, layout.pos.row=2, yscale=scale, name='ribbon'),
        viewport(layout.pos.col=3, layout.pos.row=2, yscale=scale, name='labels')
      )
  )
}

ribbonKids <- function(breaks, cols, n=10) {
  scale <- range(breaks)
  nb <- length(breaks)
  tickloc <- seq(scale[1], scale[2], len=n)
  gList(rectGrob(y=unit(breaks[-1], 'native'), height=unit(diff(breaks), 'native'), just='top', gp=gpar(col=NA, fill=cols),
      vp=vpPath('layout','ribbon')),
    rectGrob(vp=vpPath('layout','ribbon')),
    segmentsGrob(x1=unit(0.5, 'lines'), y0=unit(tickloc, 'native'), y1=unit(tickloc, 'native'), vp=vpPath('layout','labels')),
    segmentsGrob(y0=unit(tickloc, 'native'), y1=unit(tickloc, 'native'), vp=vpPath('layout','ribbon')),
    textGrob(x=unit(0.8, 'lines'), y=unit(tickloc, 'native'), just='left', label=format(signif(tickloc, 3)), vp=vpPath('layout','labels'))
  )
}

ribbonLegend <- function(breaks, cols, n=10, margin=unit(0.5, 'lines'), gp=NULL, vp=NULL, name=NULL) {
  scale <- range(breaks)
  gTree(breaks=breaks, cols=cols, n=n, children=ribbonKids(breaks, cols, n), childrenvp=ribbonVps(breaks, margin, scale),
    gp=gp, vp=vp, name=name, cl='ribbonLegend')
}

widthDetails.ribbonLegend <- function(x)
  sum(layout.widths(viewport.layout(x$childrenvp[[1]])))

#############
##Image map:#
#############
mkImageRect <- function(nr, nc, colors, name='image', vp=NULL, gp=NULL) {
  xx <- seq_len(nc)/nc
  yy <- seq_len(nr)/nr
  right <- rep(xx, nr)
  top <- rep(yy, each=nc)
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


imageMapGrob <- function(values, breaks, cols, xlim=0:1, ylim=0:1, legend=NULL, 
  xAxGrob=NULL, yAxGrob=NULL, name=NULL, gp=NULL, vp=NULL) {
  imgPltGrb <- imagePlotGrob(values, breaks, cols, xlim, ylim, xAxGrob, yAxGrob, name=NULL, gp=NULL, vp=vpPath('layVp','imgVp'))
  if(is.null(legend))
    legend <- ribbonLegend(breaks, cols, vp=vpPath('layVp','legendVp'), name='legend')
  legend1 <- legend
  legend1$vp <- NULL
  ly <- grid.layout(1, 2, widths = unit(c(1,1), c('null','grobwidth'), list(NULL, legend1)), respect=TRUE)
  vp <- vpStack(
    viewport(layout=ly, name='layVp'),
    vpList(viewport(layout.pos.col=1, name='imgVp'), viewport(layout.pos.col=2, name='legendVp'))
  )
  gTree(values=values, breaks=breaks, cols=cols, xlim=xlim, ylim=ylim, children=gList(imgPltGrb, legend), childrenvp=vp)
}
grid.imageMap <- function(...)
  grid.draw(imageMapGrob(...))
