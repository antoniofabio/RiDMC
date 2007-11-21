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
  gTree(values=values, breaks=breaks, colors = colors, children=gList(rg),
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
  xAxGrob, yAxGrob, name=NULL, gp=NULL, vp=NULL) {
  if(missing(xAxGrob)) {
    xAxGrob <- xaxisGrob(at=pretty(xlim, n=10), label=TRUE, vp=vpPath('layVp','mainVp'))
  }
  if(missing(yAxGrob)) {
    yAxGrob <- yaxisGrob(at=pretty(ylim, n=10), label=TRUE, vp=vpPath('layVp','mainVp'))
  }
  ig <- imageGrob(values, breaks, colors, xlim, ylim, name, gp, vp=vpPath('layVp','mainVp'))
  gTree(values=values, breaks=breaks, colors = colors, children=gList(ig, xAxGrob, yAxGrob),
    childrenvp = mkMainAndAxesVp(xlim, ylim), 
    gp=gp, name=name, vp=vp, cl='imagePlotGrob')
}
grid.imagePlotGrob <- function(...)
  grid.draw(imagePlotGrob(...))
