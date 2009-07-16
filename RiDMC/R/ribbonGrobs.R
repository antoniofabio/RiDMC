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
