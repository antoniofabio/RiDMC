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

##################
##Colors legend ##
##################
colorLegendGrob <- function(colors, labels,
                            x=unit(0,'npc'), y=unit(0, 'npc'),
                            numColumns=1,
                            name=NULL, gp=NULL, vp=NULL) {
  nv <- length(colors)
  if(missing(labels))
    labels <- as.character(seq_along(colors))
  if(numColumns > 1) {
    numRows <- ceiling(nv / numColumns)
    ys <- unit(1, 'npc') - unit((seq_len(nv)-1) %% numRows + 2, 'lines')
    xs0 <- (unit(0.5, 'lines') +
            .colorLegendColumnWidth(labels=labels) *
            (seq_len(numColumns) - 1))[rep(seq_len(numColumns),
                                           each=numRows)][seq_len(nv)]
  } else {
    ys <- unit(1, 'npc') - unit(seq_len(nv) + 1, 'lines')
    xs0 <- unit(0.5, 'lines')
  }
  xs1 <- xs0 + unit(1.1, 'lines')
  rg <- rectGrob(x=xs0+x, y=ys-y,
                 width=unit(0.6, 'lines'),
                 height=unit(0.6, 'lines'), just=c('left','bottom'), 
                 gp=gpar(fill=colors), name='rect')
  lg <- textGrob(labels, x=xs1+x, y=ys-y, just=c('left','bottom'), name='text')
  gTree(colors=colors, labels=labels, x=x, y=y, numColumns=numColumns,
        name=name, gp=gp, vp=vp, children=gList(rg, lg), cl='colorLegendGrob')
}
.colorLegendColumnWidth <- function(obj, labels=obj$labels) {
  max(stringWidth(labels)) + unit(2.1, 'lines')
}
grid.colorLegend <- function(...){
  grid.draw(colorLegendGrob(...))
}
editDetails.colorLegendGrob <- function(x, specs) {
  if(any(c('x','y') %in% names(specs))) {
    x <- colorLegendGrob(x$colors, x$labels, 
                         if(!is.null(specs$x)) specs$x else x$x,
                         if(!is.null(specs$y)) specs$y else x$y)
  }
  if('numColumns' %in% names(specs)) {
    x <- colorLegendGrob(x$colors, x$labels, x$x, x$y, numColumns=specs$numColumns)
  }
  if('colors' %in% names(specs)) {
    x <- editGrob(x, 'rect', gp=gpar(fill=specs$colors))
  }
  if('labels' %in% names(specs)) {
    x <- editGrob(x, 'text', label=specs$labels)
  }
  x
}

widthDetails.colorLegendGrob <- function(x) {
  .colorLegendColumnWidth(x) * x$numColumns + unit(0.5, 'lines')
}
heightDetails.colorLegendGrob <- function(x) {
  unit(ceiling(length(x$labels) / x$numColumns), 'lines')
}
