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
  numRows <- ceiling(nv / numColumns)
  ys <- unit(1, 'npc') - unit((seq_len(nv)-1) %% numRows + 1, 'lines')
  xs0 <- (unit(0.5, 'lines') +
          .colorLegendColumnWidth(labels=labels) *
          (seq_len(numColumns) - 1))[rep(seq_len(numColumns),
                                         each=numRows)][seq_len(nv)]
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
    clg <- colorLegendGrob(x$colors, x$labels,
                           if(!is.null(specs$x)) specs$x else x$x,
                           if(!is.null(specs$y)) specs$y else x$y)
    x <- Reduce(addGrob, clg$children, x)
  }
  if('numColumns' %in% names(specs)) {
    clg <- colorLegendGrob(x$colors, x$labels, x$x, x$y, numColumns=specs$numColumns)
    x <- Reduce(addGrob, clg$children, x)
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
