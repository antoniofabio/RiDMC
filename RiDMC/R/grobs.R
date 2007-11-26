##
##Helper functions for simulating inheritance
## (strongly inspired by the R.oo package)
##
extend <- function(baseObj, className, ..., warningOnOverlap=TRUE)
  UseMethod('extend')
extend.list <- function(baseObj, className, ..., warningOnOverlap=TRUE) {
  ans <- baseObj
  attr.new <- list(...)
  nms <- names(attr.new)
  ovl <- intersect(nms, names(ans))
  if(warningOnOverlap && (length(ovl) > 0))
    warning('Attributes already in', class(baseObj)[1], 'object: ', paste(ovl, collapse=', '))
  for(nm in nms)
    ans[[nm]] <- attr.new[[nm]]
  class(ans) <- c(className, class(ans))
  ans
}

###############################
##Abstract contentsGrob class##
###############################
contentsGrob <- function(xlim, ylim, respect=FALSE, name=NULL, gp=gpar(), vp=NULL)
  extend(grob(name=name, gp=gp, vp=vp), 'contents', xlim=xlim, ylim=ylim, respect=respect)
getXlim <- function(x) UseMethod('getXlim')
getXlim.grob <- function(x) 0:1
getXlim.contents <- function(x) getField(x, 'xlim')
getYlim <- function(x) UseMethod('getYlim')
getYlim.grob <- function(x) 0:1
getYlim.contents <- function(x) getField(x, 'ylim')
getRespect <- function(x) UseMethod('getRespect')
getRespect.grob <- function(x) FALSE
getRespect.contents <- function(x) getField(x, 'respect')

##Get field from specified object
##Warns if field isn't found
getField <- function(obj, fieldName, warningOnNotFount=TRUE)
  UseMethod('getFrom')
getField.list <- function(obj, fieldName, warningOnNotFound=TRUE) {
  if(warningOnNotFound && (!(fieldName %in% names(obj))))
    stop('cannot find field ', fieldName, 'in ', deparse(substitute(obj)))
  return(obj[[fieldName]])
}

##
##Generic grob classes
##
plotGrob <- function(contents=NULL, main=NULL, xlab=NULL, ylab=NULL, 
  xlim=0:1, ylim=0:1, axes=FALSE, respect=NULL, mar=NULL, name=NULL, gp=NULL, vp=NULL) {
  cv <- mkPlotChildsAndViewports(contents=contents, main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
    axes=axes, respect = respect, mar=mar)
  gTree(contents=contents, main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, mar=mar, 
    children = cv$children, childrenvp = cv$viewports,
    name=name, gp=gp, vp=vp, cl='plotGrob')
}
editDetails.plotGrob <- function(x, specs) {
  do.call(plotGrob, specs)
}

makePlotGrobViewports <- function(xlim, ylim, respect, mar) {
  ws <- unit(c(mar[2], diff(xlim), mar[4]), c('lines','null','lines'))
  hs <- unit(c(mar[3], diff(ylim), mar[1]), c('lines','null','lines'))
  ly <- grid.layout(3, 3, widths=ws, heights=hs)
  lyIso <- grid.layout(1, 1, widths= ws[2], heights= hs[2], respect=respect)
  vpStack(viewport(layout=lyIso, name='plotLayout'),
    viewport(layout.pos.col=1, layout.pos.row=1, layout=ly, name='rootArea', clip=FALSE),
      vpList(
        viewport(layout.pos.col=2, layout.pos.row=2, name='axesArea', xscale=xlim, yscale=ylim, clip=FALSE),
        viewport(layout.pos.col=2, layout.pos.row=2, name='plotArea', xscale=xlim, yscale=ylim, clip=TRUE),
        viewport(layout.pos.row=1, name='titleArea'),
        viewport(layout.pos.col=2, layout.pos.row=3, name='xlabArea'),
        viewport(layout.pos.col=1, layout.pos.row=2, name='ylabArea')))
}

mkPlotChildsAndViewports <- function(contents=NULL, main=NULL, xlab=NULL, ylab=NULL, 
  xlim=NULL, ylim=NULL, respect=NULL, axes=FALSE, mar=NULL) {
  null.mar <- is.null(mar)
  if(null.mar)
    mar <- c(0,0,0,0)
  childs <- list()
  append <- function(lst, elt) {
    lst[[length(lst)+1]] <- elt
    lst
  }
  if(!is.null(main)) { ##reserve title space
    if(null.mar)
      mar[3] <- 4
    childs <- append(childs, textGrob(main, name='title', y=unit(3.5,'lines'), just=c('center','top'),
      vp=vpPath('plotLayout', 'rootArea', 'titleArea')))
  }
  if(!is.null(xlab)) { ##reserve xlab space
    if(null.mar)
      mar[1] <- 4
    childs <- append(childs, textGrob(xlab, y=unit(1, 'lines'), name='xlab', just=c('center', 'bottom'),
      vp=vpPath('plotLayout', 'rootArea', 'xlabArea')))
  }
  if(!is.null(ylab)) { ##reserve ylab space
    if(null.mar)
      mar[2] <- 4
    childs <- append(childs, textGrob(ylab, x=unit(1, 'lines'), rot=90, name='ylab',
      vp=vpPath('plotLayout', 'rootArea', 'ylabArea')))
  }
  if(axes) { ##add axes to main area
    if(null.mar) {
      mar[4] <- 2
      mar[3] <- max(2, mar[3])
    }
    childs <- append(childs, xaxisGrob(name='xaxis', vp=vpPath('plotLayout','rootArea','axesArea')))
    childs <- append(childs, yaxisGrob(name='yaxis', 
      edits=gEdit('labels', rot=90, just=c('center','bottom')),
      vp=vpPath('plotLayout', 'rootArea', 'axesArea')))
  }
  if(!is.null(contents)) {
    contents$vp <- vpPath('plotLayout','rootArea','plotArea')
    childs <- append(childs, contents)
  }
  children <- do.call(gList, childs)
  if(is.null(xlim)) xlim <- getXlim(contents)
  if(is.null(ylim)) ylim <- getYlim(contents)
  if(is.null(respect)) respect <- getRespect(contents)
  viewports <- makePlotGrobViewports(xlim=xlim, ylim=ylim, respect=respect, mar=mar)
  list(children=children, viewports=viewports)
}

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

