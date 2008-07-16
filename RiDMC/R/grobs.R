##
##Helper functions for simulating inheritance
## (strongly inspired by the R.oo package)
##
extend <- function(baseObj, className, ..., warningOnOverlap=TRUE)
  UseMethod('extend')
extend.grob <- function(baseObj, className, ..., warningOnOverlap=TRUE) {
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

.upgrade <- function(x, specs) UseMethod('.upgrade')
.upgrade.grob <- function(x, specs) {
  for(nm in names(specs))
    x[[nm]] <- specs[[nm]]
  x
}

##Convert an object into a graphical object
as.grob <- function(x, ...) UseMethod('as.grob')
as.grob.grob <- function(x, ...) x

###############################
##Auxiliary grobs attributes ##
###############################

##Add 'xlim', 'ylim' and 'respect' attributes to an arbitrary grob
contentsGrob <- function(baseGrob, xlim, ylim, respect=FALSE) {
  Xlim(baseGrob) <- xlim
  Ylim(baseGrob) <- ylim
  Respect(baseGrob) <- respect
  baseGrob
}

##xlim and ylim attributes getters and setters
Xlim <- function(obj, ...) UseMethod("Xlim")
Xlim.grob <- function(obj, ...) {
  ans <- attr(obj, 'Xlim')
  if(is.null(ans))
    0:1
  else
    ans
}
"Xlim<-" <- function(obj, value) UseMethod("Xlim<-")
"Xlim<-.grob" <- function(obj, value) {
  attr(obj, 'Xlim') <- value
  obj
}
Ylim <- function(obj, ...) UseMethod("Ylim")
Ylim.grob <- function(obj, ...) {
  ans <- attr(obj, 'Ylim')
  if(is.null(ans))
    0:1
  else
    ans
}
"Ylim<-" <- function(obj, value) UseMethod("Ylim<-")
"Ylim<-.grob" <- function(obj, value) {
  attr(obj, 'Ylim') <- value
  obj
}

##'respect' attribute getter and setter
Respect <- function(obj, ...) UseMethod("Respect")
Respect.grob <- function(obj, ...) {
  ans <- attr(obj, 'Respect')
  if(is.null(ans))
    FALSE
  else
    ans
}
"Respect<-" <- function(obj, value) UseMethod("Respect<-")
"Respect<-.grob" <- function(obj, value) {
  attr(obj, 'Respect') <- value
  obj
}

##Get field from specified object
##Warns if field isn't found
getField <- function(obj, fieldName, warningOnNotFound=TRUE)
  UseMethod('getField')
getField.grob <- function(obj, fieldName, warningOnNotFound=TRUE) {
  if(warningOnNotFound && (!(fieldName %in% names(obj))))
    stop('cannot find field ', fieldName, 'in ', deparse(substitute(obj)))
  return(obj[[fieldName]])
}

##
##Generic grob classes
##

##x/y graph
xyGrob <- function(x, y, type='l', xlim, ylim, name=NULL, gp=NULL, pch = 1, cex=1, size = unit(cex, "char"), ...) {
  pgr <- pointsGrob(x, y, name=paste(name, 'points', sep='.'), default.units='native', pch=pch, size=size, gp=gpar(...))
  lgr <- linesGrob(x, y, name=paste(name, 'lines', sep='.'), default.units='native', gp=gpar(...))
  if(type=='l')
    comps <- list(lgr)
  else if(type=='p')
    comps <- list(pgr)
  else if (type=='b')
    comps <- list(lgr, pgr)
  if(missing(xlim))
    xlim <- range(x[is.finite(x)])
  if(missing(ylim))
    ylim <- range(y[is.finite(y)])
  ans <- gTree(x=x, y=y, type=type, children=do.call(gList, comps),
    name=name, gp=gp, 'xyGrob')
  xlim <- .fixLim(xlim)
  ylim <- .fixLim(ylim)
  ans <- contentsGrob(ans, xlim=xlim, ylim=ylim)
}

tsGrob <- function(x, name=NULL, gp=NULL, vp=NULL) {
  x <- ts(x)
  xyGrob(time(x), x, type='l', name=name, gp=gp)
}
