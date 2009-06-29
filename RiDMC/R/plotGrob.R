##A plot grob is a grob which has a main 'contents' grob plus all the sorrounding
## labellings.

plotGrob <- function(contents=NULL,
                     main=NULL, xlab=NULL, ylab=NULL,
                     xlim=NULL, ylim=NULL,
                     axes=FALSE, bty=TRUE, respect=NULL,
                     mar=NULL, legendObj=NULL,
                     name=NULL, gp=NULL, vp=NULL) {
  cv <- mkPlotChildsAndViewports(contents=contents,
                                 main=main, xlab=xlab, ylab=ylab,
                                 xlim=xlim, ylim=ylim,
                                 axes=axes, bty=bty,
                                 respect = respect, mar=mar,
                                 legendObj = legendObj)
  gTree(contents=contents,
        main=main, xlab=xlab, ylab=ylab,
        xlim=xlim, ylim=ylim,
        axes=axes, bty=bty,
        respect=respect,
        mar=mar,
        legendObj=legendObj,
        children = cv$children,
        childrenvp = cv$viewports,
        name=name, gp=gp, vp=vp,
        cl='plotGrob')
}
editDetails.plotGrob <- function(x, specs) {
  args <- as.list(formals(plotGrob))
  for(nm in names(x))
    if(nm %in% names(args)) args[[nm]] <- x[[nm]]
  for(nm in names(specs))
    args[[nm]] <- specs[[nm]]
  do.call(plotGrob, args)
}

#builds up the tree of plotGrob viewports
makePlotGrobViewports <- function(xlim, ylim, respect, mar) {
  ws <- unit(c(mar[2], diff(xlim), mar[4]), c('lines','null','lines'))
  hs <- unit(c(mar[3], diff(ylim), mar[1]), c('lines','null','lines'))
  ly <- grid.layout(3, 3, widths=ws, heights=hs)
  lyIso <- grid.layout(1, 1, widths= ws[2], heights= hs[2], respect=respect)
  vpStack(viewport(layout=lyIso, name='plotLayout'),
          viewport(layout.pos.col=1, layout.pos.row=1, layout=ly, name='rootArea', clip=FALSE),
          vpList(viewport(layout.pos.col=2, layout.pos.row=2, name='axesArea', xscale=xlim, yscale=ylim, clip="off"),
                 viewport(layout.pos.col=2, layout.pos.row=2, name='plotArea', xscale=xlim, yscale=ylim, clip="on"),
                 viewport(layout.pos.row=1, layout.pos.col=2, name='titleArea', gp=gpar(cex=par('cex.main'))),
                 viewport(layout.pos.col=2, layout.pos.row=3, name='xlabArea'),
                 viewport(layout.pos.col=1, layout.pos.row=2, name='ylabArea'),
                 viewport(layout.pos.col=3, layout.pos.row=2, name='rightMarginArea')))
}

#builds up childs and viewports for a new plotGrob
mkPlotChildsAndViewports <- function(contents=NULL,
                                     main=NULL, xlab=NULL, ylab=NULL,
                                     xlim=NULL, ylim=NULL, respect=NULL,
                                     axes=FALSE, bty=TRUE, mar=NULL,
                                     legendObj=NULL) {
  null.mar <- is.null(mar)
  if(null.mar)
    mar <- c(0,0,0,0)
  childs <- list()
  append <- function(lst, elt) {
    lst[[length(lst)+1]] <- elt
    lst
  }
  if(bty) {
    childs <- append(childs, rectGrob(name='box', vp=vpPath('plotLayout','rootArea','axesArea')))
  }
  if(!is.null(main)) { ##reserve title space
    mar[3] <- max(4, mar[3])
    childs <- append(childs,
                     textGrob(main, name='title', y=unit(2,'lines'), just=c('center','top'),
                              vp=vpPath('plotLayout', 'rootArea', 'titleArea')))
  }
  if(!is.null(xlab)) { ##reserve xlab space
    mar[1] <- max(4, mar[1])
    childs <- append(childs,
                     textGrob(xlab, y=unit(1, 'npc') - unit(3, 'lines'), name='xlab',
                              just=c('center', 'bottom'),
                              vp=vpPath('plotLayout', 'rootArea', 'xlabArea')))
  }
  if(!is.null(ylab)) { ##reserve ylab space
    mar[2] <- max(4, mar[2])
    childs <- append(childs, textGrob(ylab, x=unit(1, 'npc') - unit(3, 'lines'), rot=90, name='ylab',
      vp=vpPath('plotLayout', 'rootArea', 'ylabArea')))
  }
  if(axes) { ##add axes to main area
    mar[1] <- max(2, mar[1])
    mar[2] <- max(2, mar[2])
    mar[4] <- max(2, mar[4])
    childs <- append(childs, xaxisGrob(name='xaxis',
                                       vp=vpPath('plotLayout','rootArea','axesArea')))
    childs <- append(childs, yaxisGrob(name='yaxis',
                                       edits=gEdit('labels', rot=90, just=c('center','bottom')),
                                       vp=vpPath('plotLayout', 'rootArea', 'axesArea')))
  }
  if(!is.null(contents)) {
    contents$vp <- vpPath('plotLayout','rootArea','plotArea')
    childs <- append(childs, contents)
  }
  if(!is.null(legendObj)) {
    mar[4] <- max(unclass(convertWidth(widthDetails(legendObj), 'lines')) * 1.04,
                  mar[4])
    legendObj$vp <- vpPath('plotLayout', 'rootArea', 'rightMarginArea')
    childs <- append(childs, legendObj)
  }
  children <- do.call(gList, childs)
  if(is.null(xlim)) xlim <- Xlim(contents)
  if(is.null(ylim)) ylim <- Ylim(contents)
  if(is.null(respect)) respect <- Respect(contents)
  viewports <- makePlotGrobViewports(xlim=xlim, ylim=ylim, respect=respect, mar=mar)
  list(children=children, viewports=viewports)
}

.fixLim <- function(lim) {
  d <- diff(lim) * 0.04
  lim[1] <- lim[1] - d
  lim[2] <- lim[2] + d
  lim
}
