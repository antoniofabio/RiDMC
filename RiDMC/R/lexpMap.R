LyapunovExponentsMap <- function(idmc_model, par, var, time, eps,
  par.x = 1, par.x.range, par.x.howMany=100, par.y = 2, par.y.range, par.y.howMany=100,
  eps.zero=sqrt(.Machine$double.eps)) {
  checkModelParVar(idmc_model, par, var)
  checkPositiveScalar(time)
  modelType <- getModelType(idmc_model)
  var <- as.double(var)
  nvar <- length(var)
  ##Lookup matrix:
  lm <- enumerateExponents(nvar)
  m <- idmc_model$model
  recodeExp <- function(x) {
    codes <- integer(nvar)
    codes[is.finite(x) & x>0] <- 3
    codes[is.finite(x) & x<0] <- 1
    codes[is.finite(x) & abs(x)<eps.zero] <- 2
    codes[!is.finite(x)] <- 4
    cd <- c(sum(codes==1), sum(codes==2), sum(codes==3), sum(codes==4))
    which(apply(lm, 1, function(x) all(x == cd)))
  }
  if(modelType=='C') {
    if(missing(eps)) {
      eps <- getOption('ts.eps')
      message('using eps = ', eps)
    }
    checkPositiveScalar(eps)
    eps <- as.double(eps)
    time <- as.double(time)
    lexpLocal <- function(par)
      recodeExp( .Call("ridmc_lexp_ode", m, as.double(par), var, time, eps, PACKAGE='RiDMC') )
  } else {
    time <- as.integer(time)
    lexpLocal <- function(par)
      recodeExp( .Call("ridmc_lexp", m, as.double(par), var, time, PACKAGE='RiDMC') )
  }

  f <- function(a, b) {
    par[par.x] <- a
    par[par.y] <- b
    lexpLocal(par)
  }

  p1seq <- seq(par.x.range[1], par.x.range[2], len=par.x.howMany)
  p2seq <- seq(par.y.range[1], par.y.range[2], len=par.y.howMany)
  mat <- outer(p1seq, p2seq, Vectorize(f))
  ans <- list()
  ans$model <- idmc_model
  ans$var <- var
  ans$par <- par
  ans$par.x <- par.x
  ans$par.x.range <- par.x.range
  ans$par.x.howMany <- par.x.howMany
  ans$par.y <- par.y
  ans$par.y.range <- par.y.range
  ans$par.y.howMany <- par.y.howMany
  ans$mat <- mat
  ans$labels <- rownames(lm)
  class(ans) <- 'idmc_lexp_map'
  ans
}

enumerateExponents <- function(nvar) {
  size <- 4^nvar ##each exponent can be <0, =0, >0, non-finite
  permutations <- function(a) {
    a <- as.matrix(a)
    id <- rownames(a) <- seq_len(NROW(a))
    tmp <- expand.grid(seq_along(id), 1:4)
    cbind(a[tmp[,1],], tmp[,2])
  }

  combine <- function(nvar, current=1:4) {
    if(nvar==1)
      current
    else
      combine(nvar-1, permutations(current))
  }

  ##compute all permutations:
  ans2 <- combine(nvar)
  ans2 <- as.matrix(ans2)
  ##set row labels:
  howManyPos <- function(x) sum(x==3)
  howManyNeg <- function(x) sum(x==1)
  howManyNull <- function(x) sum(x==2)
  howManyDiverging <- function(x) sum(x==4)
  ans <- matrix(,NROW(ans2), 4)
  ans[,1] <- apply(ans2, 1, howManyNeg)
  ans[,2] <- apply(ans2, 1, howManyNull)
  ans[,3] <- apply(ans2, 1, howManyPos)
  ans[,4] <- apply(ans2, 1, howManyDiverging)
  ans <- unique(ans)
  nms <- apply(ans, 1, function(x) {
      paste(if(x[3]) paste(x[3], "positive, ") else "",
        if(x[1]) paste(x[1], 'negative, ') else "",
        if(x[2]) paste(x[2], 'zero') else "",
        if(x[4]) paste(x[4], 'diverging') else "", sep="")
    })
  rownames(ans) <- nms
  return(ans)
}

print.idmc_lexp_map <- function(x, ...) {
  m <- x$model
  cat('=iDMC Lyapunov exponents map=\n')
  cat('Model: ', getModelName(m), '\n')
  cat('Starting point: ')
    tmp <- getModelVarNames(m)
    cat(paste(tmp, signif(x$var), sep=' = ', collapse=', '), '\n')
  cat('Parameter values: ')
    tmp <- getModelParNames(m)
    cat(paste(tmp, x$par, sep=' = ', collapse=', '), '\n')
  pn <- getModelParNames(m)
  cat('Varying x-axis par.: ', pn[x$par.x], '\n')
  cat('Varying x-axis par. range: [', paste(x$par.x.range, collapse=', '), ']\n')
  cat('Varying y-axis par.: ', pn[x$par.y], '\n')
  cat('Varying y-axis par. range: [', paste(x$par.y.range, collapse=', '), ']\n')
}

as.grob.idmc_lexp_map <- function(x, colors, ...) {
  mat <- x$mat
  if(missing(colors))
    colors <- seq_len(max(as.vector(mat)))
  colors <- colors[as.vector(mat)]
  colors <- matrix(colors, NROW(mat))
  colors <- t(colors[,NCOL(colors):1])
  imageGrob(colors, xlim=x$par.x.range, ylim=x$par.y.range, respect = FALSE, name='image')
}

plot.idmc_lexp_map <- function(x, y, colors, labels,
  main = getModelName(x$model),
  xlab, ylab, axes=TRUE, mar=NULL, legend=TRUE, ...) {
  m <- x$model
  pn <- getModelParNames(m)
  if(missing(xlab))
    xlab <- pn[x$par.x]
  if(missing(ylab))
    ylab <- pn[x$par.y]
  mat <- x$mat
  levels <- seq_along(x$labels)
  colors.all <- seq_along(levels)
  ids <- unique(as.vector(mat))
  if(missing(colors))
    colors <- colors.all[ids]
  else
    if(length(colors) < length(ids))
      stop(length(ids), 'colors must be specified')
  colors.all[ids] <- colors
  cG <- as.grob(x, colors=colors.all)
  if(legend) {
    labels.all <- x$labels
    if(missing(labels))
      labels <- labels.all[ids]
    if(length(labels) < length(ids))
      stop('wrong number of labels. There are ', length(ids), 'levels to be labelled')
    cl <- colors.all[ids]
    lb <- labels
    clg <- colorLegendGrob(cl, lb, y=unit(0, 'npc'), x=unit(0, 'npc'), name='legend')
    rightMargin <- convertWidth(widthDetails(clg), 'lines')
    mar <- c(4,4,4,rightMargin)
    mar[4] <- mar[4]*1.04
  } else {
    mar <- NULL
  }
  pG <- plotGrob(cG, main=main, xlab=xlab, ylab=ylab, axes=axes, mar=mar, ...)
  grid.draw(pG)
  if(legend) {
    downViewport('rightMarginArea')
    grid.draw(clg)
    upViewport(0)
  }
}
