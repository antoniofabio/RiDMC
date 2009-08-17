LyapunovExponentsMap <- function(idmc_model, par, var, time, eps,
                                 par.x, par.x.range, par.x.howMany=100,
                                 par.y, par.y.range, par.y.howMany=100,
                                 eps.zero=sqrt(.Machine$double.eps)) {
  stopifnot(getModelNPar(idmc_model) >= 2)
  var <- .sanitizeNamedVector(var, getModelVarNames(idmc_model))
  npar <- getModelNPar(idmc_model)
  if(npar > 2) {
    stopifnot(sum(is.finite(par)) >= (npar - 2))
  }
  var <- .sanitizeNamedVector(var, getModelVarNames(m))
  parNames <- getModelParNames(idmc_model)
  names(parNames) <- parNames
  par <- .sanitizeNamedVector(par, parNames)
  stopifnot(sum(is.finite(par)) >= (getModelNPar(m) - 2))
  if(missing(par.x))
    par.x <- which(!is.finite(par))[1]
  if(missing(par.y))
    par.y <- which(!is.finite(par))[2]
  par.x <- parNames[par.x]
  par.y <- parNames[par.y]
  stopifnot(par.x != par.y)
  checkModelParVar(idmc_model, par, var, deparse(substitute(idmc_model)))
  checkPositiveScalar(time)
  modelType <- getModelType(idmc_model)
  var <- as.double(var)
  nvar <- getModelNVar(idmc_model)
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
      message('using integration step eps = ', eps)
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
    par[par.x] <- b
    par[par.y] <- a
    lexpLocal(par)
  }

  raster <- Raster(xlim=par.x.range,
                   ylim=par.y.range,
                   xres=par.x.howMany,
                   yres=par.y.howMany,
                   xName=par.x,
                   yName=par.y)
  xSeq <- rasterXvalues(raster)
  ySeq <- rasterYvalues(raster)
  raster[,] <- outer(ySeq, xSeq, Vectorize(f))
  ans <- list()
  ans$model <- idmc_model
  ans$var <- var
  ans$par <- par
  ans$raster <- raster
  labels <- rownames(lm)
  names(labels) <- seq_len(nrow(lm))
  ans$labels <- labels
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
  nms <- apply(ans, 1, .signsToLegend)
  rownames(ans) <- nms
  return(ans)
}

##Translate a vector of signs counts into a legend string
##Counts are given in that order: (#neg, #zero, #npos, #nna)
.signsToLegend <- function(x) {
  tmp <- ""
  if(x[3]) {
    tmp <- paste(x[3], "positive")
    if(any(x[1]>0, x[2]>0, x[4]>0))
      tmp <- paste(tmp, ",", sep="")
  }
  if(x[1]){
    if(tmp!="") tmp <- paste(tmp, " ", sep="")
    tmp <- paste(tmp, x[1], " negative", sep="")
    if(any(x[2]>0, x[4]>0))
      tmp <- paste(tmp, ",", sep="")
  }
  if(x[2]){
    if(tmp!="") tmp <- paste(tmp, " ", sep="")
    tmp <- paste(tmp, x[2], " zero", sep="")
    if(x[4])
      tmp <- paste(tmp, ",", sep="")
  }
  if(x[4]) {
    if(tmp!="") tmp <- paste(tmp, " ", sep="")
    tmp <- paste(tmp, x[4], " NA", sep="")
  }
  tmp
}

print.idmc_lexp_map <- function(x, ...) {
  m <- x$model
  cat('=iDMC Lyapunov exponents map=\n')
  cat('Model: ', getModelName(m), '\n')
  cat('Starting point: ')
  tmp <- getModelVarNames(m)
  cat(paste(tmp, signif(x$var), sep=' = ', collapse=', '), '\n')
  if(getModelNPar(m) > 2){
    cat('Fixed parameter values: ')
    tmp <- names(x$par)
    cat(paste(tmp, x$par, sep=' = ', collapse=', '), '\n')
  }
  print(x$raster, labels=x$labels, ...)
}

as.matrix.idmc_lexp_map <- function(x, ...)
  as.matrix(x$raster)

as.grob.idmc_lexp_map <- function(x, ...)
  as.grob(x$raster, ...)

plot.idmc_lexp_map <- function(x, y, main = getModelName(x$model),
                               legend=TRUE, palette, ...) {
  valuesTable <- as.character(sort(unique(as.vector(as.matrix(x)))))
  labels <- x$labels[valuesTable]
  names(labels) <- valuesTable
  if(!missing(palette))
    plot(x$raster, main=main, legend=legend, labels=labels, palette=palette, ...)
  else
    plot(x$raster, main=main, legend=legend, labels=labels, ...)
}
