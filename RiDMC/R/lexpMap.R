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
    codes[x>0] <- 3
    codes[x<0] <- 1
    codes[abs(x)<eps.zero] <- 2
    which(apply(lm, 1, function(x) all(x == codes)))
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
  size <- 3^nvar ##each exponent can be <0, =0, >0
  permutations <- function(a) {
    a <- as.matrix(a)
    id <- rownames(a) <- seq_len(NROW(a))
    tmp <- expand.grid(seq_along(id), 1:3)
    cbind(a[tmp[,1],], tmp[,2])
  }

  combine <- function(nvar, current) {
    if(nvar==1)
      current
    else
      combine(nvar-1, permutations(current))
  }

  ##compute all permutations:
  ans <- combine(nvar, 1:3)
  ##set row labels:
  howManyPos <- function(x) sum(x==3)
  howManyNeg <- function(x) sum(x==1)
  howManyNull <- function(x) sum(x==2)
  nms <- apply(ans, 1, function(x) {
      np <- howManyPos(x)
      nn <- howManyNeg(x)
      nz <- nvar - np - nn
      paste(if(np) paste(np, "positive, ") else "", if(nn) paste(nn, 'negative, ') else "", if(nz) paste(nz, 'zero') else "")
    })
  ans <- as.matrix(ans)
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

