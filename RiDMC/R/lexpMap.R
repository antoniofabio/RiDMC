LyapunovExponentsMap <- function(idmc_model, par, var, time, eps,
  par.x = 1, par.x.range, par.x.howMany=100, par.y = 2, par.y.range, par.y.howMany=100) {
  checkModelParVar(idmc_model, par, var)
  checkPositiveScalar(time)
  modelType <- getModelType(idmc_model)
  var <- as.double(var)
  m <- idmc_model$model
  if(modelType=='C') {
    if(missing(eps)) {
      eps <- getOption('ts.eps')
      message('using eps = ', eps)
    }
    checkPositiveScalar(eps)
    eps <- as.double(eps)
    time <- as.double(time)
    lexpLocal <- function()
      .Call("ridmc_lexp_ode", m, as.double(par), var, time, eps, PACKAGE='RiDMC')
  } else {
    time <- as.integer(time)
    lexpLocal <- function(par)
      .Call("ridmc_lexp", m, as.double(par), var, time, PACKAGE='RiDMC')
  }

  f <- function(p1, p2) {
    par[par.x] <- p1
    par[par.y] <- p2
    ans <- lexpLocal(par)
    ans[is.finite(ans) & ans>0] <- 1
    ans[is.finite(ans) & ans<0] <- -1
    as.integer(ans)
  }

  p1seq <- seq(par.x.range[1], par.x.range[2], len=par.x.howMany)
  p2seq <- seq(par.y.range[1], par.y.range[2], len=par.y.howMany)
  y <- rep(p2seq, rep.int(length(p1seq), length(p2seq)))
  x <- rep(p1seq, times = ceiling(length(y)/length(p1seq)))
  y <- as.list(y)
  x <- as.list(x)
  mat <- mapply(f, x, y, SIMPLIFY=FALSE)
  mat <- array(mat, dim=c(length(p1seq), length(p2seq)))
  ans <- list()
  ans$model <- idmc_model
  ans$var <- var
  ans$par <- par
  ans$values <- val
  ans$par.x <- getModelParNames(m)[par.x]
  ans$par.x.howMany <- par.x.howMany
  ans$par.y <- getModelParNames(m)[par.y]
  ans$par.y.howMany <- par.y.howMany
  ans$mat <- mat
  class(ans) <- 'idmc_lexp_map'
  ans
}

print.idmc_lexp_map <- function(x, ...) {
  m <- x$model
  cat('=iDMC Lyapunov exponents map=\n')
  cat('Model: ', getModelName(m), '\n')
  cat('Starting point: ')
    tmp <- getModelVarNames(m)
    cat(paste(tmp, x$var, sep=' = ', collapse=', '), '\n')
  cat('Parameter values: ')
    tmp <- getModelParNames(m)
    cat(paste(tmp, x$par, sep=' = ', collapse=', '), '\n')
  cat('Varying x-axis par.: ', x$par.x, '\n')
  cat('Varying x-axis par. range: [', paste(x$par.x.range, collapse=', '), ']\n')
  cat('Varying y-axis par.: ', x$par.y, '\n')
  cat('Varying y-axis par. range: [', paste(x$par.y.range, collapse=', '), ']\n')
  mle <- range(apply(x$values, 1, max, na.rm=TRUE), na.rm=TRUE)
  cat('MLE range: [', paste(format(mle, digits=4), collapse=', '), ']\n')
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
  nms <- apply(ans, 1, function(x)
    paste(howManyPos(x), "positive,", howManyNeg(x), 'negative', howManyNull(x), 'zero')
    )
  rownames(ans) <- nms
  return(ans)
}
