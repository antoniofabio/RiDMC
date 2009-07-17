.makeCycle <- function(fun, pt, period, eigvals=NULL,
                       eps=sqrt(.Machine$double.eps)) {
  stopifnot(length(period)==1)
  stopifnot(is.numeric(pt))
  fun <- match.fun(fun)
  dim <- length(pt)
  structure(list(fun=fun, pt=pt, period=period, eigvals=eigvals, eps=eps),
            class="periodic_cycle")
}
.cycleEps <- function(cycle)
  cycle$eps
.cycleNames <- function(cycle)
  names(cycle$pt)
.cycleDim <- function(cycle)
  length(cycle$pt)
.cyclePeriod <- function(cycle)
  cycle$period
.cycleStability <- function(cycle, eps=.cycleEps(cycle)) {
  ev <- cycle$eigvals
  if(is.null(ev))
    return("unknown")
  ev <- ev - 1.0
  if(all(ev > eps))
    return("unstable")
  else if(all(ev < -eps))
    return("stable")
  else if(all(abs(ev) < eps))
    return("hyperbolic")
  else
    return("saddle")
}

as.matrix.periodic_cycle <- function(x, ...) {
  cycle <- x
  p <- .cyclePeriod(cycle)
  f <- cycle$fun
  ans <- matrix(,  p, .cycleDim(cycle))
  ans[1,] <- cycle$pt
  for(i in 1+seq_len(p-1))
    ans[i,] <- f(ans[i-1,])
  colnames(ans) <- .cycleNames(cycle)
  return(ans)
}

.cycleAlign <- function(toBeAligned, to,
                        eps=min(.cycleEps(toBeAligned), .cycleEps(to))) {
  trToBeAligned <- as.matrix(toBeAligned)
  id <- which(apply(abs(sweep(trToBeAligned,
                              2, to$pt)), 1, max) <= eps)
  if(length(id) == 0)
    return(NULL)
  id <- min(id)
  ans <- toBeAligned
  ans$pt <- trToBeAligned[id,]
  return(ans)
}

.cycleCompare <- function(a, b,
                          eps=min(.cycleEps(a), .cycleEps(b))) {
  if(.cyclePeriod(a) != .cyclePeriod(b))
    return(FALSE)
  b <- .cycleAlign(b, a, eps)
  if(is.null(b))
    return(FALSE)
  trA <- as.matrix(a)
  trB <- as.matrix(b)
  dist <- apply(trA - trB, 1, max) <= eps
  return(all(dist))
}

##check if 'cycle' is a real, "full" cycle
## or a cycle of smaller period
.cycleTrue <- function(cycle, eps=.cycleEps(cycle)) {
  if(.cyclePeriod(cycle) == 1)
    return(TRUE)
  pt <- cycle$pt
  tr <- as.matrix(cycle)[-1,,drop=FALSE]
  dist <- apply(abs(sweep(tr, 2, pt)), 1, max)
  return(!any(dist <= eps))
}

print.periodic_cycle <- function(x, ...) {
  cat("Cycle of period ", .cyclePeriod(x), "\n")
  cat("Classification: ", .cycleStability(x), "\n")
  cat("Trajectory:\n")
  print(as.matrix(x), digits=round(-log10(.cycleEps(x))), ...)
  invisible(x)
}

cycles_find <- function(idmc_model, par, var, period=1,
                        eps=sqrt(.Machine$double.eps),
                        max.iter=100) {
  checkModelDiscrete(idmc_model)
  checkModelParVar(idmc_model, par, var, deparse(substitute(idmc_model)))
  checkPositiveScalar(eps)
  checkPositiveScalar(max.iter)
  period <- as.integer(period)
  if(period<1)
    stop('\'period\' should be an integer >=1')
  ans <- .Call("ridmc_cycles_find", idmc_model$model, as.double(par), as.double(var), 
    period, as.double(eps*0.1), as.integer(max.iter), PACKAGE='RiDMC')
  ##verify that is a real solution:
  value <- ans$result
  for(i in seq_len(period))
    value <- idmc_model$f(par, value)
  ##if not, clean out results:
  if(max(abs(value-ans$result))>eps)
    return(NULL)
  names(ans$result) <- getModelVarNames(idmc_model)
  ans <- .makeCycle(fun = function(x) idmc_model$f(par=par, var=x),
                    pt = ans$result,
                    period = period,
                    eigvals = ans$eigvals,
                    eps=eps)
  return(ans)
}

.getVarList <- function(varMin, varMax, n, vnames){
  domain <- .openRectDomainBuilder(vnames, varMin, varMax)
  return(domain$sample(n))
}

Cycles <- function(idmc_model, par, period=1,
                   eps=sqrt(.Machine$double.eps),
                   ntries=100,
                   max.iter=100,
                   varMin=-Inf, varMax=Inf, varList) {
  if(missing(varList)) {
    varList <- .getVarList(varMin, varMax, ntries,
                           vnames=getModelVarNames(idmc_model))
  } else if(!missing(ntries)) {
    warning("ignoring 'ntries' option: a list of starting values has already been specified")
  }

  stopifnot(is.matrix(varList))
  stopifnot(NCOL(varList) == getModelNVar(idmc_model))
  if(!is.null(colnames(varList))) {
    stopifnot(all(colnames(varList) %in% getModelVarNames(idmc_model)))
    varList <- varList[, getModelVarNames(idmc_model), drop=FALSE]
  }
  var <- varList
  par <- .sanitizeNamedVector(par, getModelParNames(idmc_model))

  cycles <- apply(var, 1, function(start)
                  cycles_find(idmc_model=idmc_model,
                              par=par, var=start,
                              period=period,
                              eps=eps,
                              max.iter=max.iter))
  cycles <- Filter(Negate(is.null), cycles)
  cycles <- Filter(.cycleTrue, cycles)

  ##merge identical cycles
  cycles <- uniqueSet(cycles, .cycleCompare)
  attr(cycles, "model") <- idmc_model
  attr(cycles, "par") <- par
  class(cycles) <- c("idmc_cycles", "list")
  return(cycles)
}

print.idmc_cycles <- function(x, ...) {
  cat(getModelName(attr(x, "model")), "model\n")
  cat(length(x), "periodic cycles found\n")
  cat("\n==details==\n")
  for(i in seq_along(x)) {
    cat("cycle", i, "\n")
    print(x[[i]], ...)
    cat("\n")
  }
  invisible(x)
}

.cycles2trajectoryList <- function(x) {
  stopifnot(inherits(x, "idmc_cycles"))
  model <- attr(x, "model")
  par <- attr(x, "par")
  pt_list <- lapply(x, "[[", "pt")
  time <- lapply(x, .cyclePeriod)
  argList <- list()
  for(i in seq_along(pt_list))
    argList[[i]] <- list(idmc_model = model, par=par,
                         var = pt_list[[i]],
                         time=time[[i]],
                         transient=0)
  trList <- lapply(argList, do.call, what=Trajectory)
  class(trList) <- 'idmc_trajectoryList'
  return(trList)
}

plot.idmc_cycles <- function(x, y, legend=TRUE, ...) {
  ctype <- c("stable", "unstable", "hyperbolic", "saddle", "unknown")
  types <- sapply(x, .cycleStability)
  colors <- c("green", "red", "blue", "yellow", "black")[match(types, ctype)]
  plot(.cycles2trajectoryList(x), y=NULL, colors=colors,
       labels=unique(types), legend=legend, ...)
}
