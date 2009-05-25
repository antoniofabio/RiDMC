##make an open, rectangular domain
openRectDomain <- function(varnames, mins, maxs) {
  stopifnot(is.character(varnames))
  stopifnot(is.numeric(mins))
  stopifnot(is.numeric(maxs))
  stopifnot(all(mins < maxs))
  dim <- length(varnames)
  stopifnot(length(mins) == dim)
  stopifnot(length(maxs) == dim)
  sample <- function(n) {
    a <- matrix(rnorm(n * dim), n, dim)
    colnames(a) <- varnames
    finite.mins <- is.finite(mins)
    finite.maxs <- is.finite(maxs)
    finite.mM <- finite.mins | finite.maxs
    if(sum(finite.mM) > 0) {
      a[,finite.mM] <- exp(a[,finite.mM])
      id1 <- finite.mins & (!finite.maxs)
      a[, id1] <- sweep(a[, id1, drop=FALSE], 2, mins[id1], "+")
      id2 <- (!finite.mins) & finite.maxs
      a[,id2] <- sweep(-a[, id2, drop=FALSE], 2, maxs[id2], "+")
      id3 <- finite.mins & finite.maxs
      ranges <- (maxs - mins)[id3]
      a[,id3] <- sweep(exp(-a[, id3, drop=FALSE]) * ranges, 2, mins[id3], "+")
    }
    if(n==1)
      a <- a[,,drop=TRUE]
    return(a)
  }
  inside <- function(pt) {
    if(!is.null(names(pt))) {
      stopifnot(setequal(names(pt), varnames))
      pt <- pt[varnames]
    } else {
      stopifnot(length(pt) == dim)
    }
    if(any((pt - mins) < 0))
      return(FALSE)
    if(any((pt - maxs) > 0))
      return(FALSE)
    return(TRUE)
  }
  return(list(sample=sample, inside=inside))
}

##
##Flexible open rectangular domain declaration
##
.openRectDomainBuilder <- function(varnames, mins, maxs) {
  dim <- length(varnames)
  vn <- varnames
  if(is.null(names(mins))) {
    if(length(mins) == 1)
      mins <- rep(mins, dim)
    stopifnot(length(mins) == dim)
    names(mins) <- vn
  }
  stopifnot(all(names(mins) %in% varnames))
  m <- rep(-Inf, dim)
  names(m) <- vn
  m[names(mins)] <- mins
  if(is.null(names(maxs))) {
    if(length(maxs) == 1)
      maxs <- rep(maxs, dim)
    stopifnot(length(maxs) == dim)
    names(maxs) <- vn
  }
  stopifnot(all(names(maxs) %in% varnames))
  M <- rep(Inf, dim)
  names(M) <- vn
  M[names(maxs)] <- maxs
  return(openRectDomain(varnames, m, M))
}
