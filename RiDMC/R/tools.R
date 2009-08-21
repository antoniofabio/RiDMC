exModelFile <- function(modelName) {
	p <- system.file('models', package='RiDMC')
	if(missing(modelName)) {
		fls <- list.files(path=p, pattern='\\.lua$', all.files = TRUE, recursive=TRUE)
		fls <- gsub("(.)\\.lua$","\\1",fls)
		cat('The following example models are available:\n')
		print(fls)
		return(invisible(NULL))
	}
	fname <- paste(modelName, 'lua', sep='.')
	fname <- file.path(p,fname)
	return(fname)
}

checkModel <- function(model, txt=deparse(substitute(model))) {
	if(!inherits(model, "idmc_model"))
		stop(sQuote(txt)," should be an idmc_model object")
}

checkModelParVar <- function(model, par, var, txt=deparse(substitute(model))) {
	checkModel(model, txt=txt)
	npnv <- model$infos[[3]]
	if((!missing(par))&&(length(par)!=npnv[1]))
		stop('model has exactly ', npnv[1], ' parameters')
	if((!missing(var))&&(length(var)!=npnv[2]))
		stop('model has exactly ', npnv[1], ' variables')
}

checkModelDiscrete <- function(model, txt=deparse(substitute(model))){
	checkModel(model, txt=txt)
  if(getModelType(model)!='D')
    stop(sQuote(txt)," should be a discrete idmc_model object")
}
checkModelContinuous <- function(model, txt=deparse(substitute(model))){
	checkModel(model, txt)
  if(getModelType(model)!='C')
    stop(sQuote(txt)," should be a continuous idmc_model object")
}


checkPositiveScalar <- function(arg, txt=deparse(substitute(arg))) {
  if((!is.numeric(arg))||(length(arg)>1))
    stop(sQuote(txt), ' should be a numeric scalar')
  if(arg<=0)
    stop(sQuote(txt), ' should be a positive numeric scalar')
}

##
##Expand arguments list to a list of (numerical) arguments
##
##Arguments to be expanded are given as list of 2 elements: starting and ending levels
expandArgList <- function(n=2, ...) {
  args <- list(...)
  ##select list arguments
  isList <- sapply(args, is.list)
  steps <- lapply(args[isList], function(x) (x[[2]]-x[[1]])/(n-1) )
  args[isList] <- lapply(args[isList], "[[", 1)
  ##functions which does just 1 step
  stepList <- function(old) {
    old[isList] <- mapply('+', old[isList], steps, SIMPLIFY=FALSE)
    old
  }
  ans <- list(args)
  for(i in c(2,1+seq_len(n-1)))
    ans[[i]] <- stepList(ans[[i-1]])
  ans
}

inSet <- function(set, elt, compfun) {
  for(elti in set)
    if(compfun(elt, elti))
      return(TRUE)
  return(FALSE)
}

uniqueSet <- function(lst, compfun) {
  ans <- list()
  for(a in lst)
    if(!inSet(ans, a, compfun))
      ans <- c(ans, list(a))
  return(ans)
}

.sanitizeNamedVector <- function(values, valueNames, default.value=NA) {
  stopifnot(is.character(valueNames))
  if(missing(values))
    values <- as.numeric(rep(NA, length(valueNames)))
  stopifnot(is.numeric(values))
  ans <- rep(default.value, length(valueNames))
  names(ans) <- valueNames
  if(!is.null(names(values))) {
    ans[names(values)] <- values
  } else {
    stopifnot(length(values) == length(valueNames))
    ans[seq_along(ans)] <- values
  }
  return(ans)
}

.mustMatchString <- function(x, values) {
  xDp <- deparse(substitute(x))
  if(!is.numeric(x)) {
    x <- match(x, values)
    if(!is.finite(x))
      stop(paste(sQuote(xDp), " must be one of: ", paste(values, collapse=", ")))
  }
  stopifnot(x > 0 && x <= length(values))
  return(x)
}

makeFunPower <- function(fun, k=2) {
  fun <- match.fun(fun)
  return(function(x) {
    for(i in seq_len(k))
      x <- fun(x)
    return(x)
  })
}
