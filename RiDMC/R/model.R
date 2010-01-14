Model <- function(filename=NULL, text = readLines(filename)) {
  ans <- list()
  if(missing(filename) && missing(text)) {
    if((!interactive()) || (!require('tcltk')))
      stop('you must supply model filename or model text')
    modDir <- system.file('models', package='RiDMC')
    filename <- tclvalue(tkgetOpenFile(initialdir=modDir))
    if(filename=='')
      stop('you must supply model filename or model text')
  } else if (!missing(text)) {
    if(!missing(filename))
      warning("ignoring 'filename' argument")
  } else {
    if(length(grep("\\.lua$", filename)) == 0) {
      if(!file.exists(filename))
        filename <- exModelFile(filename)
    }
  }
  text1 <- paste(text, collapse="\n")
  pointer <- .Call("ridmc_model_alloc", text1, PACKAGE='RiDMC')
  ans <- buildModel(pointer, text)
  return(ans)
}
buildModel <- function(pointer, text) {
  ans <- list()
  ans$text <- text
  ans$model <- model <- pointer
  infos <- .Call("ridmc_model_getInfos", model, PACKAGE='RiDMC')
  names(infos[[1]]) <- c("name","description","type")
  names(infos[[2]]) <- c("has_inverse","has_jacobian")
  names(infos[[3]]) <- c("n.pars","n.vars")
	n.vars <- infos[[3]]['n.vars']
  ans$infos <- infos
  ans$f <- function(par, var)
    .Call("ridmc_model_f", model, as.double(par), as.double(var), PACKAGE='RiDMC')
  ans$g <- function(par, var)
    .Call("ridmc_model_g", model, as.double(par), as.double(var), PACKAGE='RiDMC')
  ans$Jf <- function(par, var)
    matrix(.Call("ridmc_model_Jf", model, as.double(par),
                 as.double(var), PACKAGE='RiDMC'),
           ncol=n.vars, nrow=n.vars,
           byrow=TRUE)
  ans$NumJf <- function(par, var)
    matrix(.Call("ridmc_model_NumJf", model, as.double(par), as.double(var), PACKAGE='RiDMC'), n.vars)
  ans$set.seed <- function(seed)
    invisible(.Call("ridmc_model_setGslRngSeed", model, as.integer(seed), PACKAGE='RiDMC'))
  class(ans) <- "idmc_model"
  return(ans)
}
getModelType <- function(obj, ...)
  obj$infos[[1]]['type']
getModelName <- function(obj, ...)
  obj$infos[[1]]['name']
getModelDescription <- function(obj, ...)
  obj$infos[[1]]['description']
getModelParNames <- function(obj, ...)
  obj$infos[[4]]
getModelNPar <- function(obj, ...)
  length(getModelParNames(obj, ...))
getModelVarNames <- function(obj, ...)
  obj$infos[[5]]
getModelNVar <- function(obj, ...)
  length(getModelVarNames(obj, ...))
getModelText <- function(obj, ...)
  obj$text
getModelHasInverse <- function(obj, ...)
  obj$infos[[2]]["has_inverse"]
getModelHasJacobian <- function(obj, ...)
  obj$infos[[2]]["has_jacobian"]

print.idmc_model <- function(x, ...){
  cat('= iDMC model =\n')
  cat('Name: ', getModelName(x),'\n')
  cat('Description: ', getModelDescription(x),'\n')
  type <- getModelType(x)
  cat('Type: ', if(type=="D") "discrete" 
    else if(type=="C") "continuous"
    else "invalid model type", '\n')
  cat('Parameters: ', paste(getModelParNames(x), collapse=", "),'\n')
  cat('Variables: ', paste(getModelVarNames(x), collapse=", "),'\n')
  cat('Has inverse: ', getModelHasInverse(x)!=0,'\n')
  cat('Has Jacobian: ', getModelHasJacobian(x)!=0,'\n')
}

summary.idmc_model <- function(object, ...)
  structure(object, class='summary.idmc_model')

print.summary.idmc_model <- function(x, ...) {
  print.idmc_model(x, ...)
  cat('\n= iDMC model text =\n')
  cat(getModelText(x), sep='\n')
  cat('\n')
}

##
##Utility functions
##
#get the model function 'f' as a plain R function
getModelF <- function(model) {
  makeF <- function(fName, argsList) {
    argsList <- c(argsList, sep=", ")
    args <- do.call(paste, as.list(argsList))
    paste(fName, "(", args, ")")
  }

  f <- model$f
  parNames <- getModelParNames(model)
  varNames <- getModelVarNames(model)
  packParsExpr <- makeF("c", paste(parNames, parNames, sep="="))
  packVarsExpr <- makeF("c", paste(varNames, varNames, sep="="))
  F <- paste(makeF("function", c(parNames, varNames)),
             makeF("f", c(packParsExpr, packVarsExpr)))
  F <- eval(parse(text=F, srcfile=NULL))
  return(F)
}

getModelMap <- function(model, par) {
  par <- .sanitizeNamedVector(par, getModelParNames(model))
  stopifnot(all(is.finite(par)))
  numVariables <- getModelNVar(model)
  f <- function(var) model$f(var=var, par=par)
  return(function(var) {
    if(!is.matrix(var))
      return(f(var))
    stopifnot(NCOL(var) == numVariables)
    return(t(apply(var, 1, f)))
  })
}
