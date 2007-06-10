
Model <- function(filename=NULL, text = readLines(filename)) {
	ans <- list()
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
	ans$infos <- infos
	ans$f <- function(par, var)
		.Call("ridmc_model_f", model, as.double(par), as.double(var), PACKAGE='RiDMC')
	ans$g <- function(par, var)
		.Call("ridmc_model_g", model, as.double(par), as.double(var), PACKAGE='RiDMC')
	ans$Jf <- function(par, var)
		.Call("ridmc_model_Jf", model, as.double(par), as.double(var), PACKAGE='RiDMC')
	ans$NumJf <- function(par, var)
		.Call("ridmc_model_NumJf", model, as.double(par), as.double(var), PACKAGE='RiDMC')
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
getModelVarNames <- function(obj, ...)
	obj$infos[[5]]
getModelText <- function(obj, ...)
	obj$text
getModelHasInverse <- function(obj, ...)
	obj$infos[[2]]["has_inverse"]
getModelHasJacobian <- function(obj, ...)
	obj$infos[[2]]["has_jacobian"]

print.idmc_model <- function(x, ...){
	cat('= iDMC model text =\n')
	cat(getModelText(x), sep='\n')
	cat('\n')
}

summary.idmc_model <- function(object, ...)
	structure(object, class='summary.idmc_model')

print.summary.idmc_model <- function(x, ...) {
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
	cat('Has jacobian: ', getModelHasJacobian(x)!=0,'\n')
}
