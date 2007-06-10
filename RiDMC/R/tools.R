exModelFile <- function(modelName) {
	p <- system.file('models', package='RiDMC')
	if(missing(modelName)) {
		fls <- list.files(path=p, pattern = "\\.lua$", all.files = TRUE)
		fls <- gsub("(.)\\.lua$","\\1",fls)
		cat('The following example models are available:\n')
		print(fls)
		return(invisible(NULL))
	}
	fname <- paste(modelName, 'lua', sep='.')
	fname <- file.path(p,fname)
	return(fname)
}

checkModelParVar <- function(model, par, var) {
	txt <- sQuote(deparse(substitute(model)))
	if(!inherits(model, "idmc_model"))
		stop(txt,"should be an idmc_model object")
	npnv <- model$infos[[3]]
	if((!missing(par))&&(length(par)!=npnv[1]))
		stop('model has exactly', npnv[1], 'parameters')
	if((!missing(var))&&(length(var)!=npnv[2]))
		stop('model has exactly', npnv[1], 'variables')
}

checkModelDiscrete <- function(model){
	txt <- sQuote(deparse(substitute(model)))
	if(!inherits(model, "idmc_model"))
		stop(txt,"should be an idmc_model object")
	if(getModelType(model)!='D')
		stop(txt,"should be a discrete idmc_model object")
}
checkModelContinuous <- function(model){
	txt <- sQuote(deparse(substitute(model)))
	if(!inherits(model, "idmc_model"))
		stop(txt,"should be an idmc_model object")
	if(getModelType(model)!='C')
		stop(txt,"should be a continuous idmc_model object")
}


checkPositiveScalar <- function(arg) {
	txt <- sQuote(deparse(substitute(arg)))
	if((!is.numeric(arg))||(length(arg)>1))
		stop(txt, 'should be a numeric scalar')
	if(arg<=0)
		stop(txt, 'should be a positive numeric scalar')
}
