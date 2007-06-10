CTrajectory <- function(idmc_model, par, var, eps, integrator=0, 
	nsteps=1, transient=0, seed) {
	checkModelParVar(idmc_model, par, var)
	integrator <- as.integer(integrator)
	if((integrator<0)||(integrator>9)) 
		stop('\'integrator\' should be an integer code between 0 and 9')
	if(eps<=0)
		stop('\'eps\' must be a striclty positive real number')
	ans <- list()
	ans$transient <- transient
	ans$par <- par
	ans$var <- var
	ans$eps <- eps
	ans$model <- idmc_model
	ans$integrator <- integrator
	trajectory <- .Call("ridmc_ctrajectory_alloc", 
		idmc_model$model, as.double(par), as.double(var), 
		as.double(eps), as.integer(integrator), PACKAGE='RiDMC')
	ans$trajectory <- trajectory
	ans$step <- function()
		.Call("ridmc_ctrajectory_step", trajectory, PACKAGE='RiDMC')
	ans$getValue <- function()
		.Call("ridmc_ctrajectory_getValue", trajectory, PACKAGE='RiDMC')
	ans$getModel <- function() {
		pp <- .Call("ridmc_ctrajectory_getModel", trajectory, PACKAGE='RiDMC')
		buildModel(pp, idmc_model$text)
	}
	vnames <- getModelVarNames(idmc_model)
	values <- matrix(var, 1, length(vnames))
	colnames(values) <- vnames
	ans$values <- values
	class(ans) <- c("idmc_ctrajectory","idmc_trajectory")
	if(!missing(seed))
		setTrajectorySeed(ans, seed)
	if(transient>0) {
		ans <- stepTrajectory(ans, nsteps=transient)
		ans$values <- ans$values[NROW(ans$values),,drop=FALSE]
	}
	stepTrajectory(ans, nsteps=nsteps)
}
DTrajectory <- function(idmc_model, par, var, nsteps=1, transient=0, seed) {
	checkModelParVar(idmc_model, par, var)
	ans <- list()
	ans$transient <- transient
	ans$par <- par
	ans$var <- var
	ans$eps <- 1
	model <- idmc_model
	ans$model <- model
	trajectory <- .Call("ridmc_dtrajectory_alloc", 
		idmc_model$model, as.double(par), as.double(var), PACKAGE='RiDMC')
	ans$trajectory <- trajectory
	ans$step <- function()
		.Call("ridmc_dtrajectory_step", trajectory, PACKAGE='RiDMC')
	ans$getValue <- function()
		.Call("ridmc_dtrajectory_getValue", trajectory, PACKAGE='RiDMC')
	ans$getModel <- function() {
		pp <- .Call("ridmc_dtrajectory_getModel", trajectory, PACKAGE='RiDMC')
		buildModel(pp, model$text)
	}
	vnames <- getModelVarNames(idmc_model)
	values <- matrix(var, 1, length(vnames))
	colnames(values) <- vnames
	ans$values <- values
	class(ans) <- c("idmc_dtrajectory","idmc_trajectory")
	if(!missing(seed))
		setTrajectorySeed(ans, seed)
	if(transient>0) {
		ans <- stepTrajectory(ans, nsteps=transient)
		ans$values <- ans$values[NROW(ans$values),,drop=FALSE]
	}
	ans$values <- ans$values[NROW(ans$values),,drop=FALSE]
	stepTrajectory(ans, nsteps=nsteps)
}
getTrajectoryModel <- function(idmc_trajectory)
	idmc_trajectory$getModel()
getTrajectoryValues <- function(idmc_trajectory)
	idmc_trajectory$values
setTrajectorySeed <- function(idmc_trajectory, seed)
	getTrajectoryModel(idmc_trajectory)$set.seed(seed)
as.matrix.idmc_trajectory <- function(x, ...)
	getTrajectoryValues(x)
as.ts.idmc_trajectory <- function(x, ...)
	ts(as.matrix(x), frequency = 1/x$eps, ...)

stepTrajectory <- function(idmc_trajectory, nsteps=1) {
	tr <- idmc_trajectory
	vars <- getModelVarNames(getTrajectoryModel(tr))
	oldTr <- getTrajectoryValues(idmc_trajectory)
	newTr <- matrix(,nsteps, length(vars))
	for(i in seq_len(nsteps)) {
		tr$step()
		newTr[i,] <- tr$getValue()
	}
	tr$values <- rbind(oldTr, newTr)
	tr
}

print.idmc_ctrajectory <- function(x, ...) {
	modelInfo <- x$model$infos
	cat('= iDMC model continuous trajectory =\n')
	cat('model: ', modelInfo[[1]]['name'], '\n')
	cat('parameter values: ', paste(x$par, sep=','), '\n')
	cat('starting point: ', paste(x$var, sep=','),'\n')
	cat('step size: ', x$eps, '\n')
	cat('step function: ', x$integrator, '\n')
}
print.idmc_dtrajectory <- function(x, ...) {
	modelInfo <- x$model$infos
	cat('= iDMC model discrete trajectory =\n')
	cat('model: ', modelInfo[[1]]['name'], '\n')
	cat('parameter values: ', paste(x$par, sep=','), '\n')
	cat('starting point: ', paste(x$var, sep=','),'\n')
}

plot.idmc_trajectory <- function(x, y, vars=1:2, type='l', main, xlab, ylab, ...){
	mdl <- getTrajectoryModel(x)
	varNames <- getModelVarNames(mdl)
	names(varNames) <- varNames
	vars <- vars[1:2]
	vars <- varNames[vars]
	if(missing(main))
		main <- getModelName(mdl)
	if(missing(xlab))
		xlab <- vars[1]
	if(missing(ylab))
		ylab <- vars[2]
	if(length(varNames)<2) {
		plot(as.ts(x), main=main, ...)
		return(invisible(NULL))
	}
	xx <- as.matrix(x)[,vars]
	plot(xx, type=type, main=main, xlab=xlab, ylab=ylab, ...)
}
