Trajectory <- function(idmc_model, par, var, time=1, transient=0, 
  seed, eps, integrator=2) {
  m <- idmc_model
  checkModelParVar(m, par, var)
  ans <- list()
  ans$transient <- transient
  ans$time <- time
  ans$par <- par
  ans$var <- var
  ans$model <- m
  if(getModelType(m)=='C') { ##Continuous model
    integrator <- as.integer(integrator)
    if((integrator<0)||(integrator>8)) 
      stop('\'integrator\' should be an integer code between 0 and 8')
    if(missing(eps)) {
      eps <- getOption("ts.eps")
      message('using eps = ', eps)
    }
    checkPositiveScalar(eps)
    ans$eps <- eps
    ans$integrator <- integrator
    trajectory <- .Call("ridmc_ctrajectory_alloc", 
      m$model, as.double(par), as.double(var), 
      as.double(eps), as.integer(integrator), PACKAGE='RiDMC')
    ans$trajectory <- trajectory
    ans$step <- function()
      .Call("ridmc_ctrajectory_step", trajectory, PACKAGE='RiDMC')
    ans$getValue <- function()
      .Call("ridmc_ctrajectory_getValue", trajectory, PACKAGE='RiDMC')
    ans$getModel <- function() {
      pp <- .Call("ridmc_ctrajectory_getModel", trajectory, PACKAGE='RiDMC')
      buildModel(pp, getModelText(m))
    }
    class(ans) <- c("idmc_ctrajectory","idmc_trajectory")
  } else { ##Discrete model
    eps <- 1
    trajectory <- .Call("ridmc_dtrajectory_alloc", 
      m$model, as.double(par), as.double(var), PACKAGE='RiDMC')
    ans$eps <- eps
    ans$trajectory <- trajectory
    ans$step <- function()
      .Call("ridmc_dtrajectory_step", trajectory, PACKAGE='RiDMC')
    ans$getValue <- function()
      .Call("ridmc_dtrajectory_getValue", trajectory, PACKAGE='RiDMC')
    ans$getModel <- function() {
      pp <- .Call("ridmc_dtrajectory_getModel", trajectory, PACKAGE='RiDMC')
      buildModel(pp, m$text)
    }
    class(ans) <- c("idmc_dtrajectory","idmc_trajectory")
  }
  vnames <- getModelVarNames(m)
  values <- matrix(var, 1, length(vnames))
  colnames(values) <- vnames
  ans$values <- values
  if(!missing(seed))
    setTrajectorySeed(ans, seed)
  if(transient>0) {
    ans <- stepTrajectory(ans, transient)
    ans$values <- ans$values[NROW(ans$values),,drop=FALSE]
  }
  stepTrajectory(ans, time)
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
  ts(as.matrix(x), frequency = 1/x$eps, 
    start=x$transient, ...)

stepTrajectory <- function(idmc_trajectory, time=1) {
  tr <- idmc_trajectory
  eps <- tr$eps
  nsteps <- floor(time/eps)
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
  cat('model: ', getModelName(getTrajectoryModel(x)), '\n')
  cat('parameter values: ', paste(x$par, sep=','), '\n')
  cat('starting point: ', paste(x$var, sep=','),'\n')
  cat('transient length: ', x$transient, '\n')
  cat('time span: ', x$time, '\n')
  cat('step size: ', x$eps, '\n')
  cat('step function: ', x$integrator, '\n')
}
print.idmc_dtrajectory <- function(x, ...) {
  cat('= iDMC model discrete trajectory =\n')
  cat('model: ', getModelName(getTrajectoryModel(x)), '\n')
  cat('parameter values: ', paste(x$par, sep=','), '\n')
  cat('starting point: ', paste(x$var, sep=','),'\n')
  cat('transient length: ', x$transient, '\n')
  cat('time span: ', x$time, '\n')
}

as.grob.idmc_trajectory <- function(x, vars=1:2, type='l', ...) {
  mdl <- getTrajectoryModel(x)
  varNames <- getModelVarNames(mdl)
  names(varNames) <- varNames
  vars <- vars[1:2]
  vars <- varNames[vars]
  if(length(varNames)<2) {
    y <- as.ts(x)
    xyGrob(time(y), y, type=type, name='xy', ...)
  } else {
    xx <- as.matrix(x)[,vars]
    xyGrob(xx[,1], xx[,2], type=type, name='xy', ...)
  }
}

plot.idmc_trajectory <- function(x, y, vars=1:2, type='l',
  main = getModelName(getTrajectoryModel(x)), xlab, ylab,
  mar = NULL, axes=TRUE, bty=TRUE, ...) {
  mdl <- getTrajectoryModel(x)
  varNames <- getModelVarNames(mdl)
  names(varNames) <- varNames
  vars <- vars[1:2]
  vars <- varNames[vars]
  if(missing(xlab))
    xlab <- vars[1]
  if(missing(ylab))
    ylab <- vars[2]
  if(length(varNames)<2) {
    ylab <- varNames
    xlab <- 'time'
  }
  cG <- as.grob(x, vars=vars, type=type, ...)
  pG <- plotGrob(cG, axes=axes, main=main, xlab=xlab, ylab=ylab, mar=mar, bty=bty)
  grid.draw(pG)
}
