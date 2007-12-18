Trajectory <- function(idmc_model, par, var, time=1, transient=0,
  seed, eps=getOption("ts.eps"), integrator=2) {
  m <- idmc_model
  checkModelParVar(m, par, var, deparse(substitute(idmc_model)))
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
    xyGrob(time(y), y, type=type, col=col, name='xy', ...)
  } else {
    xx <- as.matrix(x)[,vars]
    xyGrob(xx[,1], xx[,2], type=type, col=col, name='xy', ...)
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

TrajectoryList <- function(idmc_model, n=2, par, var, time=1, transient=0,
  seed, eps=getOption("ts.eps"), integrator=2) {
  argList <- expandArgList(n=n, par=par, var=var, time=time, transient=transient, eps=eps, integrator=integrator)
  argList <- lapply(argList, function(x) append(list(idmc_model=idmc_model), x))
  if(!missing(seed))
    argList <- lapply(argList, function(x) append(list(seed=seed), x))
  trList <- lapply(argList, do.call, what=Trajectory)
  class(trList) <- 'idmc_trajectoryList'
  trList
}

as.grob.idmc_trajectoryList <- function(x, colors, ...) {
  as.grob2 <- function(obj, color, ...)
    as.grob(obj, gp=gpar(col=color), ...)
  childs <- mapply(as.grob2, x, colors, SIMPLIFY=FALSE)
  xmin <- min(sapply(childs, function(x) min(Xlim(x))))
  xmax <- max(sapply(childs, function(x) max(Xlim(x))))
  ymin <- min(sapply(childs, function(x) min(Ylim(x))))
  ymax <- max(sapply(childs, function(x) max(Ylim(x))))
  childs <- mapply(function(x, nm) {x$name <- nm; x},
    childs,
    as.list(paste('xy', sep='.', seq_along(x))),
    SIMPLIFY=FALSE)
  childs <- do.call(gList, childs)
  contentsGrob(gTree(children=childs), xlim=c(xmin, xmax), ylim=c(ymin, ymax), respect=FALSE)
}

plot.idmc_trajectoryList <- function(x, y, vars=1:2, type='l', colors,
  main = getModelName(getTrajectoryModel(x[[1]])), xlab, ylab,
  mar = NULL, axes=TRUE, bty=TRUE, legend=FALSE, labels, ...) {
  if(missing(colors))
    colors <- seq_along(x)
  mdl <- getTrajectoryModel(x[[1]])
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
  if(legend) {
    if(missing(labels))
      labels <- as.character(seq_along(x))
    clg <- colorLegendGrob(colors, labels, y=unit(0, 'npc'), x=unit(0, 'npc'), name='legend')
    rightMargin <- convertWidth(widthDetails(clg), 'lines')
    mar <- c(4,4,4,rightMargin)
    mar[4] <- mar[4]*1.04
  }
  cG <- as.grob(x, colors=as.list(colors), vars=vars, type=type, ...)
  grid.draw(plotGrob(cG, main=main, xlab=xlab, ylab=ylab, axes=axes, mar=mar, bty=bty))
  if(legend) {
    downViewport('rightMarginArea')
    grid.draw(clg)
    upViewport(0)
  }
}
