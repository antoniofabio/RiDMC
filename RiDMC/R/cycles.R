cycles_find <- function(idmc_model, par, var, period, eps, max.iter=100) {
  checkModelParVar(idmc_model, par, var, deparse(substitute(idmc_model)))
  checkPositiveScalar(eps)
  checkPositiveScalar(max.iter)
  period <- as.integer(period)
  if(period<1)
    stop('\'period\' should be an integer >=1')
  ans <- .Call("ridmc_cycles_find", idmc_model$model, as.double(par), as.double(var), 
    period, as.double(eps), as.integer(max.iter), PACKAGE='RiDMC')
  ##verify that is a real solution:
  value <- ans$result
  for(i in seq_len(period))
    value <- idmc_model$f(par, value)
  ##if not, clean out results:
  if(max(abs(value-ans$result))>eps)
    ans <- NULL
  return(ans)
}
