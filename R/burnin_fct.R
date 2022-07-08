



#' Burn-in function for algorithm requiring a step
#'
#' @param burnin
#' @param coef.burnin
#' @param scale
#'
#' @return
#' @export
#'
#' @examples
#'
#'u <- burnin_fct(10, 0.1)
#'plot(1:20, u(1:20))
#'
#'v <- burnin_fct(10, 3/4, 0.5)
#'plot(1:20, u(1:20))
#'plot(1:20, v(1:20))
burnin_fct <- function(burnin, coef.burnin, scale = 1)
{
  function(k) scale * ifelse(k<burnin, 1, 1/((k+1-burnin)^coef.burnin ))
}


