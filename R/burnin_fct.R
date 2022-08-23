



#' Burn-in function for algorithm requiring a step
#'
#' @param burnin
#' @param coef.burnin
#' @param scale start value
#' @param format int or double
#'
#' @return
#' @export
#'
#' @examples
#'
#'u <- burnin_fct(10, 2)
#'plot(u)
#'
#'v <- burnin_fct(10, 3/4, 0.5)
#'plot(v)
burnin_fct <- function(burnin, coef.burnin, scale = 1, format = "double")
{
  if(format == 'double')
  {
    f <- function(k) scale * ifelse(k<burnin, 1, 1/((k+1-burnin)^coef.burnin ))
    class(f) <- c('burnin_fct', class(f))
    return(f)
  }

  if(format == 'int')
  {
    f <- function(k) round(scale * ifelse(k<burnin, 1, 1/((k+1-burnin)^coef.burnin )) )
    class(f) <- c('burnin_fct', class(f))
    return(f)
  }
}

plot.burnin_fct <- function(f, eps = 0.05)
{
  e <- environment(f)

  burnin <- e[['burnin']]
  scale <- e[['scale']]

  upper <- burnin
  while( (f(upper) - f(upper+1))/scale > scale*eps) upper <- upper+1

  lower <- burnin - ceiling((upper - burnin)*0.1)

  x <- seq(lower, upper, by = 0.1)
  plot(x, f(x))

}







