
#' Vector of function
#'The purpose of this object is to provide a new type of numeric array whose components are calculated if and only their access was requested.
#'So for example if one of the components is costly and rarely called this will not impact the rest of the calculations.
#'
#'
#' @param ... Several name or unnamed functions that compose the vector.  Warning they must have all the same argument. They must return a numeric object, no length limited
#' @param dim the length of the numeric return of each function
#'
#' @export
#'
#' @examples
#'f <- function(x,y) x*y
#'g <- function(x,y) x+y
#'h <- function(x,y) c(2*x, 2*y)
#'vec <- fct_vector(f,g,h, dim = 1,1, 2)
#'vec(1, 2, i = c(1,2))
#'vec[1,2](1,2)
#'
fct_vector_old <- function(..., dim = NULL)
{
  S <- function(..., i= NULL){
    args <- list(...)
    if(length(i) == 1) return( do.call(attr(S, 'fct')[[i]], args) )
    if(is.null(i))
    {
      s <- S(..., i = 1:length(attr(S,'fct')) )
      attributes(s) <- attr(S, 'dimention')
      return(s)
    }
    unlist( lapply(i, function(j) do.call(attr(S,'fct')[[j]], args)) )
  }

  attr(S,'fct') <- list(...)

  if(is.null(dim)) dim <- rep(1, length(attr(S,'fct')))
  attr(S, 'dimention') <- get_dimension_index(dim)

  class(S) <- c('fct_vector', class(S))
  return(S)
}
