
#' Vector of function
#'
#' @param ... the functions that compose the vector. Attention they must have all the same argument
#'
#' @export
#'
#' @examples
#'f <- function(x,y) x*y
#'g <- function(x,y) x+y
#'h <- function(x,y) c(2*x, 2*y)
#'S <- fct_vector(f,g,h)
#'S(1, 2, i = c(1,2))
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
