
#' Exhaustive statistics
#'
#' @param ... the functions that make up the exhaustive statistics. Attention they must have all the same argument
#'
#' @export
#'
#' @examples
#'f <- function(x,y) x*y
#'g <- function(x,y) x+y
#'h <- function(x,y) c(2*x, 2*y)
#'S <- get_stat(f,g,h)
#'S(1, 2, i = c(1,2))
get_stat <- function(..., dim = NULL)
{
  S <- function(..., i= NULL){
    args <- list(...)
    if(length(i) == 1) return( do.call(attr(S, 'fct')[[i]], args) )
    if(is.null(i))  return( S(..., i = 1:length(attr(S,'fct')) ) )
    lapply(i, function(j) do.call(attr(S,'fct')[[j]], args))
  }

  attr(S,'fct') <- list(...)

  if(is.null(dim)) dim <- rep(1, length(attr(S,'fct')))
  dimention <- list()
  d.last <- 0
  for(i in 1:length(dim))
  {
    dimention[[as.character(i)]] <- d.last + 1:dim[i]
    d.last <- d.last + dim[i]
  }
  attr(S, 'dimention') <- dimention

  attr(S,'evaluate') <- function(...){
    s <- unlist( S(...) )
    attributes(s) <- attr(S, 'dimention')
    return(s)
  }

  class(S) <- c('fct.vector', class(S))
  return(S)
}

f <- function(x,y) x*y
g <- function(x,y) x+y
h <- function(x,y) c(2*x, 2*y)
S <- get_stat(f,g,h, dim = c(1,1,2))

S(1, 2, i = c(1,2))

attr(S, 'evaluate')(1, 2)













