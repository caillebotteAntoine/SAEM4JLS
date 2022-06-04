
#' Exhaustive statistics
#'
#' @param ... the functions that make up the exhaustive statistics. Attention they must have all the same argument
#' @export
get_stat <- function(...)
{
  S <- list(...)

  S$evaluate <- function(i, ...){
    args <- list(...)
    if(length(i) == 1) return(do.call(S[[i]], args))
    lapply(i, function(j) do.call(S[[j]], args)) %>% unlist
  }

  class(S) <- c('fct_vector', class(S))
  return(S)
}
