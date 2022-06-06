
rm(list = ls())

fct_vector <- setRefClass(
  Class = "fct_vector",
  fields = list(fct = 'list',
                dimention = 'list'),

  methods = list(
    initialize = function(..., dim = NULL)
    {
      fct <<- list(...)

      if(is.null(dim)) dim <- rep(1, length(fct))

      if(!'list' %in% class(dim)) dimention <<- get_dimension_index(dim)
      else{
        dimention <<- get_dimension_index(sapply(dim, function(i)length(i)))
        names(dimention) <<- names(dim)
      }
    },

    show = function() {print(fct) ; print(dimention) },

    eval = function(..., i= NULL){
      args <- list(...)
      if(length(i) == 1) return( do.call(fct[[i]], args) )
      if(is.null(i))
      {
        s <- .self$eval(..., i = 1:length(fct) )
        attributes(s) <- dimention
        return(s)
      }
      unlist( lapply(i, function(j) do.call(fct[[j]], args)) )
    },

    subset = function(i){
      res <- copy()

      dim <- sapply(i, function(j) length(res$dimention[[j]]))
      res$dimention <- get_dimension_index(dim)
      names(res$dimention) <- i

      res$fct <- lapply(i, function(j) res$fct[[j]])
      res
    }
  )#end methods


)



`[.fct_vector` <- function(vec, i) vec$subset(i)$eval


get_dimension_index <- function(dim)
{
  if(length(dim) == 0)return(list())
  dimention <- list()
  d.last <- 0
  for(i in 1:length(dim))
  {
    dimention[[as.character(i)]] <- d.last + 1:dim[i]
    d.last <- d.last + dim[i]
  }
  return(dimention)
}


G <- fct_vector(function(x,y) x*y,
                function(x,y) x+y,
                function(x,y) c(2*x, 2*y),
                function(x,y) x^y, dim = c(1,1,2,1) )

G$eval(2,3, i = c(1,2))

G$eval(2,3)
G$dimention


G$copy()

H <- G$subset(c(1,3))
H$dimention

H$eval(2,3)

G[c(1,3)](2,3)



