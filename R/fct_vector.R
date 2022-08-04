#' Return the good index for each component of a fct_vector
#'
#' @param dim number of component
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

#' @title Vector of function
#' @name fct_vector
#'
#' @description The purpose of this object is to provide a new type of numeric array whose components are calculated if and only their access was requested.
#'So for example if one of the components is costly and rarely called this will not impact the rest of the calculations.
#'
#'
#' @field fct list. Several name or unnamed functions that compose the vector.  Warning they must have all the same argument. They must return a numeric object, no length limited
#' @field dimention list. The length of the numeric return of each function. Default value assume that of function return a one dimension numeric.
#'
#' @examples
#'require(SAEM4JLS)
#'vec <- fct_vector(function(x,y) x*y,
#'                  function(x,y) x+y,
#'                  function(x,y) c(2*x, 2*y), dim = c(1,1, 2))
#'vec$eval(1, 2, i = c(1,2))
#'vec[1,2](1,2)
#'#Warning: the vector’s extract operators [ do not return a fct_vector but the eval function of the extracted vector
#'#If you want the extracted vector use the list’s extract operator [[
#'G <- vec[1,2] # is a function
#'G(1,2)
#'
#'H <- vec[[1,2]] # is a fct_vector
#'H$eval(1,2)
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


setMethod('length', signature = 'fct_vector', definition = function(x) length())

setMethod('[', signature = c('fct_vector', 'numeric'), definition = function(x, i) x$subset(i)$eval)

setMethod('[[', signature = c('fct_vector', 'numeric'), definition = function(x, i) x$subset(i))

setGeneric('as.fct_vector', def = function(list, dim) standardGeneric("as.fct_vector" ))
setMethod('as.fct_vector', signature = c('list', 'numeric'), definition = function(list, dim){

  g <- function(..., dim) new('fct_vector', ..., dim = dim)

  names(dim) <- NULL
  do.call(g, c(list, list(dim = dim) ))
})

# `[.fct_vector` <- function(vec, i) vec$subset(i)$eval

#Petite fonction pour retourner rapidement l'appel Phi[attr(Phi, i)] où i est '1', '2', ...,
`%a%` <- function(x,var){
  if(length(var)== 1) return(x[ attr(x,as.character(var)) ])
  lapply(var, function(v) x%a%v) %>% unlist
}

setMethod("+", c("fct_vector", "fct_vector"), function(e1, e2) {

  if(length(e1$fct) == 0) return(e2)

  dim <- c(sapply(e1$dimention, length),
           sapply(e2$dimention, length))

  names(dim) <- NULL

  as.fct_vector(c(e1$fct, e2$fct), dim = dim)
})

setMethod('print', "fct_vector", function(x){
  print(x$fct)


  print('dimention : ')
  print(unlist(x$dimention))

})

setMethod('show', "fct_vector", function(object){
  print(object$fct)


  print('dimention : ')
  print(unlist(object$dimention))

})



