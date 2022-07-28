


latent_variable <- setClass(
  Class = "latent_variable",
  slots = list( name = 'character',
                dim = 'numeric',
                dim.name = 'character',
                size = 'numeric',
                prior = 'list',
                add_on = 'character'),

  prototype = prototype(add_on = character() )

)

setMethod('initialize', 'latent_variable', function(.Object, name, dim = 1, size = 1, prior, add_on){
  .Object@name <- name
  .Object@dim <- dim
  .Object@size <- size

  .Object@dim.name <- deparse(substitute(dim))

  if(!'variance' %in% names(prior) && !'variance.hyper' %in% names(prior))
  {
    warning('variance value missing in the prior list !')
  }
  .Object@prior <- prior

  if(!missing(add_on))
    .Object@add_on <- add_on

  return(.Object)
})


get_complet_log_likelihood <- function(var)
{
  if(var@size != 1)
  {
    name <- var@name
    S <- fct_vector()
    Phi <- fct_vector()
    size <- var@size
    for(i in 1:size)
    {
      var@name <- paste0(name, i)
      var@size <- 1

      res <- get_complet_log_likelihood(var)
      S <- S + res$S

      new.name <- var@prior %>% lapply(function(n) paste0(n,'[',i,']'))
      names(new.name) <- unlist(var@prior)
      dim <- res$Phi$dimention %>% sapply(length)

      Phi <- Phi + res$Phi$fct %>% lapply(rename_fct_expression , new.name) %>% as.fct_vector(dim = dim)
    }

    return(list(Phi = Phi, S = S))
  }


  # === Phi === #
  # - n/(2*rho2), n*mu/rho2
  Phi = list(function(mean, variance, ...) dim * mean/variance,
             function(variance, ...) - dim/(2*variance) )

  Phi <- lapply(Phi, function(f) {environment(f) <- globalenv() ; f} )
  # === S === #
  S <- list(rename_fct(function(X, ...) mean(X), list(X = var@name)),
             rename_fct(function(X, ...) mean(X^2), list(X = var@name)) )

  S <- lapply(S, function(f) {environment(f) <- globalenv() ; f} )

  # === Rename variable === #
  rename <- list(dim = var@dim.name)

  if('mean' %in% names(var@prior))
  {
    rename$mean <- var@prior$mean
  }else{
    # remove compotnent one
    Phi <- Phi[-1]
    S <- S[-1]
  }

  if('variance' %in% names(var@prior)) rename$variance <- var@prior$variance
  if('variance.hyper' %in% names(var@prior))
  {
    rename$variance <- var@prior$variance.hyper
    Phi <- lapply(Phi, remove_arguement, exclude = 'variance')
  }

  list(Phi = lapply(Phi, rename_fct, rename) %>% as.fct_vector(dim = rep(1,length(Phi)) ),
       S = as.fct_vector(S, dim = rep(1,length(S)) ) )
}









regression_parameter <- setClass(
  Class = "regression_parameter",
  slots = list( name = 'character',
                dim = 'numeric',
                maximization.function = 'function')
)

setMethod('initialize', 'regression_parameter', function(.Object, name, dim, maximization.function){

  .Object@name <- name
  .Object@dim <- dim
  .Object@maximization.function <- maximization.function

  return(.Object)
})














