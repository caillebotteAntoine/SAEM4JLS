
rename_fct_expression <- function(fct, new.names)
{
  old.names <- names(new.names)#names(formals(fct))

  #argument rename
  tmp <- names(formals(fct))
  for (i in 1:length(tmp)) {
    if( tmp[[i]] %in% names(new.names ) )
      tmp[[i]] <- new.names[[tmp[[i]]]]
  }
  names(formals(fct)) <- tmp

  #body rename
  fct.body <- deparse(body(fct))

  for (j in 1:length(fct.body))
  {
    for (i in 1:length(new.names)) {
      fct.body[j] = gsub(old.names[i], new.names[[i]], fct.body[j], fixed = TRUE)
    }
  }

  body(fct) <- parse(text = fct.body) #analyse du texte
  return(fct)
}



latent_variable <- setClass(
  Class = "latent_variable",
  slots = list( name = 'character',
                nrow = 'numeric', ncol = 'numeric',
                dim.name = 'list',
                prior = 'list',
                add_on = 'character'),

  prototype = prototype(add_on = character() )

)

setMethod('initialize', 'latent_variable', function(.Object, name, nrow, ncol, prior, add_on){

  # if(!'ncol' %in% names(dim))
  #   warning('ncol value missing in the dim list !')
  # if(!'nrow' %in% names(dim))
  #   warning('nrow value missing in the dim list !')

  .Object@name <- name
  .Object@nrow <- nrow

  if(missing(ncol)) ncol = 1
  .Object@ncol <- ncol

  .Object@dim.name <- list(nrow = deparse(substitute(nrow)),
                           ncol = deparse(substitute(ncol)))

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
  # - n/(2*rho2), n*mu/rho2
  phi1 <- function(mean, variance, ...) nrow * mean/variance
  phi2 <- function(variance, ...) - nrow/(2*variance)

  phi1.hyper <- function(mean, ...) nrow * mean/variance
  phi2.hyper <- function(...) - nrow/(2*variance)

  if(var@ncol == 1)
  {
    S1 <- rename_fct_expression(function(X, ...) mean(X), list(X = var@name))
    S2 <- rename_fct_expression(function(X, ...) mean(X^2), list(X = var@name))

  }else{
    S1 <- rename_fct_expression(function(X, ...) apply(X, 2, mean), list(X = var@name))
    S2 <- rename_fct_expression(function(X, ...) apply(X^2, 2, mean), list(X = var@name))
  }

  environment(phi1) <- globalenv() ; environment(phi2) <- globalenv()
  environment(phi1.hyper) <- globalenv() ;   environment(phi2.hyper) <- globalenv()
  environment(S1) <- globalenv() ; environment(S2) <- globalenv()


  if('variance' %in% names(var@prior))
  {
    phi1 <- rename_fct_expression(phi1, list(variance = var@prior$variance, nrow = var@dim.name$nrow))
    phi2 <- rename_fct_expression(phi2, list(variance = var@prior$variance, nrow = var@dim.name$nrow))
  }

  if('mean' %in% names(var@prior))
  {
    phi1 <- rename_fct_expression(phi1, list(mean = var@prior$mean))
    phi2 <- rename_fct_expression(phi2, list(mean = var@prior$mean))
    phi1.hyper <- rename_fct_expression(phi1.hyper, list(mean = var@prior$mean))
    phi2.hyper <- rename_fct_expression(phi2.hyper, list(mean = var@prior$mean))


    if('variance.hyper' %in% names(var@prior))
    {
      phi1.hyper <- rename_fct_expression(phi1.hyper, list(variance = var@prior$variance.hyper, nrow = var@dim.name$nrow))
      phi2.hyper <- rename_fct_expression(phi2.hyper, list(variance = var@prior$variance.hyper, nrow = var@dim.name$nrow))
      Phi <- list(phi1.hyper, phi2.hyper)
    }else{
      Phi <- list(phi1, phi2)
    }
    S <- list(S1, S2)
    dim <- c(var@ncol, var@ncol)

  }else{
    Phi <- list(phi2)
    S <- list(S2)
    dim <- var@ncol
  }

  list(Phi = as.fct_vector(Phi, dim),
       S = as.fct_vector(S, dim) )
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














