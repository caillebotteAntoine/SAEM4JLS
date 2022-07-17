
SAEM_model <- setClass(
  Class = "SAEM_model",
  slots = list( Phi.noise = 'function',
                S.noise = 'function',
                Phi = 'fct_vector',
                S = 'fct_vector',

                latent_vars = 'list',
                regression.parameter = 'list',

                loglik.fct = 'list'),

  prototype = prototype(latent_vars = list(), regression.parameter = list() )

)

setMethod('initialize', 'SAEM_model', function(.Object, Phi.noise, S.noise, latent_vars, regression.parameter){
  .Object@regression.parameter <- regression.parameter

  .Object@Phi.noise <- Phi.noise
  .Object@Phi <- fct_vector(Phi.noise)

  .Object@S.noise <- S.noise
  .Object@S <- fct_vector(S.noise)


  loglik.template <- function(x, ..., Phi)
  {
    sum(Phi %a% id*S$eval(var = x, ..., i = id))
  }
  environment(loglik.template) <- globalenv()

  .Object@loglik.fct <- list()
  last.id <- 1
  .Object@latent_vars <- latent_vars
  for(var in .Object@latent_vars)
  {
    l <- get_complet_log_likelihood(var)
    .Object@Phi <- .Object@Phi + l$Phi
    .Object@S <- .Object@S + l$S

    id <- paste0(Reduce("c", as.numeric(names(l$S$dimention)) ) + last.id, collapse = ',')

    .Object@loglik.fct[[var@name]] <- rename_fct_expression(loglik.template, list(var = var@name, id = paste0('c(1,', id, ')') ) )

    if(length(var@add_on) != 0)
    {
      fct.body <- deparse(body(.Object@loglik.fct[[var@name]]))

      i <- length(fct.body)-1
      fct.body[i] <- paste0(fct.body[i], ' + ')
      i <- i + 1
      for(addon in var@add_on)
      {
        fct.body[i] <- paste0('        ', addon)
        i <- i + 1
      }
      fct.body[i] <- '}'
      body(.Object@loglik.fct[[var@name]]) <- parse(text = fct.body)
    }


    last.id <- last.id + l$S$dimention %>% names %>% as.numeric %>% { max(.[[length(.)]]) }
  }

  for(para in .Object@regression.parameter)
  {
    .Object@Phi <- .Object@Phi + fct_vector(function(beta, ...) beta)
    .Object@S <- .Object@S + fct_vector(function(beta, ...) beta)
  }


  return(.Object)
})



load.SAEM <- function(model, exclude.simulation = c(), env = globalenv())
{
  assign('Phi', model@Phi$eval, envir = env)
  assign('S', model@S, envir = env)

  assign('sim', get_simulation_step(model, exclude.simulation), envir = env)

  for(var in names(model@loglik.fct)) assign(paste0('loglik.',var), model@loglik.fct[[var]], envir = env)

}












get_simulation_step <- function(model, exclude = c())
{
  fct <- function(niter, h)
  {
    args <- list(Phi = Phih)
  }
  fct.body <- deparse(body(fct))
  first.body.line <- '    args <- list('

  k <- 1
  i <- 2
  for(var in model@latent_vars)
  {
    if(!var@name %in% exclude)
    {
      #Add simulation step
      fct.body[i + 1] <- paste0('    args$',var@name,' <- MH_Gibbs_Sampler_future(niter, args$', var@name,', sd = sd.', var@name, ',')
      fct.body[i + 2] <- paste0('        loglik.', var@name, ', args[', -k, '], cores = 1, verbatim = verbatim)' )

      i <- i + 2
    }
    #Add arguement
    args <- c(formals(fct), alist(tmp =))
    names(args)[length(names(args))] <- var@name
    formals(fct) <- args
    first.body.line <- paste0(first.body.line, var@name, ' = ', var@name, '[[1]], ')
    k <- k + 1

  }

  i <- i + 2
  for(para in model@regression.parameter)
  {
    if(!para@name %in% exclude)
    {
      #Add Maximization step
      para.body <- deparse(body(para@maximization.function))
      para.body[1] <- paste0('    args$', para@name, ' <- ', para.body[1])

      for(j in 1:length(para.body))
      {
        i <- i + 1
        fct.body[i] <- gsub('\\.\\.\\.', paste0('args[', -k, ']'), para.body[j])

      }
    }
    #Add arguement
    args <- c(formals(fct), alist(tmp =))
    names(args)[length(names(args))] <- para@name
    formals(fct) <- args
    first.body.line <- paste0(first.body.line, para@name, ' = ', para@name, '[[1]], ')
    k <- k + 1

  }

  fct.body[2] <- paste0(first.body.line, 'Phi = Phih)')
  fct.body[length(fct.body) + 1] <- 'args$Phi <- NULL'
  fct.body[length(fct.body) + 1] <- '    return( lapply(args, function(var) list(var)) )'
  fct.body[length(fct.body) + 1] <- "}"

  body(fct) <- parse(text = fct.body)
  formals(fct) <- c(formals(fct), alist(Phih =, verbatim = F))

  environment(fct) <- globalenv()

  return(fct)
}

