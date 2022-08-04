
SAEM_model <- setClass(
  Class = "SAEM_model",
  package = 'SAEM4JLS',
  slots = list( Phi.noise = 'function',
                S.noise = 'function',
                noise.name = 'character',
                Phi = 'fct_vector',
                S = 'fct_vector',

                latent_vars = 'list',
                regression.parameter = 'list',

                loglik.fct = 'list'),
)

setMethod('initialize', 'SAEM_model', function(.Object, Phi.noise, S.noise, noise.name, latent_vars, regression.parameter){
  if(missing(regression.parameter)) regression.parameter <- list()
  names(regression.parameter) <- sapply(regression.parameter, function(v) v@name)
  .Object@regression.parameter <- regression.parameter

  .Object@Phi.noise <- Phi.noise
  .Object@Phi <- fct_vector(Phi.noise)

  .Object@S.noise <- S.noise
  .Object@S <- fct_vector(S.noise)

  .Object@noise.name <- noise.name

  #=========================================#
  # === Construction of loglik function === #
  #=========================================#
  variable_linked_in_noise <- formals(S.noise) %>% names %>% { gsub('[[:digit:]]+', '', .) } %>% unique
  loglik.template <- function(x, ..., Phi)
  {
    sum( Phi %a% c(id) * S$eval(var = x, ..., i = c(id)) )
  }
  environment(loglik.template) <- globalenv()

  .Object@loglik.fct <- list()
  last.id <- 1

  if(missing(latent_vars)) latent_vars <- list()
  names(latent_vars) <- sapply(latent_vars, function(v) v@name)
  .Object@latent_vars <- latent_vars

  for(var in .Object@latent_vars)
  {
    l <- get_complet_log_likelihood(var)
    .Object@Phi <- .Object@Phi + l$Phi
    .Object@S <- .Object@S + l$S

    id <- paste0(Reduce("c", as.numeric(names(l$S$dimention)) ) + last.id, collapse = ',')
    if(var@name %in% variable_linked_in_noise)
    {
      id <- paste0('1,', id)
    }

    # === === #
    if(var@size == 1)
    {
      tmp <- rename_fct_expression(loglik.template, list(id = id) )
      tmp <- rename_fct_argument(tmp, list(x = var@name))
      tmp <- gsub_fct_expression(tmp, c('var = x'), c(paste0(var@name, ' = ', var@name)))

    }else{
      dim <- 1:var@size

      tmp <- rename_fct_expression(loglik.template, list(id = id) )
      tmp <- rename_fct_argument(tmp, list(x = paste0(var@name, 1)))
      for(i in 2:var@size)
        tmp <- add_arguement(tmp, paste0(var@name, i), paste0(var@name, i-1))

      tmp <- gsub_fct_expression(tmp, c('var = x'),
                                 c(paste0(var@name, dim, ' = ', var@name, dim, collapse = ', ') ))
    }

    .Object@loglik.fct[[var@name]] <- tmp

    # === Add additional functions === #
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
    for(var in .Object@latent_vars)
    {
      .Object@loglik.fct[[var@name]] <- add_arguement(.Object@loglik.fct[[var@name]], para@name, 'Phi')
      .Object@loglik.fct[[var@name]] <- gsub_fct_expression(.Object@loglik.fct[[var@name]],
                                        '...',
                                        paste0('...,', para@name, '= ', para@name))
    }
  }

  return(.Object)
})





setMethod('show', "SAEM_model", function(object ){
  load.SAEM(object)

  print(object@Phi)
  print(object@S)
  print('Maximisation step')
  print(max)
  print('Simulation step')
  print(sim)
})


get_maximisation_step <- function(model, exclude = c())
{

  maximisation <- function(S, parameter, Z)
  {
  }
  fct.body <- deparse(body(maximisation))

  i = 1

  fct.body[i+1] <- paste0('res <- list(', model@noise.name, ' = S%a%1')
  k <- 1

  # === Latent variables === #
  for(var in model@latent_vars)
  {
    for(p in names(var@prior))
    {
      if(!var@prior[[p]] %in% exclude){
      #Si on a pas exclue ce paramètre

        if(p != 'variance.hyper'){
          fct.body[i+1] <- paste0(fct.body[i+1], ',')
          i <- i + 1
        }

        if(var@size == 1){
          if(p == 'mean'){
            fct.body[i+1] <- paste0(var@prior[[p]], ' = S%a%', k + 1)
          }else if(p == 'variance'){
            fct.body[i+1] <- paste0(var@prior[[p]], ' = S%a%', k + 1 + as.numeric('mean' %in%names(var@prior)))
            if('mean' %in% names(var@prior))
              fct.body[i+1] <- paste0(fct.body[i+1], ' - (S%a%', k + 1, ')^2')
          }
        }else{
          if(p == 'mean'){
            fct.body[i+1] <- paste0(var@prior[[p]], ' = S%a%c(', paste0( k - 1 + 2*1:var@size, collapse = ', '), ')')
          }else if(p == 'variance'){
            fct.body[i+1] <- paste0(var@prior[[p]], ' = S%a%c(', paste0( k  -1 +as.numeric('mean' %in%names(var@prior))+ 2*1:var@size, collapse = ', '), ')')
            if('mean' %in% names(var@prior))
              fct.body[i+1] <- paste0(fct.body[i+1], ' - (S%a%c(', paste0( k - 1 + 2*1:var@size, collapse = ', '), '))^2')
          }
        }

      }
    }
    k <- k + length(var@prior)*var@size
  }

  fct.body[i+1] <- paste0(fct.body[i+1], ')')


  i <- i + 1
  # === Regression parameter === #
  for(para in model@regression.parameter)
  {
      # i <- i + 1
      # fct.body[i] <- paste0('    ',para@name, ' <- parameter$', para@name  )
    if(!para@name %in% exclude)
    {
      #Add Maximization step
      fct <- para@maximization.function %>%
        gsub_fct_expression(para@name, paste0('parameter$', para@name))

      para.body <- deparse(body(fct))
      para.body[1] <- paste0('    res$', para@name, ' <- ', para.body[1])

      for(j in 1:length(para.body))
      {
        i <- i + 1
        fct.body[i] <- para.body[j]
      }

    }
  }

  for(name in exclude)
  {
    i <- i + 1
    fct.body[i] <- paste0('    res$', name, ' <- parameter$', name  )
  }

  fct.body[i+1] <- 'return(res)'
  fct.body[i+2] <- '}'

  body(maximisation) <- parse(text = fct.body)

  environment(maximisation) <- globalenv()
  return(maximisation)
}



get_simulation_step <- function(model, exclude = c())
{
  if(length(exclude) != 0 ){
    id <-
      names(model@latent_vars) %>% lapply(function(v){
        if(model@latent_vars[[v]]@size == 1){
          return(model@latent_vars[[v]]@name)
        }else{
          return(paste0(model@latent_vars[[v]]@name, 1:model@latent_vars[[v]]@size))
        }}) %>% unlist

    new.id <- paste0('c(', paste0(which(!id %in% exclude), collapse = ','), ')')

    simulation <- gsub_fct_expression(simulation, '1:length(var)', new.id)
    }
  for(para in model@regression.parameter)
  {
    # simulation <- add_arguement(simulation, para@name, 'Phi')
    simulation <- gsub_fct_expression(simulation,
                        'Phi = Phih',
                        paste0('Phi = Phih', ',', para@name, '= parameter$', para@name))

  }
  return(simulation)
}

load.SAEM <- function(model, exclude.simulation = c(), exclude.maximisation = c(), env = globalenv())
{
  assign('Phi', model@Phi$eval, envir = env)
  assign('S', model@S, envir = env)

  assign('max', get_maximisation_step(model, c(exclude.simulation, exclude.maximisation)), envir = env)
  assign('sim', get_simulation_step(model, exclude.simulation), envir = env)

  for(var in names(model@loglik.fct)) assign(paste0('loglik.',var), model@loglik.fct[[var]], envir = env)
}

init.SAEM <- function(model, x0, sd)
{
  var <- list()

  names(names(sd) == names(model@latent_vars))
  stopifnot(names(sd) == names(x0))

  var.name <- c()
  for(v in names(x0))
  {
    stopifnot(length(x0[[v]]) == length(x0[[v]]))
    stopifnot(model@latent_vars[[v]]@size == length(x0[[v]]))

    if(length(x0[[v]]) == 1){
      fct <- rename_fct_expression(function(x, ...) loglik(var = x, ...), list(loglik = paste0('loglik.', v), var = v))

      var <- c(var, list(chain(rep(x0[[v]], model@latent_vars[[v]]@dim), sd = sd[[v]], propto_distrib_fct = fct ) ))
      var.name <- append(var.name, v)
    }else{
      for(i in 1:length(x0[[v]]))
      {
        fct <- rename_fct_expression(function(x, ...) loglik(var = x, ...), list(loglik = paste0('loglik.', v), var = paste0(v, i)))

        var <- c(var, list(chain(rep(x0[[v]][i], model@latent_vars[[v]]@dim), sd = sd[[v]][i], propto_distrib_fct = fct ) ))
      }
      var.name <- append(var.name, paste0(v, 1:model@latent_vars[[v]]@size))
    }
  }

  names(var) <- var.name

  class(var) <- c('list', 'chain')
  return(var)
}




