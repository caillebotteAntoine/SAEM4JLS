include = function(folder = 'R/', files, env.name )
{
  if(missing(env.name))
  {
    env <- .GlobalEnv
  }else{
    #if(exists(env.name, envir = .GlobalEnv)) rm(list = env.name, envir = .GlobalEnv)
    if(env.name %in% search())
    {
      detach(env.name, character.only = T)
      print(paste(env.name, 'environment has been updated'))
    }
    assign(env.name, new.env(), envir = .GlobalEnv)
    env <- get(env.name, envir = .GlobalEnv)
    print(paste('new environment create named', env.name))
  }

  if(missing(files))
    files = dir(folder)
  else
    sapply(files, function(f) source(paste0(f,'.R')) )

  #Pour être sur de pas boucler on vérifie qu'on charge pas include.R
  files <- files[which(files != 'include.R')]

  load = function(f)
  {
    print(paste('load of',f))
    source(paste0(folder, f), local = env )
  }
  #On charge tout
  sapply(files, load)

  if(!missing(env.name))
  {
    attach(env, name = env.name)
    rm(list = env.name, envir = .GlobalEnv)
  }
  rm(include, envir = .GlobalEnv)
}


require(ggplot2) #Affichage
require(gridExtra) #grid.arrange pour regrouper des plot
require(reshape2) #reformatage de data.frame

require(dplyr)
require(tidyr)

require(purrr)
require(furrr)# calcule en parallel
require(future)

if(exists('folder')){
  include(folder, env.name = 'SAEM4JLS')
  rm(folder)
}else
  include(env.name = 'SAEM4JLS')



