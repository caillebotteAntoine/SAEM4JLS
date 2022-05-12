include = function(folder = 'R/', files, env.name )
{
  if(missing(env.name))
  {
    env <- .GlobalEnv
  }else{
    if(exists(env.name, envir = .GlobalEnv))
    {
      detach(get(env.name, envir = .GlobalEnv))
      rm(list = c(env.name))
    }

    env = new.env()
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


  #print(paste('new environment create named', env.name))
  #rm(list = c(env.name))
}

include()
#rm(include)


