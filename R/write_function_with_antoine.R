
as_function <- function(f)
{
  if(is.function(f))return(f)

  template <- function(k) return
  gsub_fct_expression(template,
                      'return',
                      deparse(substitute(f)) )
}


add_arguement <- function(fct, new.arg, after)
{
  if(length(new.arg) != 1){
    for(i in 1:length(new.args)){
      fct <- add_arguement(fct, new.arg[[i]], after)
    }
    return(fct)
  }
  mid <- which(names(formals(fct)) == after)

  arg <- formals(fct)
  formals(fct) <- c(arg[1:mid], alist(p=))
  new.name <- c(names(arg)[1:mid], new.arg)
  if(mid < length(arg))
  {
    formals(fct) <- c(formals(fct), arg[(mid+1):length(arg)])
    new.name <- c(new.name, names(arg)[(mid+1):length(arg)])
  }

  names(formals(fct)) <- new.name

  return(fct)
}

remove_arguement <- function(fct, exclude)
{
  formals(fct) <-  formals(fct) %>% {.[-which(names(.) %in% exclude)]}
  return(fct)
}


gsub_fct_expression <- function(fct, pattern, replacement)
{
  stopifnot(length(pattern) == length(replacement))

  # for (i in 1:length(replacement))
  # {
  #   if(pattern[i] %in% names(formals(fct)))
  #   {
  #     if( grepl(',',  replacement[i]) )
  #     {
  #       formals(fct) <- formals(fct)[-which(names(formals(fct)) == pattern[i])]
  #
  #       new.arg <- strsplit(replacement[i], ',')[[1]]
  #       #add_arguement(fct, new.arg)
  #       for(arg in new.arg)
  #       {
  #         formals(fct) <- c( alist(p=), formals(fct) )
  #         names(formals(fct))[1] <- arg
  #       }
  #
  #     }else{
  #       names(formals(fct))[which(names(formals(fct)) == pattern[i])] <- replacement[i]
  #     }
  #
  #   }
  # }
  #body rename
  fct.body <- deparse(body(fct))
  for (j in 1:length(fct.body))
  {
    for (i in 1:length(pattern)) {
      fct.body[j] = gsub(pattern[i], replacement[i], fct.body[j], fixed = TRUE)
    }
  }

  body(fct) <- parse(text = fct.body) #analyse du texte
  return(fct)
}

rename_fct <- function(fct, new.names)
{
  fct <- rename_fct_expression(fct, new.names)
  return( rename_fct_argument(fct, new.names) )
}

rename_fct_argument <- function(fct, new.names)
{
  old.names <- names(new.names)#names(formals(fct))

  #argument rename
  tmp <- names(formals(fct))
  for (i in 1:length(tmp)) {
    if( tmp[[i]] %in% names(new.names ) )
      tmp[[i]] <- new.names[[tmp[[i]]]]
  }
  names(formals(fct)) <- tmp

  return(fct)
}
rename_fct_expression <- function(fct, new.names)
{
  old.names <- names(new.names)#names(formals(fct))

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
