

SAEM_plot <- function(res)
{
  dt <- res$parameter %>% as.data.frame %>% na.omit

  #Resultat de l'estimations
  est <- rbind( param %>% unlist, dt[nrow(dt),]) %>%
    t %>% data.frame

  names(est) <- c('Valeur réelle', 'Valeur estimée')

  est <- est %>%
    mutate( Rrmse = abs(`Valeur réelle` -`Valeur estimée`)/abs(`Valeur réelle`))

  print(
  est %>% t %>%
    knitr::kable(caption = "résultat de l'algo SAEM-MCMC") %>%
    kable_styling(full_width = F) )

  # MCMC des paramètres
  plot(
  dt %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i') %>%# filter( i < 25) %>%
    ggplot(aes(i, value, col = variable)) + geom_line() +
    labs(title = "représentation de l'algorithme SAEM pour les paramètres",
         x = 'iteration') )

  # MCMC de Z
  v <- attr(res$Z, 'value')[[1]] %>% lapply(function(x) as.numeric(x)) %>% as.data.frame %>% t %>% as.data.frame()

  plot(
  v %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i') %>%
    ggplot(aes(i, value, col = variable)) + theme(legend.position = 'null') +
    geom_line() +
    labs(title = "représentation des variables latentes de l'algorithme SAEM ",
         x = 'iteration') )
}


