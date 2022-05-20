

#' plot the result of the SAEM funciton
#'
#' @param res
#' @param MCMC
#'
#' @return
#' @export
#'
#' @examples
plot.SAEM <- function(res, MCMC = T)
{
  gg <- list()

  dt <- res$parameter %>% as.data.frame %>% na.omit

  #Resultat de l'estimations
  est <- rbind( param %>% unlist, dt[nrow(dt),]) %>%
    t %>% data.frame

  names(est) <- c('Valeur réelle', 'Valeur estimée')

  est <- est %>%
    mutate( Rrmse = abs(`Valeur réelle` -`Valeur estimée`)/abs(`Valeur réelle`))

  gg$table_estimation <-
    est %>% t %>%
      knitr::kable(caption = "résultat de l'algo SAEM-MCMC", format = 'pipe') %>%
      kable_styling(full_width = F)

  # MCMC des paramètres
  gg$plot_paramater <-
    dt %>% mutate(i = 1:nrow(.)) %>% melt(id = 'i') %>%
      mutate(hline = param %>% unlist %>% rep(each = nrow(dt)) ) %>%# filter( i < 25) %>%
      ggplot(aes(i, value, col = variable)) + geom_line() +
      geom_hline(aes(yintercept = hline, col = variable), linetype='dashed') +
      labs(title = "représentation de l'algorithme SAEM pour les paramètres",
           x = 'iteration') +  facet_grid( vars(variable), scales = 'free')

  if(!is.null( attr(res$parameter, 'stop')))
  {
    gg$plot_paramater <- gg$plot_paramater +
      geom_vline(xintercept = attr(res$parameter, 'stop'), size = 1, col = 'red')
  }


  if(MCMC && !is.null( attr(res$Z, 'value')))
  {
    # MCMC de Z
    v <- attr(res$Z, 'value')
    dt <- data.frame()
    for(var in names(v))
    {
      dt <- rbind(dt,
                  1:length(v[[var]]) %>%
                    lapply(function(i) cbind(i, v[[var]][[i]], row = 1:length(v[[var]][[i]])) ) %>%
                    {do.call(rbind, .) } %>% as.data.frame %>%
                    melt(id = c('i','row')) %>% mutate(col = variable, variable = factor(var)) )
    }

    gg$plot_MCMC <-
      dt %>% ggplot(aes(i, value, group = interaction(row, col, variable), color = col)) +
        geom_line() +
        labs(title = "représentation des variables latentes de l'algorithme SAEM ",
             x = 'iteration') + facet_grid( vars(variable), scales = 'free')
  }

  gg
}


