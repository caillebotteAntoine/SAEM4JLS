

# rm(list = ls())

f <- function(res,v){
  x <- as_tibble(res[[v]]) %>% na.omit
  names(x) <- if(ncol(x) == 1) v else paste0(v, 1:ncol(x))
  x %>% mutate(iteration = 1:nrow(.))
}

g <- function(v) lapply(1:length(multi_run_data$res),
                        function(i) f(multi_run_data$res[[i]]$res_lasso, v) %>% mutate(id = i)) %>% reduce(rbind)

var <- names( multi_run_data$res[[1]]$res )
data <- lapply( var[-6], function(v) g(v) )


#dt <- g('mu')
data %>% melt(id = c('iteration', 'id')) %>%
  ggplot(aes(iteration, value, col = factor(variable),
             group = interaction(factor(variable),factor(id)) )) +
  geom_line() +
  facet_grid(vars(variable), scale = 'free')



#============================================================================#
require(SAEM4JLS)

f <- function(res,v){
  # if(v == 'beta') return(NA)
  x <- as_tibble(res[[v]]) %>% na.omit %>% {.[nrow(.),]}
  names(x) <- if(ncol(x) == 1) v else paste0(v, 1:ncol(x))
  x
}

g <- function(v) lapply(multi_run_data$res, function(x) f(x$res_lasso, v)) %>% reduce(rbind)
# g <- function(v) lapply(multi_run_data$res, function(x) f(x$res, v)) %>% reduce(rbind)

var <- names( multi_run_data$res[[1]]$res ) #%>% {.[-length(.)]}
data <- lapply( var, function(v) g(v) )
names(data) <- var


beta <- data$beta
data$beta <- NULL
dt <-  na.omit(as.data.frame(data))
names(dt) <- unlist( data %>% sapply(function(d) names(d)) )
names(dt) <- c("sigma2", "mu 1","mu 2","mu 3","omega2 1","omega2 2","omega2 3","b","alpha" )
dt <- dt[c("mu 1","mu 2","mu 3","omega2 1","omega2 2","omega2 3","sigma2", "b","alpha" )]



#===========RMSE and orther things==========================#

rmse <- function(x) sqrt(mean(x^2))

theta_0 <- multi_run_data$parameter[c('mu','omega2','sigma2','barb','baralpha')]%>% unlist
res <- t(t(dt)/theta_0 - 1 )


require(xtable)

data.frame(rrmse = res %>% apply(2, rmse),
           mean = dt %>% apply(2, mean),
           sd   = dt %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 3, display = rep('g', ncol(.)+1) )



res <- t(t(beta[,1:4])/multi_run_data$parameter$beta[1:4] - 1)

data.frame(rrmse = res %>% apply(2, rmse),
           mean = beta[,1:4] %>% apply(2, mean),
           sd   = res %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 2, display = rep('g', ncol(.)+1) )







#=================PLOT=======================#

gg <- dt %>% melt(id = NULL) %>%

  mutate(variable = factor(variable, levels = c(paste0('mu ', 1:3), paste0('omega2 ', 1:3), 'sigma2', 'b', 'alpha'))) %>%
  ggplot(aes(value, variable, fill = variable)) +

  geom_violin() +
  geom_boxplot(width = 0.3, col = 'black', size = 0.5) +
  # geom_jitter(size = 1) +

  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +

  facet_wrap( vars(variable), scales = 'free') +
  theme(legend.position = 'null') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
  labs(x = '', y = '')

gg

ggsave('violion_plot.png', gg)


gg <- beta %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(beta), each = nrow(beta)) %>% factor) %>%
  { .[which(.$value != 0), ] } %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg
ggsave('beta_selection_good.png', gg)



f <- function(res) res[[3]]

dt <- lapply(multi_run_data$res, f) %>% reduce(rbind)


#======================TABLE=========================#
res <- t(t(dt[,1:4])/multi_run_data$parameter$beta[1:4] - 1)

data.frame(rrmse = res %>% apply(2, rmse),
           mean = dt[,1:4] %>% apply(2, mean),
           sd   = res %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 2, display = rep('g', ncol(.)+1) )


#================PLOT=======================#
gg <- dt %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(dt), each = nrow(dt)) %>% factor) %>%
  { .[which(.$value != 0), ] } %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg

ggsave('beta_lbd_1_sqrtn_biaised.png', gg)


#=======================================#

gg <- multi_run_data$res[[1]]$res$beta[,1:4] %>%
  as.data.frame() %>% mutate(iteration = 1:nrow(.)) %>%
  melt(id = 'iteration') %>%

  ggplot(aes(iteration, value, col = variable)) + geom_line() +

  scale_color_discrete('Component of beta ', labels = paste0('beta ', 1:4))

gg
ggsave('beta_lbd_1_sqrtn_biaised_cv.png', gg)





data$beta %>% apply(1, function(x) sqrt(sum((x - multi_run_data$parameter$beta)^2 ))) %>% mean




v <- "omega2"
abs( t(t(data[[v]]) - para[[v]])/para[[v]] )^2 %>% mean %>% sqrt



require(xtable)

para <- multi_run$parameter
names(data) %>% lapply(function(v) abs(t( (t(data[[v]]) - para[[v]])/para[[v]]) )^2 %>%
                         apply(2, function(x)sqrt(mean(x)) ) ) %>%
  unlist %>% as.data.frame %>% t %>%
  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 3)













