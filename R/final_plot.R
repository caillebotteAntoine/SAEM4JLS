rm(list = ls()) ; graphics.off()

require(SAEM4JLS)

#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
# multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_1_dot_2_sqrtn.rds")
# multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_2022_09_05_17_25_36.rds")
# multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_2022_09_06_13_16_43.rds") # sigma_alpha/b = 0.1 lbd = ?

# multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_2022_09_06_17_39_40.rds") # sigma_alpha/b = 0.5
# multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_2022_09_06_17_05_53.rds") #


multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_2022_09_07_16_30_08_1.rds") #
multi_run_data$data$sigma2_b

#========================= LASSO =========================#

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
gg <- plot( multi_run_data$res[[1]]$res_lasso, true.value = multi_run_data$oracle, exclude = 'beta', var = 'parameter') +
  labs(title = '')
gg
ggsave('plot_rapport/para_lbd_1_sqrtn_penalized.png', gg)

rmse <- function(x) sqrt(mean(x^2))

theta_0 <- multi_run_data$parameter[c('mu','omega2','sigma2','barb','baralpha')]%>% unlist
res <- t(t(dt)/theta_0 - 1 )


require(xtable)

data.frame(rrmse = res %>% apply(2, rmse),
           biais = theta_0 - dt %>% apply(2, mean),
           sd   = dt %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 3, display = rep('g', ncol(.)+1) )

#===============================================================================#

gg <- dt %>% melt(id = NULL) %>%

  mutate(variable = factor(variable, levels = c(paste0('mu ', 1:3), paste0('omega2 ', 1:3), 'sigma2', 'b', 'alpha'))) %>%
  ggplot(aes(value, variable, fill = variable)) +

  geom_violin() +
  geom_boxplot(width = 0.3, col = 'black', size = 0.5) +

  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +

  facet_wrap( vars(variable), scales = 'free') +
  theme(legend.position = 'null') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1) ) +
  labs(x = '', y = '')

gg

ggsave('plot_rapport/violion_plot.png', gg)

#===============================================================================#

gg <- beta %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(beta), each = nrow(beta)) %>% factor) %>%
  { .[which(.$value != 0), ] } %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg

ggsave('plot_rapport/beta_lbd_1_sqrtn_penalized.png', gg)

#===============================================================================#

res <- t(t(beta[,1:4])/multi_run_data$parameter$beta[1:4] - 1)

data.frame(rrmse = res %>% apply(2, rmse),
           biais = multi_run_data$parameter$beta[1:4] - beta[,1:4] %>% apply(2, mean),
           sd   = res %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 2, display = rep('g', ncol(.)+1) )


#===============================================================================#

f <- function(res) res[[3]]

dt <- lapply(multi_run_data$res, f) %>% reduce(rbind)
res <- t(t(dt[,1:4])/multi_run_data$parameter$beta[1:4] - 1)
#===============================================================================#

data.frame(rrmse = res %>% apply(2, rmse),
           biais = multi_run_data$parameter$beta[1:4] - dt[,1:4] %>% apply(2, mean),
           sd   = res %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 2, display = rep('g', ncol(.)+1) )

#===============================================================================#
gg <- dt %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(dt), each = nrow(dt)) %>% factor) %>%
  { .[which(.$value != 0), ] } %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg

ggsave('plot_rapport/beta_lbd_1_sqrtn_unpenalized.png', gg)

#===============================================================================#


gg <- multi_run_data$res[[1]]$res$beta[,1:4] %>%
  as.data.frame() %>% mutate(iteration = 1:nrow(.)) %>%
  melt(id = 'iteration') %>%

  ggplot(aes(iteration, value, col = variable)) + geom_line() +

  scale_color_discrete('Component of beta ', labels = paste0('beta ', 1:4))

gg
ggsave('plot_rapport/beta_lbd_1_sqrtn_penalized_cv.png', gg)

#===============================================================================#

gg <- plot( multi_run_data$res[[1]]$res, true.value = multi_run_data$oracle, exclude = 'beta', var = 'parameter') +
  labs(title = '')
gg
ggsave('plot_rapport/para_lbd_1_sqrtn_unpenalized.png', gg)




#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
#========================= 1_dot_2_sqrtn =========================#
multi_run_data <- readRDS("~/work/SAEM4JLS/multi_run_data_2022_09_05_17_14_11.rds")

#===============================================================================#

gg <- plot( multi_run_data$res[[1]]$res_lasso, true.value = multi_run_data$oracle, exclude = 'beta', var = 'parameter') +
  labs(title = '')
gg
ggsave('plot_rapport/para_lbd_1_dot_2_sqrtn_unpenalized.png', gg)

#========================= LASSO =========================#

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
dt_lasso <- dt[c("mu 1","mu 2","mu 3","omega2 1","omega2 2","omega2 3","sigma2", "b","alpha" )]

#===============================================================================#

gg <- beta %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(beta), each = nrow(beta)) %>% factor) %>%
  { .[which(.$value != 0), ] } %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg

ggsave('plot_rapport/beta_lbd_1_dot_2_sqrtn_penalized.png', gg)

#===============================================================================#
f <- function(res) res[[3]]

dt_beta <- lapply(multi_run_data$res, f) %>% reduce(rbind)
res <- t(t(dt_beta[,1:4])/multi_run_data$parameter$beta[1:4] - 1)

gg <- dt_beta %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(dt_beta), each = nrow(dt_beta)) %>% factor) %>%
  { .[which(.$value != 0), ] } %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg

ggsave('plot_rapport/beta_lbd_1_dot_2_sqrtn_unpenalized.png', gg)

#===============================================================================#

res0 <- t(t(beta[,1:4])/multi_run_data$parameter$beta[1:4] - 1)
res0 <- data.frame(rrmse = res0 %>% apply(2, rmse),
                   biais= multi_run_data$parameter$beta[1:4]  - beta[,1:4] %>% apply(2, mean),
                   sd   = res0 %>% apply(2, sd) )


data.frame(rrmse = res %>% apply(2, rmse),
           biais = multi_run_data$parameter$beta[1:4] - dt_beta[,1:4] %>% apply(2, mean),
           sd   = res %>% apply(2, sd) ) %>% {rbind(res0, .)} %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 2, display = rep('g', ncol(.)+1) )


#===============================================================================#

gg <- plot( multi_run_data$res[[1]]$res, true.value = multi_run_data$oracle, exclude = 'beta', var = 'parameter') +
  labs(title = '')
gg
ggsave('plot_rapport/para_lbd_1_dot_2sqrtn_unpenalized.png', gg)
#===============================================================================#


rmse <- function(x) sqrt(mean(x^2))

theta_0 <- multi_run_data$parameter[c('mu','omega2','sigma2','barb','baralpha')]%>% unlist
res_para <- t(t(dt_lasso)/theta_0 - 1 )

data.frame(rrmse = res_para %>% apply(2, rmse),
           biais = theta_0 - dt_lasso %>% apply(2, mean),
           sd   = dt_lasso %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 3, display = rep('g', ncol(.)+1) )


#========================= res =========================#

f <- function(res,v){
  if(v == 'beta') return(NA)
  x <- as_tibble(res[[v]]) %>% na.omit %>% {.[nrow(.),]}
  names(x) <- if(ncol(x) == 1) v else paste0(v, 1:ncol(x))
  x
}

# g <- function(v) lapply(multi_run_data$res, function(x) f(x$res_lasso, v)) %>% reduce(rbind)
g <- function(v) lapply(multi_run_data$res, function(x) f(x$res, v)) %>% reduce(rbind)

var <- names( multi_run_data$res[[1]]$res ) #%>% {.[-length(.)]}
data <- lapply( var, function(v) g(v) )
names(data) <- var


beta <- data$beta
data$beta <- NULL
dt <-  na.omit(as.data.frame(data))
names(dt) <- unlist( data %>% sapply(function(d) names(d)) )
names(dt) <- c("sigma2", "mu 1","mu 2","mu 3","omega2 1","omega2 2","omega2 3","b","alpha" )
dt <- dt[c("mu 1","mu 2","mu 3","omega2 1","omega2 2","omega2 3","sigma2", "b","alpha" )]

#===============================================================================#

rmse <- function(x) sqrt(mean(x^2))

theta_0 <- multi_run_data$parameter[c('mu','omega2','sigma2','barb','baralpha')]%>% unlist
res_para <- t(t(dt)/theta_0 - 1 )

data.frame(rrmse = res_para %>% apply(2, rmse),
           theta_0 = theta_0 - dt %>% apply(2, mean),
           sd   = dt %>% apply(2, sd) ) %>% t %>%

  xtable(align = c(rep('c|', ncol(.)), 'c') , digits = 3, display = rep('g', ncol(.)+1) )







#================================================================================#
#================================================================================#
#============================= FUN PLOT =========================================#
#================================================================================#
#================================================================================#


#============== LASSO ==============#
plot( multi_run_data$res[[1]]$res_lasso, true.value = multi_run_data$oracle, exclude = 'beta', var = 'parameter') +
  labs(title = '')

plot( multi_run_data$res[[1]]$res_lasso, true.value = multi_run_data$oracle, exclude = 'beta', var = 'MCMC') +
  labs(title = '')

#============== res ==============#

plot( multi_run_data$res[[1]]$res, true.value = multi_run_data$oracle, exclude = 'beta', var = 'parameter') +
  labs(title = '')

plot( multi_run_data$res[[1]]$res, true.value = multi_run_data$oracle, exclude = 'beta', var = 'MCMC') +
  labs(title = '')


#============== beta ==============#
plot_high_dim_tile(multi_run_data$res[[1]]$res_lasso$beta,
                   c(-2,-1,1,2, rep(0, ncol(multi_run_data$res[[1]]$res_lasso$beta)-4)), dec = 0)

plot_high_dim_tile(multi_run_data$res[[1]]$res$beta, c(-2,-1,1,2, rep(0, ncol(multi_run_data$res[[1]]$res$beta)-4)), dec = 0)









mySAEMres <- multi_run_data$res[[1]]$res
var.true <- multi_run_data$data$var.true
plot_high_dim_tile(mySAEMres$beta, c(-2,-1,1,2, rep(0, ncol(mySAEMres$beta)-4)), dec = 0)

plot(mySAEMres, true.value = multi_run_data$oracle, var = 'summary', exclude = 'beta', time = F)








