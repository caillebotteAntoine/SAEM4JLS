
tmp_beta <- beta %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(beta), each = nrow(beta)) %>% factor) %>%
  { .[which(.$value != 0), ] }
gg <- tmp_beta %>%

  ggplot(aes(variable, value)) + geom_boxplot(aes( fill = variable)) +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg <- gg +
  geom_point(data = data.frame(variable = unique(tmp_beta$variable),
                               value = c(-2,-1,1,2, rep(0, tmp_beta$variable %>% unique %>% length - 4) )),
             shape = 8, aes(col = variable) )+
  ylim(-2.5,2.5)

gg


ggsave('beta_lbd_1_dot_2_sqrtn_penalized.png', gg, width = 8.2 , height = 5.6)



f <- function(res) res[[3]]

dt <- lapply(multi_run_data$res, f) %>% reduce(rbind)
res <- t(t(dt[,1:4])/multi_run_data$parameter$beta[1:4] - 1)

beta_tmp_2 <- dt %>% melt(id = NULL) %>%
  mutate(variable = rep(1:ncol(dt), each = nrow(dt)) %>% factor) %>%
  { .[which(.$value != 0), ] }

gg2 <- beta_tmp_2 %>%

  ggplot(aes(variable, value, fill = variable)) + geom_boxplot() +
  theme(legend.position = 'null') +
  labs(x = 'Selected variables')

gg2 <- gg2 +
  geom_point(data = data.frame(variable = unique(beta_tmp_2$variable),
                               value = c(-2,-1,1,2, rep(0, beta_tmp_2$variable %>% unique %>% length - 4) )),
             shape = 8, aes(col = variable) ) +
  ylim(-2.5,2.5)

ggsave('beta_lbd_1_dot_2_sqrtn_unpenalized.png', gg2, width = 8.2 , height = 5.6)


ggsave('plot_rapport/beta_lbd_1_sqrtn_unpenalized.png', gg, width = 8.2 , height = 5.6)

