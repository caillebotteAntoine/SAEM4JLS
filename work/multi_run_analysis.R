

rm(list = ls())

#============================================================================#
require(SAEM4JLS)
setwd('work')
getwd()

folder <- 'data_3'


files <- dir(folder)
data <- readRDS(paste0(folder, '/', files[1]))

for(v in names(data))
{
  data[[v]] <- as_tibble(data[[v]]) %>% na.omit %>% {.[nrow(.),]}
  names(data[[v]]) <- if(ncol(data[[v]]) == 1) v else paste0(v, 1:ncol(data[[v]]))
}

for(i in 2:length(files))
{
  print(files[i])
  res <- readRDS(paste0(folder, '/', files[i]))
  for(v in names(res))
  {
    res[[v]] <- as_tibble(res[[v]]) %>% na.omit %>% {.[nrow(.),]}
    names(res[[v]]) <- if(ncol(res[[v]]) == 1) v else paste0(v, 1:ncol(res[[v]]))

    data[[v]] <- rbind(data[[v]], res[[v]])
  }
}



data$beta <- data$beta[,1:4]
dt <-  na.omit(as.data.frame(data))
names(dt) <- sapply(names(data), function(v) names(data[[v]])) %>% unlist


oracle <- multi_run_data$oracle
oracle$beta <- oracle$beta[1:4]


gg <- dt %>% melt(id = NULL) %>%
  ggplot(aes(value, variable, fill = variable)) +

  geom_violin() +
  geom_boxplot(width = 0.3, col = 'black', size = 0.5) +
  geom_jitter(size = 1) +

  facet_wrap( vars(variable), scales = 'free')

# for(v in names(oracle))
# {
#   if(length(oracle[[v]]) == 1){
#     gg <- gg + geom_vline(data = data.frame(variable = v, value = oracle[[v]] ), aes(xintercept = value))
#   }else{
#     for(i in 1:length(oracle[[v]]))
#     {
#       gg <- gg + geom_vline(data = data.frame(variable = paste0(v,i), value = oracle[[v]][i] ), aes(xintercept = value))
#     }
#   }
# }


gg






data$beta %>% apply(1, function(x) sqrt(sum((x - multi_run_data$parameter$beta)^2 ))) %>% mean











data$beta %>% mutate(id = 1:nrow(.)) %>% melt(id = 'id') %>% mutate(variable = rep(1:1000, each = 30)) %>%
  ggplot(aes(variable, value, group = as.factor(id))) +
  geom_jitter()






data$beta %>%
  mutate(id = 1:nrow(.)) %>%
  melt(id = 'id') %>%
  mutate(variable = rep(1:1000, each = 30) %>% variable ) %>%
  { .[.$value != 0,]}%>%
  ggplot(aes(x = variable, xend = variable,
             y = min(value), yend = max(value), col = factor(variable)))  +

  geom_segment()




dt %>%

  ggplot(aes(x = support, xend = support,
             y = down, yend = up,
             col = support)) +

  geom_segment()+
  geom_point(aes(y = down), size = 3)+
  geom_point(aes(y = up), size = 3)




dt <- data.frame(up = data$beta %>% apply(2, max),
           down = data$beta %>% apply(2, min),
           support = 1:nrow(data$beta) %>% factor) %>%
  { .[which(.$up != 0 & .$down != 0),]}







