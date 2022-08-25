

rm(list = ls())




#=============PLOT =====================#
folder <- 'data'

files <- dir(folder)
data <- readRDS(paste0(folder, '/', files[1]))

for(v in names(data))
{
  data[[v]] <- as_tibble(data[[v]])
  names(data[[v]]) <- paste0(v, 1:ncol(data[[v]]))
  data[[v]] <- data[[v]] %>% mutate(run_id = 1)
}





for(i in 2:length(files))
{
  res <- readRDS(paste0(folder, '/', files[i]))
  for(v in names(res))
  {
    res[[v]] <- as_tibble(res[[v]] )
    names(res[[v]]) <- paste0(v, 1:ncol(res[[v]]))
    res[[v]] <- res[[v]] %>% mutate(run_id = i)

    data[[v]] <- rbind(data[[v]], res[[v]])
  }

  data[[v]] <- rbind(data[[v]], res[[v]])
}













#============================================================================#
require(SAEM4JLS)
setwd('work')
getwd()

folder <- 'data'


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

for(v in names(oracle))
{
  if(length(oracle[[v]]) == 1){
    gg <- gg + geom_vline(data = data.frame(variable = v, value = oracle[[v]] ), aes(xintercept = value))
  }else{
    for(i in 1:length(oracle[[v]]))
    {
      gg <- gg + geom_vline(data = data.frame(variable = paste0(v,i), value = oracle[[v]][i] ), aes(xintercept = value))
    }
  }
}


gg






data$beta %>% apply(1, function(x) sqrt(sum((x - oracle$beta)^2 ))) %>% mean




















