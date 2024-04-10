
rm(list = ls())
graphics.off()
require(saemix)
require(tidyr)
require(ggplot2)


J = 20
G = 8
L = 4

dt = data.frame(time = rep(seq(0,200, len = J), times = G*L),
                L = rep(rep(1:L, each = J), times = G),
                G = rep(1:G, each = L*J))

param <- list(sigma2 = 0.0001,
              mu = c(0.3,90,7.5),
              omega2 = c(0.0025, 20,1))

phi1 <- rnorm(G, param$mu[1], sqrt(param$omega2[1]))
phi2 <- rnorm(G, param$mu[2], sqrt(param$omega2[2]))
phi3 <- rep(param$mu[3], G)

nlme_fct <- function(t,phi1,phi2,phi3) { phi1/(1+exp( (phi2-t)/phi3)) } # Logistic function

eps <- rnorm(G*L*J, 0, sqrt(param$sigma2))
dt$Y = -1
for(i in 1:nrow(dt))
{
  g = dt[i,"G"]
  
  dt$Y[i] = nlme_fct(dt[i,"time"], phi1[g], phi2[g],phi3[g]) + eps[i]
}

dt$L <- dt$L - 1
dt$G <- dt$G - 1

rm(list=c(paste0("phi", 1:3), "eps"))
dt %>% ggplot(aes(time, Y, group = interaction(G, L))) + geom_line(aes(col = factor(G) ), linewidth = 1)+
                theme(legend.position = "None")




saemix.data <- saemixData(name.data       = dt,
                          name.group      = "G",
                          name.predictors = "time",
                          name.response   = "Y")


fct.model<- function(psi, id, x){
  t <- x[,1]
  
  phi1 <- psi[id, 1]
  phi2 <- psi[id, 2]
  phi3 <- psi[id, 3]
  
  return( phi1/(1+exp( (phi2-t)/phi3)) )
}


saemix.model <- saemixModel(model = fct.model, 
                            psi0  = c(phi1 = 300, phi2 = 400, phi3 = 100))


saemix.options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, displayProgress=FALSE, seed=632545)
saemix.fit1    <- saemix(saemix.model, saemix.data, saemix.options)

saemix.fit1@results




