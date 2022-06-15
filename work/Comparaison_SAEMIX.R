rm(list = ls()) ; graphics.off()
require(SAEM4JLS)

m <- function(t,eta, phi) {
  D   <- 320
  ka  <-phi[,1] ; V   <-phi[,2] ; ke  <-phi[,3]
  fpred <-D*ka/(V*(ka-ke))*(exp(-ke*t)-exp(-ka*t))
  return(fpred)
}

param <- list(sigma2 = 0.0005,
              mu = c(1.8,32.5 , .08),
              omega2 = c(0.3,1.6,.006)^2)

F. <- length(param$mu)

t <- c(seq(0,5, length.out = 6), seq(5,40, length.out = 4))
G <- 40
ng = 1
dt <- NLME_obs(G, ng, t, param, m )
Y <- dt$obs
n <- G*ng*length(t) ; N <- G*ng  #nombre total de data, et #nombre total d'individu

dt %>% ggplot(aes(t, obs, col = gen, group = id)) +
  geom_point() + geom_line() + theme(legend.position = 'null')



#==============================================================================#
Phi <- fct_vector(function(sigma2, mu, omega2) {
  c(- n/(2*sigma2),                #1, 2
    - G/(2*omega2), G*mu/omega2               #3, 4
  )},
  dim = c(1,F.,F.) )$eval

S <- fct_vector(function( phi) mean((Y - get_obs(m, dt, phi = phi) )^2 ),
                function( phi) apply(phi^2, 2, mean), #3
                function( phi) apply(phi, 2, mean),   #4
                dim = c(1,F.,F.) )
#==============================================================================#
loglik.phi <- function(x,  Phi) sum( Phi* S$eval(phi = x) )



# ---  Initialisation des paramètres --- #
para <- param %>% sapply(function(x) x* runif(1, 1.2,1.4))
para$omega2 <- rep(.001,3)

# --- Initialisation des chaines MC : Z_0 ---
Z <- list(phi = list( matrix(rnorm(F.*G, para$mu, para$omega2), nrow = F.) %>% t ))

ncores = 1

sim <- function(niter, k, Phih,  phi)
{
  phi <- list( MH_High_Dim_para_future(niter, phi[[1]], sd = sd.phi(k), loglik.phi,  Phih, cores = ncores ))
  list( phi = phi)
}

maxi <- function(S)
{
  list(sigma2 = S%a%1,
       mu =     S%a%3,
       omega2 = S%a%2 - (S%a%3)^2)
}


do.call(Phi, para)
S$eval( Z$phi[[1]])
oracle <- maxi( S$eval( Z$phi[[1]]) )


niter <- 10
correction.phase <- 15
MH.iter <- function(k) ifelse(k<=correction.phase, 100, 20)

sd.phi <- function(k) ifelse(k<=correction.phase, 0.5, 0.05 )

res <- SAEM(niter, MH.iter, para, Phi, S$eval, Z, sim, maxi,
            eps = 1e-3, verbatim = 2)

plot(res, true.value =  param)








library(saemix)
model1cpt <- function(psi,id,x) {
  D   <- 320
  t   <-x[,1]
  ka  <-psi[id,1]
  V   <-psi[id,2]
  ke  <-psi[id,3]
  fpred <-D*ka/(V*(ka-ke))*(exp(-ke*t)-exp(-ka*t))
  return(fpred)
}

saemix.data <- saemixData(name.data       = dt,
                          name.group      = "id",
                          name.predictors = "t",
                          name.response   = "obs")

saemix.model <- saemixModel(model = model1cpt,
                            psi0  = c(ka=1,V=20,ke=0.5))

saemix.options <- list(map=TRUE, fim=TRUE, ll.is=FALSE, displayProgress=FALSE, seed=632545)

start <- Sys.time()
saemix.fit1    <- saemix(saemix.model, saemix.data, saemix.options)
difftime(Sys.time(), start)


oracle$mu
saemix.fit1@results@fixed.psi
saemix.fit1@results@se.fixed






