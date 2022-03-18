##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-mar-18 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
source('functions.R')

## install.packages('pacman')
pacman::p_load(Matrix, mvtnorm, mc2d, ## rmultinomial()
               furrr,                 ## future + purrr
               TMB, tictoc)

future::plan(multicore);TMB::openmp(11)

## ------------------------------------
delta <- 80
time  <- runif(30e3, 30, 79.9)
cs    <- 5
Jcs   <- 6e3

s2_1  <- 1.0
s2_2  <- 0.6
s2_3  <- 0.7
s2_4  <- 0.9

rho12 <-  0.1
rho13 <- -0.5
rho14 <-  0.3
rho23 <-  0.3
rho24 <- -0.4
rho34 <-  0.2

## ------------------------------------
sigma.v1 <- sigmaPD2(s2_1=s2_1,
                     s2_2=s2_2, rho12=rho12)

sigma.v2 <- sigmaPD2(s2_1=s2_3,
                     s2_2=s2_4, rho12=rho34)

sigma.v3 <- sigmaPD4(s2_1=s2_1,
                     s2_2=s2_2,
                     s2_3=s2_3,
                     s2_4=s2_4, rho12=rho12,
                                rho13=0,
                                rho14=0,
                                rho23=0,
                                rho24=0,
                                rho34=rho34)

sigma.v4 <- sigmaPD4(s2_1=s2_1,
                     s2_2=s2_2,
                     s2_3=s2_3,
                     s2_4=s2_4, rho12=rho12,
                                rho13=rho13,
                                rho14=rho14,
                                rho23=rho23,
                                rho24=rho24,
                                rho34=rho34)

## ------------------------------------
beta.1 <- c(beta1= 3.0, beta2= 2.6)
beta.2 <- c(beta1=-2.0, beta2=-1.5)

gama.1 <- c(gama1=2.5, gama2=4.0)
gama.2 <- c(gama1=1.0, gama2=1.5)

w.1 <- c(w1=5.0, w2=10.0)
w.2 <- c(w1=3.0, w2= 4.0)

## ------------------------------------
future_datasimu(
    J     =Jcs,
    cs    =cs,
    n     =1,
    time  =time,
    delta =80,
    beta  =beta.1,
    gama  =gama.1,
    w     =w.1,
    latent='partial',
    type  ='risk',
    Sigma =sigma.v1,
    tag   ='risk_model_data'
)
future_datasimu(
    J     =Jcs,
    cs    =cs,
    n     =10,
    time  =time,
    delta =80,
    beta  =beta.1,
    gama  =gama.1,
    w     =w.1,
    latent='partial',
    type  ='trajectory',
    Sigma =sigma.v2,
    tag   ='time_model_data'
)
future_datasimu(
    J     =Jcs,
    cs    =cs,
    n     =1,
    time  =time,
    delta =80,
    beta  =beta.1,
    gama  =gama.1,
    w     =w.1,
    latent='complete',
    Sigma =sigma.v3,
    tag   ='blockdiag_model_data'
)
future_datasimu(
    J     =Jcs,
    cs    =cs,
    n     =1,
    time  =time2,
    delta =80,
    beta  =beta.1,
    gama  =gama.1,
    w     =w.1,
    latent='complete',
    Sigma =sigma.v4,
    tag   ='complete_model_data'
)
## END -----------------------------------------------------------------
