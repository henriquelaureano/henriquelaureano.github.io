##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-26 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ~/Git/henriquelaureano.github.io/THESIS/code/
source('functions.R')

## install.packages('pacman')

pacman::p_load(Matrix, mvtnorm, mc2d, ## rmultinomial()
               furrr,                 ## future + purrr
               TMB, tictoc)

future::plan(multicore);TMB::openmp(11)

## ------------------------------------
delta <- 80
time  <- runif(60e3, 30, 79.9)

J1    <- 30e3
cs1   <- 2

J2    <- 12e3
cs2   <- 5

J3    <- 6e3
cs3   <- 10

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

## 1 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_1'
)
## 2 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_1'
)
## 3 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_1'
)
## 4 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1,
    tag='v1_cs1_2'
)
## 5 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_2'
)
## 6 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_2'
)
## 7 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_1'
)
## 8 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_1'
)
## 9 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_1'
)
## 10 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_2'
)
## 11 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_2'
)
## 12 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_2'
)
## 13 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_1'
)
## 14 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_1'
)
## 15 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_1'
)
## 16 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_2'
)
## 17 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_2'
)
## 18 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_2'
)
## 19 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_1'
)
## 20 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_1'
)
## 21 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_1'
)
## 22 ====================================
future_datasimu(
    J=J1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_2'
)
## 23 ====================================
future_datasimu(
    J=J2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_2'
)
## 24 ====================================
future_datasimu(
    J=J3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_2'
)
## END -----------------------------------------------------------------
