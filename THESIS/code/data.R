##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-mar-02 · Curitiba/PR/Brazil
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

cs1   <- 2
cs2   <- 5
cs3   <- 10

J1cs1 <- 2500
J1cs2 <- 1000
J1cs3 <- 500

J2cs1 <- 15e3
J2cs2 <-  6e3
J2cs3 <-  3e3

J3cs1 <- 30e3
J3cs2 <- 12e3
J3cs3 <-  6e3

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
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_1_1'
)
## 2 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_1_1'
)
## 3 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_1_1'
)
## 4 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_2_1'
)
## 5 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_2_1'
)
## 6 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_2_1'
)
## 7 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_1_1'
)
## 8 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_1_1'
)
## 9 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_1_1'
)
## 10 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_2_1'
)
## 11 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_2_1'
)
## 12 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_2_1'
)
## 13 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_1_1'
)
## 14 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_1_1'
)
## 15 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_1_1'
)
## 16 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_2_1'
)
## 17 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_2_1'
)
## 18 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_2_1'
)
## 19 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_1_1'
)
## 20 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_1_1'
)
## 21 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_1_1'
)
## 22 ====================================
future_datasimu(
    J=J1cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_2_1'
)
## 23 ====================================
future_datasimu(
    J=J1cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_2_1'
)
## 24 ====================================
future_datasimu(
    J=J1cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_2_1'
)
## 25 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_1_2'
)
## 26 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_1_2'
)
## 27 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_1_2'
)
## 28 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_2_2'
)
## 29 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_2_2'
)
## 30 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_2_2'
)
## 31 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_1_2'
)
## 32 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_1_2'
)
## 33 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_1_2'
)
## 34 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_2_2'
)
## 35 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_2_2'
)
## 36 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_2_2'
)
## 37 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_1_2'
)
## 38 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_1_2'
)
## 39 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_1_2'
)
## 40 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_2_2'
)
## 41 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_2_2'
)
## 42 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_2_2'
)
## 43 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_1_2'
)
## 44 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_1_2'
)
## 45 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_1_2'
)
## 46 ====================================
future_datasimu(
    J=J2cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_2_2'
)
## 47 ====================================
future_datasimu(
    J=J2cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_2_2'
)
## 48 ====================================
future_datasimu(
    J=J2cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_2_2'
)
## 49 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_1_3'
)
## 50 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_1_3'
)
## 51 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_1_3'
)
## 52 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs1_2_3'
)
## 53 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs2_2_3'
)
## 54 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='risk', Sigma=sigma.v1, tag='v1_cs3_2_3'
)
## 55 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_1_3'
)
## 56 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_1_3'
)
## 57 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_1_3'
)
## 58 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs1_2_3'
)
## 59 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs2_2_3'
)
## 60 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='partial', type='trajectory', Sigma=sigma.v2, tag='v2_cs3_2_3'
)
## 61 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_1_3'
)
## 62 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_1_3'
)
## 63 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_1_3'
)
## 64 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs1_2_3'
)
## 65 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs2_2_3'
)
## 66 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v3, tag='v3_cs3_2_3'
)
## 67 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_1_3'
)
## 68 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_1_3'
)
## 69 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.1, gama=gama.1, w=w.1,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_1_3'
)
## 70 ====================================
future_datasimu(
    J=J3cs1, cs=cs1, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs1_2_3'
)
## 71 ====================================
future_datasimu(
    J=J3cs2, cs=cs2, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs2_2_3'
)
## 72 ====================================
future_datasimu(
    J=J3cs3, cs=cs3, n=100, time=time, delta=80,
    beta=beta.2, gama=gama.2, w=w.2,
    latent='complete', Sigma=sigma.v4, tag='v4_cs3_2_3'
)
## END -----------------------------------------------------------------
