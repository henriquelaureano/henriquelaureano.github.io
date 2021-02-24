##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-23 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ~/Git/henriquelaureano.github.io/THESIS/code/
source('functions.R')

## install.packages('pacman')
pacman::p_load(Matrix, mvtnorm, mc2d, ## rmultinomial()
               furrr,                 ## future + purrr
               TMB, tictoc)

future::plan(multicore);TMB::openmp(11)

## TAKE-HOME MESSAGE ---------------------------------------------------
## DO ALL INITIAL GUESSES LEAD TO THE SAME PLACE? KIND OF

## TESTING WITH THE BIGGER (MOST COMPLICATED?) MODEL IN THE MOST
## DIFFICULT SCENARIO (SMALLER GROUPS AND HIGHEST CENSORSHIP)

beta  <- c(-2, -1.5)
gama  <- c(1.2, 1)
w     <- c(3, 5)
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
Sigma <- sigmaPD4(s2_1=s2_1,
                  s2_2=s2_2,
                  s2_3=s2_3,
                  s2_4=s2_4,
                  rho12=rho12,
                  rho13=rho13,
                  rho14=rho14,
                  rho23=rho23,
                  rho24=rho24,
                  rho34=rho34)
Sigma

J     <- 3e4
cs    <- 2
time  <- runif(cs*J, 30, 79.9)
delta <- 80
Z     <- matrixZ(J=J, cs=cs)
U     <- matrix(0, nrow=J, ncol=4)

y <- datasimu(J=J, cs=cs, time=time,
              beta=beta, gama=gama, w=w, latent='complete', Sigma=Sigma)
prop.table(colSums(y))

## ~/Git/henriquelaureano.github.io/THESIS/code/
dll <- 'v4';invisible(TMB::compile(paste0('cpps/', dll, '.cpp')))
## dyn.load(TMB::dynlib(dll))

coefs <- data.frame(
    beta1    =c(0, 0, -2.0, rep(NA, 3)),
    beta2    =c(0, 0, -1.5, rep(NA, 3)),
    gama1    =c(0, 0,  1.2, rep(NA, 3)),
    gama2    =c(0, 0,  1.0, rep(NA, 3)),
    w1       =c(1, 1,  3.0, rep(NA, 3)),
    w2       =c(1, 1,  5.0, rep(NA, 3)),
    logs2_1  =c(log(0.1), log(0.1), log(1.0), rep(NA, 3)),
    logs2_2  =c(log(0.1), log(0.1), log(0.6), rep(NA, 3)),
    logs2_3  =c(log(0.1), log(0.1), log(0.7), rep(NA, 3)),
    logs2_4  =c(log(0.1), log(0.1), log(0.9), rep(NA, 3)),
    rhoZ12   =c(atanh(0.05), atanh( 0.05), atanh( 0.1), rep(NA, 3)),
    rhoZ13   =c(atanh(0.05), atanh(-0.05), atanh(-0.5), rep(NA, 3)),
    rhoZ14   =c(atanh(0.05), atanh( 0.05), atanh( 0.3), rep(NA, 3)),
    rhoZ23   =c(atanh(0.05), atanh( 0.05), atanh( 0.3), rep(NA, 3)),
    rhoZ24   =c(atanh(0.05), atanh(-0.05), atanh(-0.4), rep(NA, 3)),
    rhoZ34   =c(atanh(0.05), atanh( 0.05), atanh( 0.2), rep(NA, 3)),
    conv     =rep(NA, 6), 
    mll      =rep(NA, 6),
    row.names=c(paste0('init', 1:3), paste0('fit', 1:3))
)

for (i in seq(3))
{
    checkDLL(dll)
    obj <- TMB::MakeADFun(data=list(Y=y, Z=Z, time=time, delta=delta),
                          parameters=list(beta1  =coefs[i, 'beta1'],
                                          beta2  =coefs[i, 'beta2'],
                                          gama1  =coefs[i, 'gama1'],
                                          gama2  =coefs[i, 'gama2'],
                                          w1     =coefs[i, 'w1'],
                                          w2     =coefs[i, 'w2'],
                                          logs2_1=coefs[i, 'logs2_1'],
                                          logs2_2=coefs[i, 'logs2_2'],
                                          logs2_3=coefs[i, 'logs2_3'],
                                          logs2_4=coefs[i, 'logs2_4'],
                                          rhoZ12 =coefs[i, 'rhoZ12'],
                                          rhoZ13 =coefs[i, 'rhoZ13'],
                                          rhoZ14 =coefs[i, 'rhoZ14'],
                                          rhoZ23 =coefs[i, 'rhoZ23'],
                                          rhoZ24 =coefs[i, 'rhoZ24'],
                                          rhoZ34 =coefs[i, 'rhoZ34'], 
                                          U=U),
                          DLL=dll, random='U', hessian=TRUE, silent=TRUE)
    tictoc::tic()
    opt <- with(obj, nlminb(par, fn, gr))
    tictoc::toc()
    coefs[i+3, ] <- c(opt$par, opt$conv, opt$obj)
    TMB::FreeADFun(obj);gc()
}
coefs
