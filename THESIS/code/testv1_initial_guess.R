##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-24 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ~/Git/henriquelaureano.github.io/THESIS/code/
source('functions.R')

## install.packages('pacman')
pacman::p_load(Matrix, mvtnorm, mc2d, ## rmultinomial()
               furrr,                 ## future + purrr
               TMB, tictoc)

future::plan(multicore);TMB::openmp(11)

## TAKE-HOME MESSAGE ---------------------------------------------------
## DO ALL INITIAL GUESSES LEAD TO THE SAME PLACE? BASICALLY, YES

## TESTING IN THE MOST DIFFICULT SCENARIO (SMALLER GROUPS AND HIGHEST
## CENSORSHIP)

beta  <- c(-2, -1.5)
gama  <- c(1.2, 1)
w     <- c(3, 5)
s2_1  <- 1.0
s2_2  <- 0.6
rho12 <-  0.1
Sigma <- sigmaPD2(s2_1=s2_1,
                  s2_2=s2_2, rho12=rho12)
Sigma

J     <- 3e4
cs    <- 2
time  <- runif(cs*J, 30, 79.9)
delta <- 80
Z     <- matrixZ(J=J, cs=cs)
U     <- matrix(0, nrow=J, ncol=2)

y <- datasimu(J=J, cs=cs, time=time,
              beta=beta, gama=gama, w=w,
              latent='partial', type='risk', Sigma=Sigma)
prop.table(colSums(y))

## ~/Git/henriquelaureano.github.io/THESIS/code/
dll <- 'v1';invisible(TMB::compile(paste0('cpps/', dll, '.cpp')))
## dyn.load(TMB::dynlib(dll))

coefs <- data.frame(
    beta1    =c(0, -2.0, rep(NA, 2)),
    beta2    =c(0, -1.5, rep(NA, 2)),
    gama1    =c(0,  1.2, rep(NA, 2)),
    gama2    =c(0,  1.0, rep(NA, 2)),
    w1       =c(1,  3.0, rep(NA, 2)),
    w2       =c(1,  5.0, rep(NA, 2)),
    logs2_1  =c(log(0.1), log(1.0), rep(NA, 2)),
    logs2_2  =c(log(0.1), log(0.6), rep(NA, 2)),
    rhoZ12   =c(atanh(0.05), atanh(0.1), rep(NA, 2)),
    conv     =rep(NA, 4), 
    mll      =rep(NA, 4),
    row.names=c(paste0('init', 1:2), paste0('fit', 1:2))
)

for (i in seq(2))
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
                                          rhoZ12 =coefs[i, 'rhoZ12'],
                                          U      =U),
                          DLL=dll, random='U', hessian=TRUE, silent=TRUE)
    tictoc::tic()
    opt <- with(obj, nlminb(par, fn, gr))
    tictoc::toc()
    coefs[i+2, ] <- c(opt$par, opt$conv, opt$obj)
    TMB::FreeADFun(obj);gc()
}
coefs
