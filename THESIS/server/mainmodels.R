##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                      leg.ufpr.br/~henrique · github.com/mynameislaure
##                                      laureano@ufpr.br · @hap_laureano
##                     Laboratory of Statistics and Geoinformation (LEG)
##       2020-dez-21 · Federal University of Paraná · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## MODEL 22=============================================================

## packages-------------------------------------------------------------
library(Rcpp, lib.loc='/home/est/bonat/nobackup/')
library(RcppEigen, lib.loc='/home/est/bonat/nobackup/')
library(TMB, lib.loc='/home/est/bonat/nobackup/')

## simulating data (just the structure)---------------------------------
datasimu <- function(J, delta=80, t, 
                     beta=c(-2, -1.5), gamma=c(1.2, 1), w=c(3, 5),
                     Z,  S=matrix(c(0.4, 0.15, 0.05, 0,
                                    0.15, 0.4, 0, 0.05,
                                    0.05, 0, 0.25, 0.1,
                                    0, 0.05, 0.1, 0.25), 4, 4)
                     ){
    K <- dim(S)[1]/2+1
    ladim <- 2*(K-1) ## latent effects dimension
    B <- mvtnorm::rmvnorm(J, mean=rep(0, ladim), sigma=S)
    R <- Z%*%B
    risk1 <- exp(beta[1]+R[, 1])
    risk2 <- exp(beta[2]+R[, 2])
    level <- 1+risk1+risk2
    p1 <- risk1/level*w[1]*delta/(2*t*(delta-t))*
        dnorm(w[1]*atanh(2*t/delta-1)-gamma[1]-R[, 3])
    p2 <- risk2/level*w[2]*delta/(2*t*(delta-t))*
        dnorm(w[2]*atanh(2*t/delta-1)-gamma[2]-R[, 4])
    y <- mc2d::rmultinomial(2*J, 1, prob=cbind(p1, p2, 1-p1-p2))
    return(y)
}

## how many models------------------------------------------------------
hmm <- 5
## compute one time to not neet to compute every time-------------------
J <- 3e4
Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
t <- rep(seq(from=30, to=79.5, by=0.5), length.out=2*J)
R <- matrix(0, nrow=J, ncol=4)
logs2_init <- c(log(0.2), log(0.3), log(0.4), log(0.5))
rhoZ_init <- c(atanh(0.15/sqrt(0.2*0.3)), atanh(0.15/sqrt(0.4*0.5)),
               atanh(0.1/sqrt(0.2*0.4)), atanh(0.1/sqrt(0.3*0.5)))

coefs <- matrix(NA, nrow=hmm, ncol=10+length(rhoZ_init))
## loop-----------------------------------------------------------------
for (i in hmm) {
    df22 <- datasimu(J=J, t=t, Z=Z)
    ## getting the initial guesses--------------------------------------
    dyn.load(dynlib('multiGLM'))
    obj22f <- MakeADFun(
        data=list(Y=df22, T=t, delta=80),
        parameters=list(beta1=0, beta2=0, gama1=0, gama2=0, w1=1, w2=1),
        DLL = 'multiGLM', silent=TRUE)
    opt22f <- nlminb(obj22f$par, obj22f$fn, obj22f$gr)
    ## model fitting----------------------------------------------------
    tmbpars22 <- list(beta1=opt22f$par['beta1'],
                      beta2=opt22f$par['beta2'],
                      gama1=opt22f$par['gama1'],
                      gama2=opt22f$par['gama2'],
                      w1=opt22f$par['w1'],
                      w2=opt22f$par['w2'],
                      R=R, logs2=logs2_init, rhoZ=rhoZ_init)
    dyn.load(dynlib('multiGLMM_22'))
    openmp(n=28)
    config(tape.parallel=FALSE, DLL='multiGLMM_22')
    obj22 <- MakeADFun(data=list(Y=df22, Z=Z, T=t, delta=80),
                       parameters=tmbpars22,
                       DLL='multiGLMM_22',
                       random='R', hessian=TRUE, silent=TRUE)
    opt22 <- nlminb(obj22$par, obj22$fn, obj22$gr)
    coefs[i, ] <- opt22$par
    FreeADFun(obj22)
    gc()
}
save.image('mainmodels.RData', version=2)
## END------------------------------------------------------------------
