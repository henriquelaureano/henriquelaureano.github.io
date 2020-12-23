##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                      leg.ufpr.br/~henrique · github.com/mynameislaure
##                                      laureano@ufpr.br · @hap_laureano
##                     Laboratory of Statistics and Geoinformation (LEG)
##       2020-dez-23 · Federal University of Paraná · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## MODEL 22=============================================================

## packages-------------------------------------------------------------
## library(Rcpp, lib.loc='/home/est/bonat/nobackup/')
## library(RcppEigen, lib.loc='/home/est/bonat/nobackup/')
library(TMB, lib.loc='/home/est/bonat/nobackup/github/')

## load data and initial guesses----------------------------------------
load('data.RData')

## how many models------------------------------------------------------
hmm <- 5
## compute one time to not need to compute every time-------------------
J <- 3e4
R <- matrix(0, nrow=J, ncol=4)
Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
t <- rep(seq(from=30, to=79.5, by=0.5), length.out=2*J)
logs2_init <- c(log(0.2), log(0.3), log(0.4), log(0.5))
rhoZ_init <- c(atanh(0.15/sqrt(0.2*0.3)), atanh(0.15/sqrt(0.4*0.5)),
               atanh(0.1/sqrt(0.2*0.4)), atanh(0.1/sqrt(0.3*0.5)))

coefs <- matrix(NA, nrow=hmm, ncol=10+length(rhoZ_init))
openmp(29)
dyn.load(dynlib('multiGLMM_22'))
config(tape.parallel=FALSE, DLL='multiGLMM_22')
## loop-----------------------------------------------------------------
for (i in 1:hmm) {
    tmbpars22 <- list(beta1=initFixed[i, 1], beta2=initFixed[i, 2],
                      gama1=initFixed[i, 3], gama2=initFixed[i, 4],
                      w1=initFixed[i, 5], w2=initFixed[i, 6],
                      R=R, logs2=logs2_init, rhoZ=rhoZ_init
                      )
    obj22 <- MakeADFun(data=list(Y=df22[[i]], Z=Z, T=t, delta=80),
                       parameters=tmbpars22,
                       DLL='multiGLMM_22',
                       random='R', silent=TRUE)
    opt22 <- try(nlminb(obj22$par, obj22$fn, obj22$gr), silent=TRUE)
    FreeADFun(obj22)
    gc()
    if (class(opt22)!='try-error') {
        coefs[i, ] <- opt22$par
        write.table(coefs, file='coefs.txt')
    }
}
save.image('model22.RData', version=2)
## END------------------------------------------------------------------
