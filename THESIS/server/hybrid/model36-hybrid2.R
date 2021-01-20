##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                      leg.ufpr.br/~henrique · github.com/mynameislaure
##                                      laureano@ufpr.br · @hap_laureano
##                     Laboratory of Statistics and Geoinformation (LEG)
##       2021-jan-19 · Federal University of Paraná · Curitiba/PR/Brazil
##----------------------------------------------------------------------

(args <- commandArgs())
i <- abs(as.numeric(args[7]))

## packages-------------------------------------------------------------
## library(TMB, lib.loc='/home/est/bonat/nobackup/github/')
## This I'll run on my machine
library(TMB)

## load data and initial guesses----------------------------------------
load('../data36.RData')

## miscellaneous--------------------------------------------------------
model <- 'model36_hybrid'
openmp(12)
where <- 'coefs36_hybrid2'
J <- 3e4
t <- rep(seq(from=30, to=79.5, by=0.5), length.out=2*J)
Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
R <- matrix(0, nrow=J, ncol=4)

logs2_init <- c(log(0.2), log(0.3), log(0.4), log(0.5))
rhoZ_init <- c(atanh(0.15/sqrt(0.2*0.3)), atanh(0.15/sqrt(0.4*0.5)),
               atanh(0.1/sqrt(0.2*0.4)), atanh(0.1/sqrt(0.3*0.5)),
               atanh(0.2/sqrt(0.2*0.5)), atanh(0.2/sqrt(0.3*0.4)))

## model fitting--------------------------------------------------------
compile(paste0('cpps/', model, '.cpp'))
tmbdata <- list(Y=y[[i]], Z=Z, T=t, delta=80,
                beta1=-2, beta2=-1.5, gama1=1.2, gama2=1, w1=3, w2=5)
tmbpars <- list(R=R, logs2=logs2_init, rhoZ=rhoZ_init)
if (!model%in%names(getLoadedDLLs())) {
    cat(crayon::blue(clisymbols::symbol$star), 'Loading DLL\n')
    dyn.load(dynlib(paste0('cpps/', model)))
    config(tape.parallel=FALSE, DLL=model)
}
obj <- MakeADFun(data=tmbdata, parameters=tmbpars,
                 DLL=model, random='R', hessian=TRUE, silent=TRUE)
opt <- try(nlminb(obj$par, obj$fn, obj$gr, eval.max=1e3, iter.max=500),
           silent=TRUE)
if (class(opt)!='try-error') {
    write.table(rbind(c(opt$par, opt$convergence)),
                file=paste0(where, '.txt'),
                append=TRUE, col.names=FALSE) 
    sdr <- try(sdreport(obj, par.fixed=TRUE, hessian.fixed=TRUE),
               silent=TRUE)
    if (class(sdr)!='try-error') {
        eps <- c(summary(sdr, 'fixed')[, 2],
                 summary(sdr, 'report')[, 2])
        write.table(rbind(eps),
                    file=paste0('eps_', where, '_', i, '.txt'),
                    append=TRUE, col.names=FALSE)
    }
}
FreeADFun(obj);gc()
## END------------------------------------------------------------------
