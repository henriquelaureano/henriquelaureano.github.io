##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-26 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
( args <- commandArgs() )
i      <- abs(as.numeric(args[7]))

library(TMB)
## library(TMB, lib.loc='/home/est/bonat/nobackup/github/')

## ------------------------------------
name  <- 'v2_cs2_2'
where <- paste0('coefs', name)
model <- gsub('_.*', '', name)

TMB::compile(paste0('cpps/', model, '.cpp'))

load(paste0('data/', name, '.RData'))

TMB::openmp(12)
## TMB::openmp(28)

## ------------------------------------
J  <- 12e3
cs <- 5
U  <- matrix(0, nrow=J, ncol=2)

blocks <- replicate(J, rep(1, cs), simplify=FALSE)
Z      <- Matrix::bdiag(blocks)

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

beta.1 <- c(beta1= 3.0, beta2= 2.6)
beta.2 <- c(beta1=-2.0, beta2=-1.5)

gama.1 <- c(gama1=2.5, gama2=4.0)
gama.2 <- c(gama1=1.0, gama2=1.5)

w.1 <- c(w1=5.0, w2=10.0)
w.2 <- c(w1=3.0, w2= 4.0)

if ( !model %in% names(getLoadedDLLs()) ) {

    cat('Loading DLL\n')

    dyn.load(TMB::dynlib(paste0('cpps/', model)))

    TMB::config(tape.parallel=FALSE, DLL=model)
}
## ------------------------------------
obj <- TMB::MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=80),
                      parameters=list(beta1  =beta.2['beta1'],
                                      beta2  =beta.2['beta2'],
                                      gama1  =gama.2['gama1'],
                                      gama2  =gama.2['gama2'],
                                      w1     =w.2['w1'],
                                      w2     =w.2['w2'],
                                      logs2_3=log(s2_3),
                                      logs2_4=log(s2_4),
                                      rhoZ34 =atanh(rho34),
                                      U      =U),
                      DLL=model, random='U', hessian=TRUE, silent=TRUE)

opt <- try(with(obj, nlminb(par, fn, gr)), silent=TRUE)

if (class(opt) != 'try-error') {
    
    write.table(
        rbind(c(opt$par, opt$conv, opt$obj)),
        file=paste0(where, '.txt'), append=TRUE, col.names=FALSE
    )
}
TMB::FreeADFun(obj);gc()
## END -----------------------------------------------------------------
