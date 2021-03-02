##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-mar-02 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
( args <- commandArgs() )
i      <- abs(as.numeric(args[7]))

library(TMB, lib.loc='/home/est/bonat/nobackup/github/')

## ------------------------------------
name  <- 'v2_cs1_2_2'
where <- paste0('coefs', name)
model <- gsub('_.*', '', name)

TMB::compile(paste0('cpps/', model, '.cpp'))

load(paste0('data/', name, '.RData'))

TMB::openmp(28)

## ------------------------------------
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

if ( !model %in% names(getLoadedDLLs()) )
{
    cat('Loading DLL\n')

    dyn.load(TMB::dynlib(paste0('cpps/', model)))

    TMB::config(tape.parallel=FALSE, DLL=model)
}
## ------------------------------------
obj <- TMB::MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=80),
                      parameters=list(beta1  =beta['beta1'],
                                      beta2  =beta['beta2'],
                                      gama1  =gama['gama1'],
                                      gama2  =gama['gama2'],
                                      w1     =w['w1'],
                                      w2     =w['w2'],
                                      logs2_3=log(s2_3),
                                      logs2_4=log(s2_4),
                                      rhoZ34 =atanh(rho34),
                                      U      =U),
                      DLL=model, random='U', hessian=TRUE, silent=TRUE)

opt <- try(with(obj, nlminb(par, fn, gr)), silent=TRUE)

if (class(opt) != 'try-error')
{    
    write.table(
        rbind(c(opt$par, opt$conv, opt$obj)),
        file=paste0(where, '.txt'), append=TRUE, col.names=FALSE
    )
}
TMB::FreeADFun(obj);gc()
## END -----------------------------------------------------------------
