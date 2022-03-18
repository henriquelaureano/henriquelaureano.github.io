##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-mar-18 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
library(TMB)

## ------------------------------------
model <- 'blockdiag_model'

TMB::compile(paste0(model, '.cpp'))

load(paste0(model, '_data.RData'))

TMB::openmp(28)

## ------------------------------------
U <- matrix(0, nrow=J, ncol=2)

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

    dyn.load(TMB::dynlib(model))

    TMB::config(tape.parallel=FALSE, DLL=model)
}
## ------------------------------------
obj <- TMB::MakeADFun(data=list(Y=y[[1]], Z=Z, time=time, delta=80),
                      parameters=list(beta1  =beta['beta1'],
                                      beta2  =beta['beta2'],
                                      gama1  =gama['gama1'],
                                      gama2  =gama['gama2'],
                                      w1     =w['w1'],
                                      w2     =w['w2'],
                                      logs2_1=log(s2_1),
                                      logs2_2=log(s2_2),
                                      logs2_3=log(s2_3),
                                      logs2_4=log(s2_4),
                                      rhoZ12 =atanh(rho12),
                                      rhoZ34 =atanh(rho34),
                                      U1     =U,
                                      U2     =U),
                      DLL=model,
                      random=c('U1', 'U2'), hessian=TRUE, silent=TRUE)

opt <- try(with(obj, nlminb(par, fn, gr)), silent=TRUE)

TMB::FreeADFun(obj);gc()
## END -----------------------------------------------------------------
