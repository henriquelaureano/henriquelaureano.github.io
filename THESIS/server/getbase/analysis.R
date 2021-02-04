##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-03 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## install.packages('pacman')
pacman::p_load(TMB, furrr, crayon, clisymbols)

source('functions.R')

## OVERALL DEFINITIONS -------------------------------------------------
plan(multicore)
openmp(10)

n <- 5
J <- 3e4
Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
R <- matrix(0, nrow=J, ncol=4)
time <- runif(2*J, 30, 79.5)
delta <- 80

## generating ``n`` datasets in a parallelized fashion WITH A LATENT
## STRUCTURE
y <- future_map(rep(J, n), ~datasimu(.x, time=time, Sigma=diag(4)),
                .options=furrr_options(seed=NULL))
## list with n slots
## str(y)

dll0 <- 'multiGLM'
dll1 <- 'multiGLMM_diag0'
dll2 <- 'multiGLMM_diag1'

compile(paste0(dll0, '.cpp'))
compile(paste0(dll1, '.cpp'))
compile(paste0(dll2, '.cpp'))

## true parameter values -----------------------------------------------
coefs.true <- c(beta1=-2, beta2=-1.5, gama1=1.2, gama2=1, w1=3, w2=5,
                logs2=log(1))

## FIRST, we fit the GLM -----------------------------------------------
dll0.out <- matrix(
    NA, nrow=n+1, ncol=7,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true)[1:6], 'conv'))
)
dll0.out[n+1, ] <- c(coefs.true[1:6], NaN)

for (i in seq(n))
{    
    checkDLL(dll0)
    obj <- MakeADFun(
        data=list(Y=y[[i]], time=time, delta=delta),
        parameters=list(beta1=0, beta2=0, gama1=0, gama2=0, w1=1, w2=1),
        DLL=dll0, hessian=TRUE, silent=TRUE
    )
    opt <- nlminb(
        obj$par,
        obj$fn,
        obj$gr, control=list(eval.max=1e3, iter.max=500)
    )
    dll0.out[i, ] <- c(opt$par, opt$convergence) 
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}
## REMEMBER, the model is ignoring the latent structure
dll0.out

## SECOND, considering the latent structure but as fixed ---------------
dll1.out <- matrix(
    NA, nrow=n+1, ncol=7,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true)[1:6], 'conv'))
)
dll1.out[n+1, ] <- c(coefs.true[1:6], NaN)

for (i in seq(n))
{
    checkDLL(dll1)
    obj <- MakeADFun(
        data=list(
            Y=y[[i]], Z=Z, time=time, delta=delta, logs2=log(1)
        ),
        parameters=list(
            beta1=0, beta2=0, gama1=0, gama2=0, w1=1, w2=1, R=R
        ),
        DLL=dll1, random='R', hessian=TRUE, silent=TRUE
    )
    opt <- nlminb(
        obj$par,
        obj$fn,
        obj$gr, control=list(eval.max=1e3, iter.max=500)
    )
    dll1.out[i, ] <- c(opt$par, opt$convergence) 
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}

rbind(dll0.out, dll1.out)

## LAST BUT NOT LEAST, estimating the single parameter latent structure
dll2.out <- matrix(
    NA, nrow=n+1, ncol=8,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true), 'conv'))
)
dll2.out[n+1, ] <- c(coefs.true, NaN)

for (i in seq(n))
{
    checkDLL(dll2)
    obj <- MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=delta),
                     parameters=list(
                         ## starting in zero doesn't work
                         beta1=dll0.out[i, 1], beta2=dll0.out[i, 2],
                         gama1=dll0.out[i, 3], gama2=dll0.out[i, 4],
                         w1=dll0.out[i, 5], w2=dll0.out[i, 6], R=R,
                         ## log(1e-2) and log(0.1) didn't work
                         logs2=log(0.5)
                     ),
                     DLL=dll2, random='R', hessian=TRUE, silent=TRUE)
    opt <- try(nlminb(obj$par,
                      obj$fn,
                      obj$gr, control=list(eval.max=1e3, iter.max=500)),
               silent=TRUE)
    if (class(opt)!='try-error')
    {
        dll2.out[i, ] <- c(opt$par, opt$convergence)
    }
    print(paste('Model', i, 'done'))
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}

dll2.out
rbind(dll0.out, dll1.out)
