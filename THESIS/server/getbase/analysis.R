##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-05 · Curitiba/PR/Brazil
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
                         ## using dll0.out and dll1.out also doesn't
                         ## work
                         beta1=coefs.true['beta1'],
                         beta2=coefs.true['beta2'],
                         gama1=coefs.true['gama1'],
                         gama2=coefs.true['gama2'],
                         w1=coefs.true['w1'],
                         w2=coefs.true['w2'],
                         R=R,
                         ## log(1e-2), log(0.1), log(0.5), log(1) (TRUE
                         ## VALUE), and log(2) didn't work
                         logs2=coefs.true['logs2']
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
## EVEN with the true values as starting points, the model doesn't work
dll2.out

##----------------------------------------------------------------------

## NOW, let's try a more 'shy' latent structure variance ---------------
y <- future_map(rep(J, n), ~datasimu(.x,
                                     time=time,
                                     Sigma=diag(rep(0.25, 4))),
                .options=furrr_options(seed=NULL))

## NEW true parameter values -------------------------------------------
coefs.true <- c(beta1=-2, beta2=-1.5, gama1=1.2, gama2=1, w1=3, w2=5,
                logs2=log(0.25))

## AGAIN, first just a GLM (ignoring the latent structure) -------------
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
## much closer to the true values than with logs2=log(1)
dll0.out

## FIXING the latent structure parameter -------------------------------
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
            Y=y[[i]], Z=Z, time=time, delta=delta, logs2=log(0.25)
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

## OKAY, until now we fixed the latent structure parameter in its real
## value but what happens to the estimates if we fix it in a different
## value? let's say log(0.75)
dll1OLD.out <- dll1.out
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
            Y=y[[i]], Z=Z, time=time, delta=delta, logs2=log(0.75)
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
## reasonable/expected
rbind(dll0.out, dll1OLD.out, dll1.out)

## let's try again to estimate the latent structure parameter ----------
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
                         ## using dll0.out and dll1.out also doesn't
                         ## work
                         beta1=coefs.true['beta1'],
                         beta2=coefs.true['beta2'],
                         gama1=coefs.true['gama1'],
                         gama2=coefs.true['gama2'],
                         w1=coefs.true['w1'],
                         w2=coefs.true['w2'],
                         R=R,
                         ## log(1e-2), log(0.1), log(0.5), log(1) (TRUE
                         ## VALUE), and log(2) didn't work
                         logs2=coefs.true['logs2']
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
## EVEN with the true values as starting points, the model doesn't work
dll2.out

## AND if we try to estimate the variance directly? --------------------
dll3 <- 'multiGLMM_diag1s2'
compile(paste0(dll3, '.cpp'))

coefs.true <- c(beta1=-2, beta2=-1.5, gama1=1.2, gama2=1, w1=3, w2=5,
                s2=0.25)

dll3.out <- matrix(
    NA, nrow=n+1, ncol=8,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true), 'conv'))
)
dll3.out[n+1, ] <- c(coefs.true, NaN)

for (i in seq(n))
{
    checkDLL(dll3)
    obj <- MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=delta),
                     parameters=list(
                         ## let's start with the true values as starting
                         ## points, just to see if the model converges
                         beta1=coefs.true['beta1'],
                         beta2=coefs.true['beta2'],
                         gama1=coefs.true['gama1'],
                         gama2=coefs.true['gama2'],
                         w1=coefs.true['w1'],
                         w2=coefs.true['w2'],
                         R=R,
                         s2=coefs.true['s2']
                     ),
                     DLL=dll3, random='R', hessian=TRUE, silent=TRUE)
    opt <- try(nlminb(obj$par,
                      obj$fn,
                      obj$gr, control=list(eval.max=1e3, iter.max=500)),
               silent=TRUE)
    if (class(opt)!='try-error')
    {
        dll3.out[i, ] <- c(opt$par, opt$convergence)
    }
    print(paste('Model', i, 'done'))
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}
## CONVERGENCE 1 AND THE VARIANCE GOT LOST
dll3.out

## BEFORE we try different starting points is worthy to notice that
## modeling the crude variance takes much more time. Let's then
## constraint the optimization and see what's happens
dll3old.out <- dll3.out

dll3.out <- matrix(
    NA, nrow=n+1, ncol=8,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true), 'conv'))
)
dll3.out[n+1, ] <- c(coefs.true, NaN)

for (i in seq(n))
{
    checkDLL(dll3)
    obj <- MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=delta),
                     parameters=list(
                         beta1=coefs.true['beta1'],
                         beta2=coefs.true['beta2'],
                         gama1=coefs.true['gama1'],
                         gama2=coefs.true['gama2'],
                         w1=coefs.true['w1'],
                         w2=coefs.true['w2'],
                         R=R,
                         s2=coefs.true['s2']
                     ),
                     DLL=dll3, random='R', hessian=TRUE, silent=TRUE)
    opt <- try(
        nlminb(obj$par,
               obj$fn,
               obj$gr, control=list(eval.max=1e3, iter.max=500),
               lower=c(-Inf, -Inf, -Inf, -Inf, 1e-16, 1e-16, 1e-16)),
        silent=TRUE)
    if (class(opt)!='try-error')
    {
        dll3.out[i, ] <- c(opt$par, opt$convergence)
    }
    print(paste('Model', i, 'done'))
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}
## CONVERGENCE ZERO BUT THE VARIANCE ESTIMATE IS WRONG (MUCH FASTER)
dll3.out

## DIFFERENT starting point --------------------------------------------
dll3.out <- matrix(
    NA, nrow=n+1, ncol=8,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true), 'conv'))
)
dll3.out[n+1, ] <- c(coefs.true, NaN)

for (i in seq(n))
{
    checkDLL(dll3)
    obj <- MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=delta),
                     parameters=list(
                         beta1=0,
                         beta2=0,
                         gama1=0,
                         gama2=0,
                         w1=1,
                         w2=1,
                         R=R,
                         s2=1e-16
                     ),
                     DLL=dll3, random='R', hessian=TRUE, silent=TRUE)
    opt <- try(
        nlminb(obj$par,
               obj$fn,
               obj$gr, control=list(eval.max=1e3, iter.max=500),
               lower=c(-Inf, -Inf, -Inf, -Inf, 1e-16, 1e-16, 1e-16)),
        silent=TRUE)
    if (class(opt)!='try-error')
    {
        dll3.out[i, ] <- c(opt$par, opt$convergence)
    }
    print(paste('Model', i, 'done'))
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}
## THE FIXED EFFECT ESTIMATES ARE QUITE GOOD, THE PROBLEM IS STILL WITH
## THE VARIANCE COMPONENT
dll3.out

## a different variance starting guess ---------------------------------
dll3old.out <- dll3.out

dll3.out <- matrix(
    NA, nrow=n+1, ncol=8,
    dimnames=list(c(seq(n), 'true'), c(names(coefs.true), 'conv'))
)
dll3.out[n+1, ] <- c(coefs.true, NaN)

for (i in seq(n))
{
    checkDLL(dll3)
    obj <- MakeADFun(data=list(Y=y[[i]], Z=Z, time=time, delta=delta),
                     parameters=list(
                         beta1=0,
                         beta2=0,
                         gama1=0,
                         gama2=0,
                         w1=1,
                         w2=1,
                         R=R,
                         s2=1
                     ),
                     DLL=dll3, random='R', hessian=TRUE, silent=TRUE)
    opt <- try(
        nlminb(obj$par,
               obj$fn,
               obj$gr, control=list(eval.max=1e3, iter.max=500),
               lower=c(-Inf, -Inf, -Inf, -Inf, 1e-16, 1e-16, 1e-16)),
        silent=TRUE)
    if (class(opt)!='try-error')
    {
        dll3.out[i, ] <- c(opt$par, opt$convergence)
    }
    print(paste('Model', i, 'done'))
    ## sdr <- sdreport(obj)
    FreeADFun(obj);gc()
}
## THE SAME (GOOD AND BAD)
rbind(dll3old.out, dll3.out)

## ONE MORE TIME -------------------------------------------------------

## END -----------------------------------------------------------------
