##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                      leg.ufpr.br/~henrique · github.com/mynameislaure
##                                      laureano@ufpr.br · @hap_laureano
##                     Laboratory of Statistics and Geoinformation (LEG)
##       2020-dez-26 · Federal University of Paraná · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## data simulator-------------------------------------------------------
datasimu <- function(J, t, Z, S, 
                     delta=80,
                     beta=c(-2, -1.5), gamma=c(1.2, 1), w=c(3, 5)){
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

## getting initial guesses to the GLMM fixed effects--------------------
library(TMB)
fitGLM <- function(y, t, model) {
    if (!model%in%names(getLoadedDLLs())) {
        cat(crayon::blue(clisymbols::symbol$star), 'Loading DLL\n')
        dyn.load(dynlib(model))
    }
    obj <- MakeADFun(
        data=list(Y=y, T=t, delta=80),
        parameters=list(beta1=0, beta2=0, gama1=0, gama2=0, w1=1, w2=1),
        DLL=model, silent=TRUE)
    optpar <- nlminb(obj$par, obj$fn, obj$gr, eval.max=1e3, iter.max=500
                     )$par
    FreeADFun(obj);gc()
    return(optpar)
}
## END------------------------------------------------------------------
