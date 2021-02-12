##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-08 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

ps <- function(J, beta, Sigma=NULL)
{
    LE <- matrix(0, nrow=2*J, ncol=2)

    if (!is.null(Sigma)) {

        if (!is.matrix(Sigma) | dim(Sigma)[1]!=2 | dim(Sigma)[2]!=2)
            stop('Sigma must be a 4x4 matrix')

        Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
        R <- mvtnorm::rmvnorm(J,
                              mean=rep(0, dim(Sigma)[1]),
                              sigma=Sigma)
        LE <- Z%*%R
    }
    risk1 <- exp(beta[1]+LE[, 1])
    risk2 <- exp(beta[2]+LE[, 2])
    level <- 1+risk1+risk2

    p1 <- risk1/level
    p2 <- risk2/level
    p3 <- 1-p1-p2

    return(cbind(p1, p2, p3))
}

datasimu <- function(J, beta, Sigma=NULL)
{
    ps <- ps(J=J, beta=beta, Sigma=Sigma)

    out <- mc2d::rmultinomial(2*J, 1, prob=ps)
    colnames(out) <- paste0('y', 1:3)

    return(out)
}

future_datasimu <- function(J, n, beta, Sigma=NULL)
{
    y <- furrr::future_map(
                    rep(J, n), ~datasimu(.x, beta=beta, Sigma=Sigma),
                    .options=furrr_options(seed=NULL))
    return(y)
}

checkDLL <- function(dll)
{
    if (!dll%in%names(getLoadedDLLs()))
    {
        cat('Loading DLL\n')
        dyn.load(TMB::dynlib(dll))
        invisible(TMB::config(tape.parallel=FALSE, DLL=dll))
    }
}

GLMMfit <- function(dll, y, Z, pars)
{
    checkDLL(dll)
    obj <- TMB::MakeADFun(data=list(Y=y, Z=Z),
                          parameters=pars,
                          DLL=dll,
                          random='U', hessian=TRUE, silent=TRUE)
    out <- NULL
    opt <- try(
        nlminb(obj$par, obj$fn, obj$gr), silent=TRUE
    )
    if (class(opt) != 'try-error') out <- opt
    
    return(out)
}

dllOut <- function(optObj, comp2)
{
    if (length(optObj$par) != length(comp2))
        stop("# of parameters don't match")
    
    out <- matrix(
        NA,
        nrow=2,
        ncol=length(comp2)+1,
        dimnames=list(c(1, 'true'), c(names(comp2), 'conv'))
    )
    out[1, ] <- c(optObj$par, optObj$convergence)
    out[2, ] <- c(comp2, NaN)
    
    return(out)
}
