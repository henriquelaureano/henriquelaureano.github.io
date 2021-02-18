##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-18 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

ps.l1 <- function(J, cs, beta, sd)
{
    if (length(sd) > 2)
        stop('off-grid, fn built for a 3 classes multinomial model')
    
    blocks <- replicate(J, rep(1, cs), simplify=FALSE)
    Z <- Matrix::bdiag(blocks)

    if (length(sd) == 1)
    {
        u <- rnorm(n=J, mean=0, sd=sd);zu <- Z%*%u        

        risk1 <- exp(beta[1] + zu)
        risk2 <- exp(beta[2] + zu)
    } else
    {
        u1 <- rnorm(n=J, mean=0, sd=sd[1]);zu1 <- Z%*%u1
        u2 <- rnorm(n=J, mean=0, sd=sd[2]);zu2 <- Z%*%u2
         
        risk1 <- exp(beta[1] + zu1)
        risk2 <- exp(beta[2] + zu2)
    }
    level <- 1 + risk1 + risk2
    p1 <- risk1/level
    p2 <- risk2/level
    p3 <- 1 - p1 - p2

    return(cbind(p1, p2, p3))
}

ps.l2 <- function(J, cs, beta, Sigma=NULL)
{
    LE <- matrix(0, nrow=cs*J, ncol=2)

    if (!is.null(Sigma)) {

        if (!is.matrix(Sigma) | dim(Sigma)[1]!=2 | dim(Sigma)[2]!=2)
            stop('Sigma must be a 2x2 matrix')

        Z <- Matrix::bdiag(replicate(J, rep(1, cs), simplify=FALSE))
        R <- mvtnorm::rmvnorm(J,
                              mean=rep(0, dim(Sigma)[1]),
                              sigma=Sigma)
        LE <- Z%*%R
    }
    risk1 <- exp(beta[1]+LE[, 1])
    risk2 <- exp(beta[2]+LE[, 2]);level <- 1+risk1+risk2

    p1 <- risk1/level
    p2 <- risk2/level;p3 <- 1-p1-p2

    return(cbind(p1, p2, p3))
}

datasimu.l1 <- function(J, cs, beta, sd)
{
    ps <- ps.l1(J=J, cs=cs, beta=beta, sd=sd)

    out <- mc2d::rmultinomial(cs*J, 1, prob=ps)
    colnames(out) <- paste0('y', 1:3)

    return(out)
}

datasimu.l2 <- function(J, cs, beta, Sigma=NULL)
{
    ps <- ps.l2(J=J, cs=cs, beta=beta, Sigma=Sigma)

    out <- mc2d::rmultinomial(cs*J, 1, prob=ps)
    colnames(out) <- paste0('y', 1:3)

    return(out)
}

future_datasimu.l2 <- function(J, cs, n, beta, Sigma=NULL)
{
    y <- furrr::future_map(
                    rep(J, n),
                    ~datasimu(.x, cs=cs, beta=beta, Sigma=Sigma),
                    .options=furrr_options(seed=NULL))
    return(y)
}

modelZ <- function(J, cs)
{
    blocks <- replicate(J, rep(1, cs), simplify=FALSE)

    Z <- Matrix::bdiag(blocks)
    
    return(Z)
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

getguess <- function(dll, y, beta)
{
    if (names(beta)[1] != 'beta1' | names(beta)[2] != 'beta2')
        stop('Check beta names')

    n <- length(y)

    out <- matrix(NA, nrow=n, ncol=length(beta))
    colnames(out) <- names(beta)
    
    for (i in seq(n))
    {
        checkDLL(dll)
        obj <- TMB::MakeADFun(data=list(Y=y[[i]]),
                              parameters=list(beta1=beta['beta1'],
                                              beta2=beta['beta2']),
                              DLL=dll,
                              hessian=TRUE, silent=TRUE)
        
        opt <- nlminb(obj$par, obj$fn, obj$gr)

        out[i, ] <- opt$par
    }
    return(out)
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
