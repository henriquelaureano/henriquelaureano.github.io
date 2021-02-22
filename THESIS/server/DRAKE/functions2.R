##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-22 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA

sigmaPD <- function(s2_1, s2_2, s2_3, s2_4,
                    rho12, rho13, rho14, rho23, rho24, rho34)
{
    cov12 <- rho12*sqrt(s2_1)*sqrt(s2_2)
    cov13 <- rho13*sqrt(s2_1)*sqrt(s2_3)
    cov14 <- rho14*sqrt(s2_1)*sqrt(s2_4)
    cov23 <- rho23*sqrt(s2_2)*sqrt(s2_3)
    cov24 <- rho24*sqrt(s2_2)*sqrt(s2_4)
    cov34 <- rho34*sqrt(s2_3)*sqrt(s2_4)

    Sigma <- matrix(c(s2_1, cov12, cov13, cov14,
                      cov12, s2_2, cov23, cov24,
                      cov13, cov23, s2_3, cov34,
                      cov14, cov24, cov34, s2_4), nrow=4)

    if (is.matrix(chol(Sigma)))
    
    return(Sigma)
}

ps <- function(J, cs, time, delta=80, beta, gama, w, Sigma=NULL)
{
    ZU <- matrix(0, nrow=cs*J, ncol=2)

    if (!is.null(Sigma)) {

        if (!is.matrix(Sigma) | dim(Sigma)[1]!=4 | dim(Sigma)[2]!=4)
            stop('Sigma must be a 4x4 matrix')

        Z <- Matrix::bdiag(replicate(J, rep(1, cs), simplify=FALSE))
        U <- mvtnorm::rmvnorm(J,
                              mean=rep(0, dim(Sigma)[1]),
                              sigma=Sigma)
        ZU <- Z%*%U
    }
    risk1 <- exp(beta[1] + ZU[, 1])
    risk2 <- exp(beta[2] + ZU[, 2])

    level <- 1 + risk1 + risk2

    gt  <- 0.5*log(time/(delta-time))
    dgt <- delta/(2*time*(delta-time))

    x1 <- w[1]*gt - gama[1] - ZU[, 3]
    x2 <- w[2]*gt - gama[2] - ZU[, 4]
    
    p1 <- risk1/level * w[1]*dgt * dnorm(x1)
    p2 <- risk2/level * w[2]*dgt * dnorm(x2)

    p3 <- 1 - p1 - p2

    return(cbind(p1, p2, p3))
}

datasimu <- function(J, cs, time, delta=80, beta, gama, w, Sigma=NULL)
{
    ps <- ps(J=J, cs=cs, time=time, delta=delta,
             beta=beta, gama=gama, w=w, Sigma=Sigma)

    out <- mc2d::rmultinomial(cs*J, 1, prob=ps)
    colnames(out) <- paste0('y', 1:3)

    return(out)
}

future_datasimu <- function(J, cs, n, time, delta=80, beta, gama, w,
                            Sigma=NULL)
{
    y <- furrr::future_map(
                    rep(J, n),
                    ~datasimu(.x, cs=cs, time=time, delta=delta,
                              beta=beta, gama=gama, w=w, Sigma=Sigma),
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
