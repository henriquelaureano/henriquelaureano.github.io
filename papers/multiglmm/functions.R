##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-mar-18 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
## multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA

sigmaPD2 <- function(s2_1, s2_2, rho12)
{
    cov12 <- rho12*sqrt(s2_1)*sqrt(s2_2)

    Sigma <- matrix(c(s2_1, cov12, cov12, s2_2), nrow=2)

    if (is.matrix(chol(Sigma)))
    
    return(Sigma)
}

sigmaPD4 <- function(s2_1, s2_2, s2_3, s2_4,
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

ps2 <- function(J, cs, time, delta=80, beta, gama, w,
                type=c('risk', 'trajectory'), Sigma=NULL)
{
    ZU <- matrix(0, nrow=cs*J, ncol=2)

    if (!is.null(Sigma)) {

        if (!is.matrix(Sigma) | dim(Sigma)[1]!=2 | dim(Sigma)[2]!=2)
            stop('Sigma must be a 2x2 matrix')

        Z <- Matrix::bdiag(replicate(J, rep(1, cs), simplify=FALSE))
        U <- mvtnorm::rmvnorm(J,
                              mean=rep(0, dim(Sigma)[1]),
                              sigma=Sigma)
        ZU <- Z%*%U
    }
    gt <- 0.5*log(time/(delta-time))
    
    switch(
        type,
        risk=
            {
                risk1 <- exp(beta[1] + ZU[, 1])
                risk2 <- exp(beta[2] + ZU[, 2])
                
                x1 <- w[1]*gt - gama[1]
                x2 <- w[2]*gt - gama[2]
            },
        trajectory=
            {
                risk1 <- exp(beta[1])
                risk2 <- exp(beta[2])
                
                x1 <- w[1]*gt - gama[1] - ZU[, 1]
                x2 <- w[2]*gt - gama[2] - ZU[, 2]
            },
        stop("Argument ``type`` off the labels")
    )
    dgt <- delta/(2*time*(delta-time))

    level <- 1 + risk1 + risk2

    p1 <- risk1/level * w[1]*dgt * dnorm(x1)
    p2 <- risk2/level * w[2]*dgt * dnorm(x2)

    p3 <- 1 - p1 - p2

    return(cbind(p1, p2, p3))
}

ps4 <- function(J, cs, time, delta=80, beta, gama, w, Sigma=NULL)
{
    ZU <- matrix(0, nrow=cs*J, ncol=4)

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

datasimu <- function(J, cs, time, delta=80, beta, gama, w,
                     latent=c('partial', 'complete'), type=NULL,
                     Sigma=NULL)
{
    switch(latent,
           partial={ ps <- ps2(J=J, cs=cs, time=time, delta=delta,
                               beta=beta, gama=gama, w=w,
                               type=type, Sigma=Sigma)
           },
           complete={ ps <- ps4(J=J, cs=cs, time=time, delta=delta,
                                beta=beta, gama=gama, w=w, Sigma=Sigma)
           },
           stop('Sigma must be a 2x2 or 4x4 matrix'))
    
    out <- mc2d::rmultinomial(cs*J, 1, prob=ps)
    colnames(out) <- paste0('y', 1:3)

    return(out)
}

future_datasimu <- function(J, cs, n, time, delta=80, beta, gama, w,
                            latent=c('partial', 'complete'), type=NULL,
                            Sigma=NULL, tag)
{
    tictoc::tic()

    y <- furrr::future_map(
                    rep(J, n),
                    ~datasimu(.x, cs=cs, time=time, delta=delta,
                              beta=beta, gama=gama, w=w,
                              latent=latent, type=type, Sigma=Sigma),
                    .options=furrr_options(seed=NULL))

    save(y, time, J, cs, beta, gama, w,
         file=paste0('data/', tag, '.RData'), version=2)

    return( tictoc::toc() )
}

matrixZ <- function(J, cs)
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
        dyn.load(TMB::dynlib(paste0('cpps/', dll)))
        invisible(TMB::config(tape.parallel=FALSE, DLL=dll))
    }
}

cif <- function(time, delta=80, beta, gama, w)
{
    risk1 <- exp(beta[1])
    risk2 <- exp(beta[2])

    level <- 1 + risk1 + risk2

    gt <- 0.5*log(time/(delta-time))

    x1 <- w[1]*gt - gama[1]
    x2 <- w[2]*gt - gama[2]
    
    cif1 <- risk1/level * pnorm(x1)
    cif2 <- risk2/level * pnorm(x2)

    label.cif1 <- paste0(
        'CIF1: beta1=', beta[1], ', gama1=', gama[1], ', w1=', w[1]
    )
    label.cif2 <- paste0(
        'CIF2: beta2=', beta[2], ', gama2=', gama[2], ', w2=', w[2]
    )
    n <- length(time)

    dat <- tibble::tibble(time =rep(time, 2),
                          cif  =c(cif1, cif2),
                          label=c(rep(label.cif1, n),
                                  rep(label.cif2, n)))

    dgt <- delta/(2*time*(delta-time))
    
    p1 <- risk1/level * w[1]*dgt * dnorm(x1)
    p2 <- risk2/level * w[2]*dgt * dnorm(x2)

    p3 <- 1 - p1 - p2

    y <- mc2d::rmultinomial(n, 1, prob=cbind(p1, p2, p3))

    censorship <- prop.table(colSums(y))[3]

    return(list(dat=dat, censorship=censorship))
}
