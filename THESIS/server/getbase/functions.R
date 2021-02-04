##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-03 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

dcif <- function(J, time, delta=80,
                 beta=c(-2, -1.5), gamma=c(1.2, 1), w=c(3, 5),
                 Sigma=NULL)
{
    if (J!=0.5*length(time)) stop('time length must be 2*J')

    LE <- matrix(0, nrow=2*J, ncol=4)

    if (!is.null(Sigma)) {

        if (dim(Sigma)[1]!=4 | dim(Sigma)[2]!=4)
            stop('Sigma must be 4x4')

        Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
        R <- mvtnorm::rmvnorm(J, mean=rep(0, dim(Sigma)[1]), sigma=Sigma)
        LE <- Z%*%R
    }
    
    risk1 <- exp(beta[1]+LE[, 1])
    risk2 <- exp(beta[2]+LE[, 2])
    level <- 1+risk1+risk2

    gt <- atanh(2*time/delta-1)
    dgt <- delta/(2*time*(delta-time))
    
    p1 <- risk1/level*w[1]*dgt*dnorm(w[1]*gt-gamma[1]-LE[, 3])
    p2 <- risk2/level*w[2]*dgt*dnorm(w[2]*gt-gamma[2]-LE[, 4])
    p3 <- 1-p1-p2

    return(cbind(p1, p2, p3))
}

datasimu <- function(J, time, delta=80,
                     beta=c(-2, -1.5), gamma=c(1.2, 1), w=c(3, 5),
                     Sigma=NULL)
{
    ps <- dcif(J=J, time=time, delta=delta,
               beta=beta, gamma=gamma, w=w, Sigma=Sigma)

    out <- mc2d::rmultinomial(2*J, 1, prob=ps)
    colnames(out) <- paste0('y', 1:3)
    return(out)
}

checkDLL <- function(dll)
{
    if (!dll%in%names(getLoadedDLLs()))
    {
        cat(crayon::blue(clisymbols::symbol$star), 'Loading DLL\n')
        dyn.load(dynlib(dll))
        invisible(config(tape.parallel=FALSE, DLL=dll))
    }
}
