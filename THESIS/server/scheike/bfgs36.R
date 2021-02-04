## henrique laureano (.github.io)
## date: 1-fev-2021
## scheike's pairwise model via BFGS in the structure 36----------------

(args <- commandArgs())
i <- abs(as.numeric(args[7]))

pacman::p_load('Rcpp', 'mets')

source('kkholst-mcif/inst/examples/helpfunctions_test.R')
sourceCpp('kkholst-mcif/src/loglik.cpp')
load('../data36.RData')

scheike <- function(theta, t, delta, J, causes) {
    vcv <- matrix(0, nrow=4, ncol=4)
    rhoZ_12 <- tanh(theta['rho_12'])
    rhoZ_13 <- tanh(theta['rho_13'])
    rhoZ_14 <- tanh(theta['rho_14'])
    rhoZ_23 <- tanh(theta['rho_23'])
    rhoZ_24 <- tanh(theta['rho_24'])
    rhoZ_34 <- tanh(theta['rho_34'])
    vcv[1, ] <- c(
        exp(theta['s2_1']),
        rhoZ_12*sqrt(exp(theta['s2_1']))*sqrt(exp(theta['s2_2'])),
        rhoZ_13*sqrt(exp(theta['s2_1']))*sqrt(exp(theta['s2_3'])),
        rhoZ_14*sqrt(exp(theta['s2_1']))*sqrt(exp(theta['s2_4'])))
    vcv[2, ] <- c(
        rhoZ_12*sqrt(exp(theta['s2_2']))*sqrt(exp(theta['s2_1'])),
        exp(theta['s2_2']),
        rhoZ_23*sqrt(exp(theta['s2_2']))*sqrt(exp(theta['s2_3'])),
        rhoZ_24*sqrt(exp(theta['s2_2']))*sqrt(exp(theta['s2_4'])))
    vcv[3, ] <- c(
        rhoZ_13*sqrt(exp(theta['s2_3']))*sqrt(exp(theta['s2_1'])),
        rhoZ_23*sqrt(exp(theta['s2_3']))*sqrt(exp(theta['s2_2'])),
        exp(theta['s2_3']),
        rhoZ_34*sqrt(exp(theta['s2_3']))*sqrt(exp(theta['s2_4'])))
    vcv[4, ] <- c(
        rhoZ_14*sqrt(exp(theta['s2_4']))*sqrt(exp(theta['s2_1'])),
        rhoZ_24*sqrt(exp(theta['s2_4']))*sqrt(exp(theta['s2_2'])),
        rhoZ_34*sqrt(exp(theta['s2_4']))*sqrt(exp(theta['s2_3'])),
        exp(theta['s2_4']))
    gt1 <- gt2 <- atanh((t-delta/2)/(delta/2))
    dgt1 <- dgt2 <- 0.5*delta/(t*(delta-t))
    alpha <- cbind(gt1*theta['a1'], gt1*theta['a2'],
                   gt2*theta['a1'], gt2*theta['a2'])
    dalpha <- cbind(dgt1*theta['a1'], dgt1*theta['a2'],
                    dgt2*theta['a1'], dgt2*theta['a2'])
    x.1 <- as.matrix(cbind(rep(1, 2*J)))
    x.2 <- as.matrix(cbind(rep(1, 2*J)))
    beta <- cbind(x.1%*%theta['b1'], x.1%*%theta['b2'],
                  x.2%*%theta['b1'], x.2%*%theta['b2'])
    gamma <- cbind(x.1%*%theta['g1'], x.1%*%theta['g2'],
                   x.2%*%theta['g1'], x.2%*%theta['g2'])
    out <- loglik(sigma=SigmaGen(vcv, 2, old=FALSE),
                  ncauses=2, causes=causes,
                  alpha=alpha, dalpha=dalpha,
                  beta=beta, gamma=gamma,
                  eb0=t(matrix(0, nrow=2*J, ncol=2)), nq=3)
    return(-sum(out))
}

theta <- c(b1=initFixed[i, 1], b2=initFixed[i, 2],
           g1=initFixed[i, 3], g2=initFixed[i, 4],
           a1=initFixed[i, 5], a2=initFixed[i, 6],
           s2_1=log(0.2), s2_2=log(0.3), s2_3=log(0.4), s2_4=log(0.5),
           rho_12=atanh(0.15/sqrt(0.2*0.3)),
           rho_13=atanh(0.1/sqrt(0.2*0.4)),
           rho_14=atanh(0.2/sqrt(0.2*0.5)),
           rho_23=atanh(0.2/sqrt(0.3*0.4)), 
           rho_24=atanh(0.1/sqrt(0.3*0.5)),
           rho_34=atanh(0.15/sqrt(0.4*0.5)))

J <- 3e4
t <- rep(seq(from=30, to=79.5, by=0.5), length.out=2*J)
causes <- y[[i]][, 1:2]

## scheike(theta=theta, t=t, delta=80, J=J, causes=causes)

where <- 'bfgs36'

opt <- try(optim(theta, scheike, t=t, delta=80, J=J, causes=causes,
                 method='BFGS', control=list(maxit=500)),
           silent=TRUE)
if (class(opt)!='try-error') {
    write.table(rbind(c(opt$par, opt$convergence)),
                file=paste0(where, '.txt'), append=TRUE, col.names=FALSE)
}
