##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-mar-29 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

library(mvtnorm)   ## install.packages('mvtnorm')
library(tidyverse) ## install.packages('tidyverse')
library(mc2d)      ## install.packages('mc2d')

datasimu <- function(J,    ## number of clusters
                     cs,   ## clusters size (all the same size)  
                     time, ## failure, censorship times
                     Z,    ## latent effects design-matrix
                     S,    ## variance-covariance matrix
                     delta=80,
                     beta =c( beta1=-2.0,  beta2=-1.5),
                     gamma=c(gamma1= 1.2, gamma2= 1.0),
                     w    =c(    w1= 3.0,     w2= 5.0),
                     seed1=NULL,
                     seed2=NULL)
{
    out <- tibble::tibble(i=rep(seq(cs), times=J), ## cluster element
                          j=rep(seq(J),  each=cs), ## cluster
                          time=time,
                          p1=NA,
                          p2=NA,
                          p3=NA)

    K     <- dim(S)[1]/2 + 1
    ladim <- 2*(K-1) ## latent effects dimension
    set.seed(seed1)
    U     <- mvtnorm::rmvnorm(J, mean=rep(0, ladim), sigma=S)
    ZU    <- Z%*%U
    risk1 <- exp(beta['beta1'] + ZU[, 1])
    risk2 <- exp(beta['beta2'] + ZU[, 2])
    level <- 1 + risk1 + risk2
    gt    <- atanh(2*time/delta - 1)
    dgt   <- delta/(2*time*(delta - time))
    x1    <- w['w1']*gt - gamma['gamma1'] - ZU[, 3]
    x2    <- w['w2']*gt - gamma['gamma2'] - ZU[, 4]
    
    out$p1 <- risk1/level*w['w1']*dgt*dnorm(x1)
    out$p2 <- risk2/level*w['w2']*dgt*dnorm(x2)

    out    <- out %>% dplyr::mutate(p3=1-p1-p2)
    set.seed(seed2)
    y      <- mc2d::rmultinomial(cs*J, 1, prob=out%>%select(p1:p3))

    out    <- out %>%
        dplyr::bind_cols(tibble::as_tibble(y)) %>%
        dplyr::rename(y1=V1, y2=V2, y3=V3)
    return(out)
}
