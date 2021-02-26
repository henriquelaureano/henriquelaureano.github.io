##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-25 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
source('functions.R')

## install.packages('pacman')
pacman::p_load(tidyverse, ## tibble + ggplot2
               patchwork) ## plots arrangement

plotcif <- function(cif.obj)
{
    ggplot(data=cif.obj$dat, aes(x=time, y=cif, color=label))+
        geom_line(size=2)+
        labs(x=NULL, y=NULL, color=NULL,
             title=paste('Censorship %:', cif.obj$censorship))+
        theme(legend.position=c(0.3, 0.85))
}
time <- seq(from=30, to=79.9, length.out=100)

beta <- c(-2.0, -1.5)
gama <- c( 1.0,  1.5)
w    <- c( 3.0,  4.0)
set.seed(1442)
dat1 <- cif(time=time, beta=beta, gama=gama, w=w)

beta <- c(3.0,  2.6)
gama <- c(2.5,  4.0)
w    <- c(5.0, 10.0)
set.seed(1487)
dat2 <- cif(time=time, beta=beta, gama=gama, w=w)

pc1 <- plotcif(dat1)
pc2 <- ggplot()+
        geom_line(
        mapping=aes(x=time, y=cif, group=label), data=dat1$dat, size=2,
        alpha=0.25
    )+
    geom_line(
        mapping=aes(x=time, y=cif, color=label), data=dat2$dat, size=2
    )+
    labs(x=NULL, y=NULL, color=NULL,
         title=paste('Censorship %:', dat2$censorship)
         )+
    theme(
        legend.position=c(0.3, 0.85)
    )
pc1|pc2
