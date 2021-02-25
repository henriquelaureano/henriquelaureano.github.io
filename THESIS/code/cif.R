##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-24 · Curitiba/PR/Brazil
##----------------------------------------------------------------------
source('functions.R')

## install.packages('pacman')
pacman::p_load(tidyverse) ## tibble + ggplot2

plotcif <- function(cif.obj)
{
    ggplot(data=cif.obj$dat, aes(x=time, y=cif, color=label))+
        geom_line(size=2)+
        labs(x=NULL, y=NULL, color=NULL,
             title=paste('Censorship %:', cif.obj$censorship))+
        theme(legend.position=c(0.2, 0.9))
}
time <- seq(from=30, to=79.9, length.out=100)

beta <- c(-2,   -1.5)
gama <- c( 1.2,  1  )
w    <- c( 3,    5  )
plotcif(cif(time=time, beta=beta, gama=gama, w=w))

beta <- c(5, 3.75)
gama <- c(1, 1.5)
w    <- c(9, 6)
plotcif(cif(time=time, beta=beta, gama=gama, w=w))
