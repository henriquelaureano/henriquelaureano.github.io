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
    ggplot(data=cif.obj$dat, aes(x=time, y=cif, linetype=label))+
        geom_line()+
        labs(x=NULL, y=NULL, color=NULL,
             title=paste('Censorship %:', cif.obj$censorship))+
        theme_minimal()+
        theme(legend.position='bottom',
              legend.box='vertical',
              legend.margin=margin(), 
              legend.title=element_blank(),
              legend.text=element_text(size=12),
              axis.text.x=element_text(size=11),
              axis.text.y=element_text(size=11),
              plot.title=element_text(size=12, face='bold'))
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
pc2 <- plotcif(dat2)
pc1|pc2

## ---------------------------------------------------------------------
J <- 1e4;cs <- 2

time <- runif(J*cs, 30, 79.9)
## time <- sample(c(runif(J*cs*0.60, 30, 49.9), 
##                  runif(J*cs*0.30, 50, 64.9), 
##                  runif(J*cs*0.10, 65, 79.9)), J*cs)
tf <- tibble(
    as_tibble(
        datasimu(
            J=J, cs=cs, time=time, beta=beta, gama=gama, w=w,
            latent='complete'
        )
    ),
    time=time)%>%filter(y3==0)
hist(tf$time, xlim=c(30, 80), main=nrow(tf))
