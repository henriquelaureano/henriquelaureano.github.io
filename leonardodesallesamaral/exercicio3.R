## ---------------------------------------------------------------------
##                                             Leonardo de Salles Amaral
##                                                             RA 770617
##                                                                UFSCAR
## ---------------------------------------------------------------------

## mc ------------------------------------------------------------------

mc <- function(x=617, a, m=2**13-1, length=500)
{
    xs    <- rep(NA, length)
    xs[1] <- x
    for (i in 2:length) xs[i] <- a * xs[i-1] %% m
    return(xs)
}
xa17 <- mc(a=17)
xa85 <- mc(a=85)

## mc_summaries --------------------------------------------------------

cbind('a'=c(17, 85), rbind(summary(xa17), summary(xa85)))

## mc_cor_lag1 ---------------------------------------------------------

library(tibble)
library(ggplot2)
library(patchwork)

mc.cor <- function(mc_seq, lag=1)
{
    size <- length(mc_seq)
    x    <- mc_seq[lag:size - lag]
    y    <- mc_seq[(lag + 1):size]
    tibble::tibble(x, y)|>
        ggplot(aes(x, y))+
        geom_point(size=2, shape=21)+    
        labs(x=bquote(x[i]),
             y=bquote(x[i+.(lag)]),
             title=paste0('Correlação entre\npares de lag ', lag, ': ',
                          round(cor(x, y), 3)))+
        theme_minimal()+
        theme(axis.text.x =element_blank(), 
              axis.title.x=element_text(size=14),
              axis.text.y =element_blank(),
              axis.title.y=element_text(size=14),
              plot.title  =element_text(face='bold'))
}
mc.cor(xa17) + mc.cor(xa85)

## mc_cor_lag2 ---------------------------------------------------------

mc.cor(xa17, lag=2) + mc.cor(xa85, lag=2)

## mc_mat --------------------------------------------------------------

(a12 <- c(0, 0:17, 17))
(a21 <- c(0, 17:0, 17))

mc.mat<- function(x1=617, x2=770, a11=17, a22=85, a12, a21, 
                  m=2**13-1, length=500)
{
    xs      <- matrix(NA, nrow=length, ncol=2)
    xs[1, ] <- c(x1, x2)
    A <- matrix(c(a11, a21, a12, a22), nrow=2, ncol=2)
    for (i in 2:length) xs[i, ] <- A %*% xs[i-1, ] %% m
    return(xs)
}
xs_mat  <- vector('list', 20)

for (i in seq(20)) xs_mat[[i]] <- mc.mat(a12=a12[i], a21=a21[i])

## mc_mat_vcov ---------------------------------------------------------

xs_vcov <- vector('list', 20)

for (i in seq(20)) xs_vcov[[i]] <- var(xs_mat[[i]])

mc_mat_vcov_plot -------------------------------------------------------

value <- numeric(80)
k     <- 1
id    <- numeric(20)

for (i in seq(20))
{
    for (j in c(2, 1, 4, 3)) ## a12, a11, a22, a21 ---------------------
    {
        value[k] <- xs_vcov[[i]][j]
        k        <- k+1
    }
    id[i] <- paste('a12 =', a12[i], '& a21 =', a21[i])
}

library(scales)
library(dplyr)
library(haven)

dat <- tibble::tibble(id   =rep(id, each=4),
                      x    =rep(rep(1:2, each =2), times=20),  
                      y    =rep(rep(1:2, times=2), times=20), 
                      value=value)|>
    dplyr::mutate(id=haven::as_factor(id))
dat|>
    ggplot(aes(x, y, fill=value))+
    facet_wrap(~ id)+
    geom_tile()+
    scale_fill_distiller(palette='Spectral',
                         labels=scales::comma, n.breaks=6)+
    geom_text(aes(label=round(value, 0)), color='white', size=3.3)+
    labs(x=NULL, y=NULL, fill=NULL)+
    theme_classic()+
    theme(legend.position     ='bottom',
          legend.justification=c(0.65, 0),
          legend.key.width    =ggplot2::unit(3, 'cm'),
          legend.key.height   =ggplot2::unit(0.4, 'cm'),
          axis.text.x         =element_blank(),
          axis.text.y         =element_blank())

## mc_mat_var_plot -----------------------------------------------------

library(tidyr)

value_var <- dat|>
    dplyr::mutate(var=ifelse(x != y, value, NA))|>
    tidyr::drop_na()|>
    dplyr::pull(value)

tibble::tibble(a12, a21,
               '11'=value_var[seq(1, 40, by=2)],
               '22'=value_var[seq(2, 40, by=2)])|>
    tidyr::pivot_longer(`11`:`22`, names_to='var')|>
    ggplot(aes(x=a12, y=a21, fill=value))+
    geom_tile(color='black')+
    facet_wrap(~ var,
               labeller=label_bquote(var[.(var)]))+
    scale_fill_distiller(palette='Spectral')+
    scale_x_continuous(breaks=seq(0, 16, by=2))+
    scale_y_continuous(breaks=seq(0, 16, by=2))+
    labs(x=bquote(a[12]),
         y=bquote(a[21]), fill='Variância: ')+
    theme_classic()+
    theme(legend.position     ='bottom',
          legend.justification=c(0.65, 0),
          legend.key.width    =ggplot2::unit(2.65, 'cm'),
          legend.key.height   =ggplot2::unit(0.4, 'cm'),
          axis.title.x        =element_text(
              size=12, 
              margin=ggplot2::unit(c(t=3, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y        =element_text(
              size=12, 
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x        =element_text(size=12))

## mc_mat_cov_plot -----------------------------------------------------

value_cov <- dat|>
    dplyr::mutate(var=ifelse(x == y, value, NA))|>
    tidyr::drop_na()|>
    dplyr::pull(value)

tibble::tibble(a12, a21,
               cov=value_cov[seq(1, 40, by=2)])|>
    ggplot(aes(x=a12, y=a21, fill=cov))+
    geom_tile(color='black')+
    scale_fill_distiller(palette='Spectral',
                         n.breaks=8, labels=scales::comma)+
    scale_x_continuous(breaks=seq(0, 16, by=2))+
    scale_y_continuous(breaks=seq(0, 16, by=2))+
    labs(x=bquote(a[12]),
         y=bquote(a[21]), fill=NULL, title='Covariância')+
    theme_classic()+
    theme(legend.key.width    =ggplot2::unit(0.4, 'cm'),
          legend.key.height   =ggplot2::unit(1.55, 'cm'),
          axis.title.x        =element_text(
              size=12, 
              margin=ggplot2::unit(c(t=3, r=0, b=0, l=0), 'mm')
          ),
          axis.title.y        =element_text(
              size=12, 
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ))
