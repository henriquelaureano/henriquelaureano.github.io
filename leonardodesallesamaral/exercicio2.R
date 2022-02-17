## packages ------------------------------------------------------------

library(tibble)
library(ggplot2)
library(tidyr)

## cosx ----------------------------------------------------------------

tibble::tibble(x =seq(-10, 10, length.out=100),
               fx=cos(x))|>
    ggplot(aes(x=x, y=fx))+
    geom_line(size=1)+
    labs(y    ='f(x)',
         title='cos(x)')+
    scale_x_continuous(breaks=seq(-10, 10, by=2))+
    scale_y_continuous(breaks=seq(-1, 1, by=0.25))+
    theme_minimal(base_size=13)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, r=0, b=0, l=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')),
          plot.title=element_text(face='bold'))

## taylorat0 -----------------------------------------------------------

tibble::tibble(x           =seq(-4, 4, length.out=100),
               cosx        =cos(x),
               taylor1_at_0=1,
               taylor2_at_0=taylor1_at_0 - x**2/2,
               taylor4_at_0=taylor2_at_0 + x**4/24,
               taylor6_at_0=taylor4_at_0 - x**6/720)|>
    tidyr::pivot_longer(!x, names_to='Curva')|>
    ggplot(aes(x=x, y=value, color=Curva))+
    geom_line(size=1)+
    labs(y    ='f(x)',
         title='cos(x) e aproximações de Taylor',
         color=NULL)+
    scale_x_continuous(breaks=seq(-4, 4, by=1))+
    scale_y_continuous(breaks=seq(-10, 10, by=1))+
    theme_minimal(base_size=13)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, r=0, b=0, l=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')),
          plot.title=element_text(face='bold'),
          legend.position=c(0.5, 0.25),
          legend.box.background=element_rect(),
          legend.key.width=ggplot2::unit(1, 'cm'),
          legend.box.margin=ggplot2::unit(c(t=-2, r=1, b=0, l=0), 'mm'))

## taylorat0_smallrange ------------------------------------------------

tibble::tibble(x           =seq(-5e-6, 5e-6, length.out=100),
               cosx        =cos(x),
               taylor1_at_0=1,
               taylor2_at_0=taylor1_at_0 - x**2/2,
               taylor4_at_0=taylor2_at_0 + x**4/24,
               taylor6_at_0=taylor4_at_0 - x**6/720)|>
    tidyr::pivot_longer(!x, names_to='Curva')|>
    ggplot(aes(x=x, y=value, color=Curva))+
    geom_line(size=1)+
    labs(y    ='f(x)',
         title='cos(x) e aproximações de Taylor',
         color=NULL)+
    theme_minimal(base_size=13)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, r=0, b=0, l=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')),
          plot.title=element_text(face='bold'),
          legend.position=c(0.5, 0.4),
          legend.box.background=element_rect(),
          legend.key.width=ggplot2::unit(1, 'cm'),
          legend.box.margin=ggplot2::unit(c(t=-2, r=1, b=0, l=0), 'mm'))

## taylorat2 -----------------------------------------------------------

tibble::tibble(x           =seq(-4, 4, length.out=100),
               cosx        =cos(x),
               taylor1_at_2=cos(2) - (x - 2)*sin(2),
               taylor2_at_2=taylor1_at_2 - 0.5*(x - 2)**2*cos(2),
               taylor4_at_2=taylor2_at_2 +
                   1/6*(x - 2)**3*sin(2) + 1/24*(x - 2)**4*cos(2),
               taylor6_at_2=taylor4_at_2 -
                   1/120*(x - 2)**5*sin(2) - 1/720*(x - 2)**6*cos(2))|>
    tidyr::pivot_longer(!x, names_to='Curva')|>
    ggplot(aes(x=x, y=value, color=Curva))+
    geom_line(size=1)+
    labs(y    ='f(x)',
         title='cos(x) e aproximações de Taylor',
         color=NULL)+
    scale_x_continuous(breaks=seq(-4, 4, by=1))+
    scale_y_continuous(breaks=seq(-50, 50, by=10))+
    theme_minimal(base_size=13)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=3, r=0, b=0, l=0), 'mm')),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')),
          plot.title=element_text(face='bold'),
          legend.position=c(0.8, 0.775),
          legend.box.background=element_rect(),
          legend.key.width=ggplot2::unit(1, 'cm'),
          legend.box.margin=ggplot2::unit(c(t=-2, r=1, b=0, l=0), 'mm'))
