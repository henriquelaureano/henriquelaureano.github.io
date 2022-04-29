##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-abr-29 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ---------------------------------------------------------------------
## Malária -------------------------------------------------------------
## ---------------------------------------------------------------------

## packages ------------------------------------------------------------

if (!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')

pacman::p_load(readxl,
               dplyr,
               ggplot2,
               mgcv,
               scales,
               patchwork,
               devtools,
               geobr)

## devtools::install_github('ipeaGIT/geobr', subdir='r-package')
## library(geobr)

## data ----------------------------------------------------------------

malaria <- readxl::read_xlsx('malaria_clima_old.xlsx')|>
    dplyr::select(Ano, municipio, ibge_code_7, uf, malaria_100k)|>
    dplyr::rename(ANO=Ano)

ur <- read.csv('HURS_historical.csv')|>
    dplyr::select(!c(X, cv_hist, municipio))|>
    dplyr::rename(UR=media_hist)

sdii <- read.csv('SDII_historical.csv')|>
    dplyr::select(!c(X, cv_hist, municipio))|>
    dplyr::rename(SDII=media_hist)

temp <- read.csv('TASMAX_historical.csv')|>
    dplyr::select(!c(X, cv_hist, municipio))|>
    dplyr::rename(temp=media_hist)

dat <- dplyr::inner_join(malaria, ur, by=c('ibge_code_7', 'ANO'))
dat <- dplyr::inner_join(dat,   sdii, by=c('ibge_code_7', 'ANO'))
dat <- dplyr::inner_join(dat,   temp, by=c('ibge_code_7', 'ANO'))

dat   <- dplyr::rename(dat, Ano=ANO)

bioma <- readxl::read_xlsx('Classe_Bioma_Munic.xlsx')|>
    dplyr::select(codigo_ibge, biome)

bioma <- dplyr::rename(bioma, ibge_code_7=codigo_ibge)

dat1 <- dplyr::inner_join(dat, bioma, by='ibge_code_7')|>
    dplyr::filter(biome %in% 1)

str(dat1)

## Temperatura ---------------------------------------------------------

p1 <- dat1|>
    ggplot(aes(temp, malaria_100k))+
    geom_point(pch=21, size=2)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    geom_smooth(method=mgcv::gam, formula=y ~ s(x, bs='cs'),
                se=TRUE, color='red', size=1)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='Temperatura', y='Malária (100k)',
         caption='Curva/Modelo GAM ajustado')+
    theme_classic(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=2, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x=element_text(face='bold'),
          plot.caption=element_text(color='red'))

p2 <- dat1|>
    ggplot(aes(temp, malaria_100k))+
    geom_bin_2d(binwidth=c(1, 0.25e-5))+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', direction=-1,
                         breaks=seq(10, 250, by=20),
                         labels=paste0(round(100*
                                             seq(10, 250, by=20)/
                                             (nrow(dat1)/5), 2), '%'))+
    scale_y_continuous(labels=scales::comma)+
    labs(x='Temperatura', y='Malária (100k)', 
         caption='Largura da banda: (1, 0.000025)')+
    theme_classic(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=2, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          legend.justification=c(1, 0))+
    guides(fill=guide_colorbar(title='Freq',
                               barheight=0.75, barwidth=40))

p1 / p2

## Umidade Relativa ----------------------------------------------------

p1 <- dat1|>
    ggplot(aes(UR, malaria_100k))+
    geom_point(pch=21, size=2)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    geom_smooth(method=mgcv::gam, formula=y ~ s(x, bs='cs'),
                se=TRUE, color='red', size=1)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='Umidade Relativa', y='Malária (100k)',
         caption='Curva/Modelo GAM ajustado')+
    theme_classic(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=2, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x=element_text(face='bold'),
          plot.caption=element_text(color='red'))

p2 <- dat1|>
    ggplot(aes(UR, malaria_100k))+
    geom_bin_2d(binwidth=c(2.5, 0.25e-5))+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', direction=-1,
                         breaks=seq(5, 75, by=7.5),
                         labels=paste0(round(100*
                                             seq(5, 75, by=7.5)/
                                             (nrow(dat1)/5), 2), '%'))+
    scale_y_continuous(labels=scales::comma)+
    labs(x='Umidade Relativa', y='Malária (100k)',
         caption='Largura da banda: (2.5, 0.000025)')+
    theme_classic(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=2, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          legend.justification=c(1, 0))+
    guides(fill=guide_colorbar(title='Freq',
                               barheight=0.75, barwidth=40))

p1 / p2

## Precipitação --------------------------------------------------------

p1 <- dat1|>
    ggplot(aes(SDII, malaria_100k))+
    geom_point(pch=21, size=2)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    geom_smooth(method=mgcv::gam, formula=y ~ s(x, bs='cs'),
                se=TRUE, color='red', size=1)+
    scale_y_continuous(labels=scales::comma)+
    labs(x='Precipitação', y='Malária (100k)',
         caption='Curva/Modelo GAM ajustado')+
    theme_classic(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=2, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x=element_text(face='bold'),
          plot.caption=element_text(color='red'))

p2 <- dat1|>
    ggplot(aes(SDII, malaria_100k))+
    geom_bin_2d(binwidth=c(0.5, 0.25e-5))+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', direction=-1,
                         breaks=seq(10, 200, by=20),
                         labels=paste0(round(100*
                                             seq(10, 200, by=20)/
                                             (nrow(dat1)/5), 2), '%'))+
    scale_y_continuous(labels=scales::comma)+
    labs(x='Precipitação', y='Malária (100k)',
         caption='Largura da banda: (0.5, 0.000025)')+
    theme_classic(base_size=14)+
    theme(axis.title.x=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=2, r=0, b=-2, l=0), 'mm')
          ),
          axis.title.y=element_text(
              face='bold',
              margin=ggplot2::unit(c(t=0, r=3, b=0, l=0), 'mm')
          ),
          strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          legend.justification=c(1, 0))+
    guides(fill=guide_colorbar(title='Freq',
                               barheight=0.75, barwidth=40))

p1 / p2

## Mapas ---------------------------------------------------------------

br    <- geobr::read_municipality(code_muni='all', year=2018)

dat   <- dplyr::rename(dat,   code_muni=ibge_code_7)
bioma <- dplyr::rename(bioma, code_muni=ibge_code_7)

datm <- dplyr::inner_join(  br,   dat, by='code_muni')
datm <- dplyr::inner_join(datm, bioma, by='code_muni')|>
    dplyr::filter(biome %in% 1)

datm|>
    ggplot(aes(fill=malaria_100k))+
    geom_sf(size=0.15)+
    coord_sf(datum=NA)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', labels=scales::comma,
                         breaks=seq(0, 0.25e-4, by=0.0000025))+
    labs(title='Malária (100k)')+
    theme_classic(base_size=14)+
    theme(strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(fill=guide_colorbar(title=NULL,
                               barheight=0.75, barwidth=45))

datm|>
    ggplot(aes(fill=temp))+
    geom_sf(size=0.15)+
    coord_sf(datum=NA)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', labels=scales::comma,
                         breaks=seq(28, 34, by=0.5))+
    labs(title='Temperatura')+
    theme_classic(base_size=14)+
    theme(strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(fill=guide_colorbar(title=NULL,
                               barheight=0.75, barwidth=45))

datm|>
    ggplot(aes(fill=UR))+
    geom_sf(size=0.15)+
    coord_sf(datum=NA)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', labels=scales::comma,
                         breaks=seq(60, 90, by=2.5))+
    labs(title='Umidade Relativa')+
    theme_classic(base_size=14)+
    theme(strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(fill=guide_colorbar(title=NULL,
                               barheight=0.75, barwidth=45))

datm|>
    ggplot(aes(fill=SDII))+
    geom_sf(size=0.15)+
    coord_sf(datum=NA)+
    facet_wrap(~Ano, nrow=1, labeller=label_both)+
    scale_fill_distiller(palette='Spectral', labels=scales::comma,
                         breaks=seq(2, 10, by=0.5))+
    labs(title='Precipitação')+
    theme_classic(base_size=14)+
    theme(strip.text.x=element_text(face='bold'),
          legend.position='bottom',
          plot.title=element_text(face='bold'))+
    guides(fill=guide_colorbar(title=NULL,
                               barheight=0.75, barwidth=45))
