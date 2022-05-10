##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-mai-10 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ---------------------------------------------------------------------
## Indicadores de malária: Precipitação --------------------------------
## ---------------------------------------------------------------------

if (!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')

pacman::p_load(readxl, dplyr, tidyr, devtools, geobr, rlang, purrr,
               ggplot2, patchwork, spdep)

## devtools::install_github('ipeaGIT/geobr', subdir='r-package')
## library(geobr)

precipitacao <-
    readxl::read_xlsx('valores_ind_simples_malaria_pre_oficina.xlsx',
                      sheet='valores_precipitacao_henrique')

prec.chr <- dplyr::select(precipitacao,
                          c(`code_muni`, `Município`, `UF`, `Biome`))

prec.num <- dplyr::select(precipitacao,
                          !c(`code_muni`, `Município`, `UF`, `Biome`))|>
    dplyr::mutate_if(is.character, as.numeric)|>
    dplyr::mutate(
               `Sem acesso ao saneamento adequado`=
                   tidyr::replace_na(
                              `Sem acesso ao saneamento adequado`, 0), 
               `IDH-M`=tidyr::replace_na(`IDH-M`, 0),
               `Turismo Municipal`=
                   tidyr::replace_na(`Turismo Municipal`, 0),
               `Imigração recente`=
                   tidyr::replace_na(`Imigração recente`, 0),
               `Cobertura da Atenção Básica`=
                   tidyr::replace_na(`Cobertura da Atenção Básica`, 0),
               `Acesso a diagnóstico e tratamento`=
                   tidyr::replace_na(
                              `Acesso a diagnóstico e tratamento`, 0),
               `Desflorestamento`=
                   tidyr::replace_na(`Desflorestamento`, 0),
               `População Rural`=tidyr::replace_na(`População Rural`, 0)
           )

precipitacao <- cbind(prec.chr, prec.num)

## str(precipitacao)

br <- geobr::read_municipality(code_muni='all', year=2018)

## Nível 1 -------------------------------------------------------------

### Uso e ocupação do solo

indicator <- function(data, vars, index=c('median', 'mean', 'pca'))
{
    dat       <- dplyr::select(data,
                               dplyr::all_of(!!rlang::enquo(vars)))
    n         <- nrow(dat)
    dat$index <- switch(
        index,
        'median'=purrr::map_dbl(seq(n), 
                                function(.x)
                                    median(as.numeric(dat[.x, ]))),
        'mean'  =purrr::map_dbl(seq(n),
                                function(.x)
                                    mean  (as.numeric(dat[.x, ]))),
        'pca'   =prcomp(dat)$x[, 1]
    )
    if (index == 'pca')
    {
        index.min <- min(dat$index)
        dat$index <- (dat$index-index.min)/(max(dat$index)-index.min)
    }
    dat <- cbind(prec.chr, dat)
    dat <- dplyr::inner_join(br, dat, by='code_muni')
    return(dat)
}
indicator.map <- function(data, title='Título')
{
    data|>
        ggplot(aes(fill=index))+
        geom_sf(size=0.025)+
        coord_sf(datum=NA)+
        scale_fill_distiller(palette='Spectral',
                             limits=c(0, 1), 
                             breaks=seq(0, 1, by=0.2))+
        labs(title=title)+
        theme_classic(base_size=14)+
        ## theme(plot.title=element_text(face='bold'))+
        guides(fill=guide_colorbar(title=NULL,
                                   barheight=11, barwidth=0.65))
}

dat1.vars <- c("Assentamentos",
               "Mineração",
               "Desflorestamento",
               "População Rural",
               "Corpos d'água")

dat1.median <- indicator(precipitacao, dat1.vars, index='median')
dat1.mean   <- indicator(precipitacao, dat1.vars, index='mean')
dat1.pca    <- indicator(precipitacao, dat1.vars, index='pca')
## str(dat1.median)

p1 <- indicator.map(dat1.median, title='Mediana')
p2 <- indicator.map(dat1.mean  , title='Média')
p3 <- indicator.map(dat1.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Uso e ocupação do solo',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

get.ZeroPolicyOption()
set.ZeroPolicyOption(TRUE)
get.ZeroPolicyOption()

morans <- function(data, var)
{
    neigh  <- spdep::poly2nb(data)
    morans <- spdep::moran.test(var,
                                spdep::nb2listw(neigh),
                                alternative='two.sided')
    c(morans$estimate, 'p-value'=morans$p.value)
}
dplyr::bind_rows(morans(dat1.median, dat1.median$index), 
                 morans(dat1.mean  , dat1.mean$index), 
                 morans(dat1.pca   , dat1.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')

finalindicator.map <- function(data, title='Título')
{
    data|>
        ggplot(aes(fill=index))+
        geom_sf(size=0.025)+
        coord_sf(datum=NA)+
        scale_fill_distiller(palette='Spectral',
                             limits=c(0, 1), 
                             breaks=seq(0, 1, by=0.1))+
        labs(title=title)+
        theme_classic(base_size=14)+
        theme(plot.title=element_text(face='bold'))+
        guides(fill=guide_colorbar(title=NULL,
                                   barheight=19, barwidth=0.65))
}
finalindicator.map(dat1.pca, title='Uso e ocupação do solo')

### Malha rodoviária

var.map <- function(data, var)
{
    dat <- dplyr::inner_join(br, data, by='code_muni')
    dat|>
        ggplot(aes(fill=!!rlang::ensym(var)))+
        geom_sf(size=0.025)+
        coord_sf(datum=NA)+
        scale_fill_distiller(palette='Spectral',
                             limits=c(0, 1), 
                             breaks=seq(0, 1, by=0.1))+
        labs(title=deparse(substitute(var)))+
        theme_classic(base_size=14)+
        theme(plot.title=element_text(face='bold'))+
        guides(fill=guide_colorbar(title=NULL,
                                   barheight=19, barwidth=0.65))
}
var.map(precipitacao, `Presença de rodovias`)

precipitacao.br <- dplyr::inner_join(br, precipitacao, by='code_muni')

rbind(morans(precipitacao.br, precipitacao.br$`Presença de rodovias`))|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(4, bold=TRUE)|>
    kableExtra::row_spec(1, bold=TRUE,
                         color='white', background='#D7261E')

### Serviços de saúde

dat3.vars <- c("Cobertura da Atenção Básica",
               "Acesso a diagnóstico e tratamento")

dat3.median <- indicator(precipitacao, dat3.vars, index='median')
dat3.mean   <- indicator(precipitacao, dat3.vars, index='mean')
dat3.pca    <- indicator(precipitacao, dat3.vars, index='pca')

p1 <- indicator.map(dat3.median, title='Mediana')
p2 <- indicator.map(dat3.mean  , title='Média')
p3 <- indicator.map(dat3.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Serviços de saúde',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat3.median, dat3.median$index), 
                 morans(dat3.mean  , dat3.mean$index), 
                 morans(dat3.pca   , dat3.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat3.median, title='Serviços de saúde')

### Perfil Epidemiológico da Malária

dat4.vars <- c("IPA",
               "Malária falciparum")

dat4.median <- indicator(precipitacao, dat4.vars, index='median')
dat4.mean   <- indicator(precipitacao, dat4.vars, index='mean')
dat4.pca    <- indicator(precipitacao, dat4.vars, index='pca')

p1 <- indicator.map(dat4.median, title='Mediana')
p2 <- indicator.map(dat4.mean  , title='Média')
p3 <- indicator.map(dat4.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Perfil Epidemiológico da Malária',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat4.median, dat4.median$index), 
                 morans(dat4.mean  , dat4.mean$index), 
                 morans(dat4.pca   , dat4.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat4.pca,
                   title='Perfil Epidemiológico da Malária')

### Mobilidade populacional

dat5.vars <- c("Turismo Municipal",
               "Imigração recente")

dat5.median <- indicator(precipitacao, dat5.vars, index='median')
dat5.mean   <- indicator(precipitacao, dat5.vars, index='mean')
dat5.pca    <- indicator(precipitacao, dat5.vars, index='pca')

p1 <- indicator.map(dat5.median, title='Mediana')
p2 <- indicator.map(dat5.mean  , title='Média')
p3 <- indicator.map(dat5.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Perfil Epidemiológico da Malária',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat5.median, dat5.median$index), 
                 morans(dat5.mean  , dat5.mean$index), 
                 morans(dat5.pca   , dat5.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat5.median,
                   title='Mobilidade Populacional')

### Suscetibilidade social

dat6.vars <- c("IDH-M",
               "Sem acesso ao saneamento adequado")

dat6.median <- indicator(precipitacao, dat6.vars, index='median')
dat6.mean   <- indicator(precipitacao, dat6.vars, index='mean')
dat6.pca    <- indicator(precipitacao, dat6.vars, index='pca')

p1 <- indicator.map(dat6.median, title='Mediana')
p2 <- indicator.map(dat6.mean  , title='Média')
p3 <- indicator.map(dat6.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Perfil Epidemiológico da Malária',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat6.median, dat6.median$index), 
                 morans(dat6.mean  , dat6.mean$index), 
                 morans(dat6.pca   , dat6.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat6.median, title='Suscetibilidade Social')

## Nível 2 -------------------------------------------------------------

### Índice de Capacidade Adaptativa

finalindicator.map(dat3.median,
                   title='Índice de Capacidade Adaptativa')

### Índice de sensibilidade

dat8 <- tibble::tibble(ind1=dat4.pca   $index, 
                       ind2=dat5.median$index, 
                       ind3=dat6.median$index)

dat8.vars <- paste0('ind', 1:3)

dat8.median <- indicator(dat8, dat8.vars, index='median')
dat8.mean   <- indicator(dat8, dat8.vars, index='mean')
dat8.pca    <- indicator(dat8, dat8.vars, index='pca')

p1 <- indicator.map(dat8.median, title='Mediana')
p2 <- indicator.map(dat8.mean  , title='Média')
p3 <- indicator.map(dat8.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Índice de sensibilidade',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat8.median, dat8.median$index), 
                 morans(dat8.mean  , dat8.mean$index), 
                 morans(dat8.pca   , dat8.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat8.pca, title='Índice de sensibilidade')

## Nível 3 -------------------------------------------------------------

### Índice de Exposição

dat9 <- tibble::tibble(ind1=precipitacao$`Presença de rodovias`, 
                       ind2=dat1.pca$index)

dat9.vars <- paste0('ind', 1:2)

dat9.median <- indicator(dat9, dat9.vars, index='median')
dat9.mean   <- indicator(dat9, dat9.vars, index='mean')
dat9.pca    <- indicator(dat9, dat9.vars, index='pca')

p1 <- indicator.map(dat9.median, title='Mediana')
p2 <- indicator.map(dat9.mean  , title='Média')
p3 <- indicator.map(dat9.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Índice de Exposição',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat9.median, dat9.median$index), 
                 morans(dat9.mean  , dat9.mean$index), 
                 morans(dat9.pca   , dat9.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat9.median, title='Índice de Exposição')

### Índice de Vulnerabilidade

dat10 <- tibble::tibble(ind1=dat3.median$index, 
                        ind2=dat8.pca$index)

dat10.vars <- paste0('ind', 1:2)

dat10.median <- indicator(dat10, dat10.vars, index='median')
dat10.mean   <- indicator(dat10, dat10.vars, index='mean')
dat10.pca    <- indicator(dat10, dat10.vars, index='pca')

p1 <- indicator.map(dat10.median, title='Mediana')
p2 <- indicator.map(dat10.mean  , title='Média')
p3 <- indicator.map(dat10.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(title='Índice de Vulnerabilidade',
                               subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat10.median, dat10.median$index), 
                 morans(dat10.mean  , dat10.mean$index), 
                 morans(dat10.pca   , dat10.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(dat10.median, title='Índice de Vulnerabilidade')

## Nível 4 -------------------------------------------------------------

### Índice de Risco de Impacto para precipitação

dat11 <- tibble::tibble(ind1=dat9.median$index, 
                        ind2=dat10.median$index,
                        ind3=precipitacao$
                            `Índice de Ameaça Climática (Presente)`)

dat11.vars <- paste0('ind', 1:3)

dat11.median <- indicator(dat11, dat11.vars, index='median')
dat11.mean   <- indicator(dat11, dat11.vars, index='mean')
dat11.pca    <- indicator(dat11, dat11.vars, index='pca')

p1 <- indicator.map(dat11.median, title='Mediana')
p2 <- indicator.map(dat11.mean  , title='Média')
p3 <- indicator.map(dat11.pca   , title='PCA')

p1 + p2 + p3 +
    patchwork::plot_layout(guides='collect')&
    patchwork::plot_annotation(
                   title=
                       'Índice de Risco de Impacto para precipitação',
                   subtitle='Agregação por')&
    theme(plot.title   =element_text(face='bold', size=14),
          plot.subtitle=element_text(size=14))

dplyr::bind_rows(morans(dat11.median, dat11.median$index), 
                 morans(dat11.mean  , dat11.mean$index), 
                 morans(dat11.pca   , dat11.pca$index))|>
    dplyr::mutate(var=c('Mediana', 'Média', 'PCA'))|>
    dplyr::relocate(var, .before='Moran I statistic')|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(5, bold=TRUE)|>
    kableExtra::row_spec(1:3, bold=TRUE,
                         color='white', background='#D7261E')

finalindicator.map(
    dat11.mean,
    title='Índice de Risco de Impacto para precipitação'
)

indicadores <-
    tibble::tibble(
                'Uso e ocupação do solo'=dat1.pca$index,
                'Malha rodoviária'=precipitacao$`Presença de rodovias`,
                'Serviços de saúde'=dat3.median$index,
                'Perfil Epidemiológico da Malária'=dat4.pca$index,
                'Mobilidade Populacional'=dat5.median$index,
                'Suscetibilidade Social'=dat6.median$index,
                'Índice de Capacidade Adaptativa'=dat3.median$index,
                'Índice de sensibilidade'=dat8.pca$index,
                'Índice de Exposição'=dat9.median$index,
                'Índice de Vulnerabilidade'=dat10.median$index,
                'Índice de Risco de Impacto para precipitação (média)'=dat11.mean$index, 
                'Índice de Risco de Impacto para precipitação (mediana)'=dat11.median$index, 
                'Índice de Risco de Impacto para precipitação (PCA)'=dat11.pca$index
            )
write.csv(indicadores, 'indicadores_precipitacao.csv', row.names=FALSE)
