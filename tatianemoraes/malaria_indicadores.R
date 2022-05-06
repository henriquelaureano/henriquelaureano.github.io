##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-mai-06 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ---------------------------------------------------------------------
## Indicadores de malária ----------------------------------------------
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

## Nível 4 -------------------------------------------------------------

### Índice de Risco de Impacto para precipitação

var.map(precipitacao, `Índice de Intensidade de Precipitação Simples`)

rbind(morans(
    precipitacao.br,
    precipitacao.br$`Índice de Intensidade de Precipitação Simples`
))|>
    kableExtra::kbl(booktabs=TRUE, digits=10)|>
    kableExtra::kable_classic_2(full_width=FALSE)|>
    kableExtra::column_spec(4, bold=TRUE)|>
    kableExtra::row_spec(1, bold=TRUE,
                         color='white', background='#D7261E')
