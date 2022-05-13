##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-mai-13 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## ---------------------------------------------------------------------
## Indicadores de malária: Precipitação (Sem NAs) ----------------------
## ---------------------------------------------------------------------

if (!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')

pacman::p_load(readxl, dplyr, tidyr, devtools, geobr, rlang, purrr,
               ggplot2, patchwork)

## devtools::install_github('ipeaGIT/geobr', subdir='r-package')
## library(geobr)

precipitacao <-
    readxl::read_xlsx('valores_ind_simples_malaria_pre_oficina.xlsx',
                      sheet='valores_precipitacao_henrique')

prec.chr <- dplyr::select(precipitacao,
                          c(`code_muni`, `Município`, `UF`, `Biome`))

prec.num <- dplyr::select(precipitacao,
                          !c(`code_muni`, `Município`, `UF`, `Biome`))|>
    dplyr::mutate_if(is.character, as.numeric)

precipitacao <- cbind(prec.chr, prec.num)

## str(precipitacao)

br <- geobr::read_municipality(code_muni='all', year=2018)

## Nível 1 -------------------------------------------------------------

### Uso e ocupação do solo

indicator <- function(data, vars, index=c('median', 'mean', 'pca'))
{
    dat       <- dplyr::select(data,
                               c(code_muni,
                                 dplyr::all_of(!!rlang::enquo(vars))))|>
        tidyr::drop_na()
    code_muni <- dat$code_muni
    dat       <- dplyr::select(dat, !code_muni)
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
    dat <- cbind(code_muni, dat)
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
                             breaks=seq(0, 1, by=0.1))+
        labs(title=title)+
        theme_classic(base_size=14)+
        theme(plot.title=element_text(face='bold'))+
        guides(fill=guide_colorbar(title=NULL,
                                   barheight=19, barwidth=0.65))
}

dat1.vars <- c("Assentamentos",
               "Mineração",
               "Desflorestamento",
               "População Rural",
               "Corpos d'água")

dat1.pca    <- indicator(precipitacao, dat1.vars, index='pca')
## str(dat1.pca)

indicator.map(dat1.pca, title='Uso e ocupação do solo')

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

### Serviços de saúde

dat3.vars <- c("Cobertura da Atenção Básica",
               "Acesso a diagnóstico e tratamento")

dat3.median <- indicator(precipitacao, dat3.vars, index='median')

indicator.map(dat3.median, title='Serviços de saúde')

### Perfil Epidemiológico da Malária

dat4.vars <- c("IPA",
               "Malária falciparum")

dat4.pca <- indicator(precipitacao, dat4.vars, index='pca')

indicator.map(dat4.pca, title='Perfil Epidemiológico da Malária')

### Mobilidade populacional

dat5.vars <- c("Turismo Municipal",
               "Imigração recente")

dat5.median <- indicator(precipitacao, dat5.vars, index='median')

indicator.map(dat5.median, title='Mobilidade Populacional')

### Suscetibilidade social

dat6.vars <- c("IDH-M",
               "Sem acesso ao saneamento adequado")

dat6.median <- indicator(precipitacao, dat6.vars, index='median')

indicator.map(dat6.median, title='Suscetibilidade Social')

## Nível 2 -------------------------------------------------------------

### Índice de Capacidade Adaptativa

indicator.map(dat3.median, title='Índice de Capacidade Adaptativa')

### Índice de sensibilidade

d1 <- dplyr::inner_join(br,
                        tibble::as_tibble(dat4.pca)|>
                        dplyr::select(c(code_muni, index))|>
                        dplyr::rename(ind1=index),
                        by='code_muni')

d2 <- dplyr::inner_join(d1,
                        tibble::as_tibble(dat5.median)|>
                        dplyr::select(c(code_muni, index))|>
                        dplyr::rename(ind2=index),
                        by='code_muni')

dat8 <- dplyr::inner_join(d2,
                          tibble::as_tibble(dat6.median)|>
                          dplyr::select(c(code_muni, index))|>
                          dplyr::rename(ind3=index),
                          by='code_muni')|>
    tibble::as_tibble()|>
    dplyr::select(c(code_muni, ind1:ind3))

dat8.vars <- paste0('ind', 1:3)

dat8.pca <- indicator(dat8, dat8.vars, index='pca')

indicator.map(dat8.pca, title='Índice de sensibilidade')

## Nível 3 -------------------------------------------------------------

### Índice de Exposição

d1 <- dplyr::inner_join(br,
                        precipitacao|>
                        dplyr::select(c(code_muni,
                                        `Presença de rodovias`))|>
                        dplyr::rename(ind1=`Presença de rodovias`),
                        by='code_muni')

dat9 <- dplyr::inner_join(d1,
                          tibble::as_tibble(dat1.pca)|>
                          dplyr::select(c(code_muni, index))|>
                          dplyr::rename(ind2=index),
                          by='code_muni')|>
    tibble::as_tibble()|>
    dplyr::select(c(code_muni, ind1:ind2))

dat9.vars <- paste0('ind', 1:2)

dat9.median <- indicator(dat9, dat9.vars, index='median')

indicator.map(dat9.median, title='Índice de Exposição')

### Índice de Vulnerabilidade

d1 <- dplyr::inner_join(br,
                        tibble::as_tibble(dat3.median)|>
                        dplyr::select(c(code_muni, index))|>
                        dplyr::rename(ind1=index),
                        by='code_muni')

dat10 <- dplyr::inner_join(d1,
                           tibble::as_tibble(dat8.pca)|>
                           dplyr::select(c(code_muni, index))|>
                           dplyr::rename(ind2=index),
                           by='code_muni')|>
    tibble::as_tibble()|>
    dplyr::select(c(code_muni, ind1:ind2))

dat10.vars <- paste0('ind', 1:2)

dat10.median <- indicator(dat10, dat10.vars, index='median')

indicator.map(dat10.median, title='Índice de Vulnerabilidade')

## Nível 4 -------------------------------------------------------------

### Índice de Risco de Impacto para precipitação

d1 <- dplyr::inner_join(br,
                        tibble::as_tibble(dat9.median)|>
                        dplyr::select(c(code_muni, index))|>
                        dplyr::rename(ind1=index),
                        by='code_muni')

d2 <- dplyr::inner_join(d1,
                        tibble::as_tibble(dat10.median)|>
                        dplyr::select(c(code_muni, index))|>
                        dplyr::rename(ind2=index),
                        by='code_muni')

dat11 <-
    dplyr::inner_join(d2,
                      precipitacao|>
                      dplyr::select(c(
                                 code_muni,
                                 `Índice de Ameaça Climática (Presente)`
                             ))|>
                      dplyr::rename(ind3=`Índice de Ameaça Climática (Presente)`
                                    ),
                      by='code_muni')|>
    tibble::as_tibble()|>
    dplyr::select(c(code_muni, ind1:ind3))

dat11.vars <- paste0('ind', 1:3)

dat11.median <- indicator(dat11, dat11.vars, index='median')
dat11.mean   <- indicator(dat11, dat11.vars, index='mean')
dat11.pca    <- indicator(dat11, dat11.vars, index='pca')|>
    dplyr::mutate(index=(1-index))

indicator.map(dat11.mean,
              title='Índice de Risco de Impacto para precipitação')

indicadores <-
    tibble::tibble(
                'Índice de Risco de Impacto para precipitação (média)'=dat11.mean$index, 
                'Índice de Risco de Impacto para precipitação (mediana)'=dat11.median$index, 
                'Índice de Risco de Impacto para precipitação (PCA)'=dat11.pca$index
            )
write.csv(
    indicadores, 'indicadores_precipitacao_semnas.csv', row.names=FALSE
)
