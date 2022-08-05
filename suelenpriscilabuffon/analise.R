## -----------------------------------------------------------------------------
if(!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')

pacman::p_load(quarto,
               readxl,
               dplyr,
               tidyr,
               rlang,
               purrr,
               kableExtra,
               psych,
               MASS,
               betareg,
               lmtest,
               equatiomatic,
               marginaleffects,
               broom)



## -----------------------------------------------------------------------------
dat <- readxl::read_xlsx('dados renda+educação.xlsx')|>
    dplyr::select(!c(Brasil,
                     `Nome da Região`,
                     `Nome da Região Metropolitana`,
                     `Nome do Município`,
                     `Nome da UDH`))
newnames <- c('UF',
              'Ano',
              'RDpCmeioSM.perc', 
              'deso.taxa', 
              'esc.ind', 
              'FC18.perc', 
              'FE.ind', 
              'esc5a6anos.perc',
              'F11a13.perc',
              'FC15a17.perc',
              'MC18a20.perc')

names(dat) <- newnames

dat <- dat|>
    dplyr::mutate(dplyr::across(!c(UF, Ano), dplyr::na_if, '-'))|>
    dplyr::mutate(dplyr::across(!c(UF, Ano), as.numeric))|>
    dplyr::mutate(dplyr::across(!c(UF, Ano),
                                ~ifelse(pmax(.) > 1, ./100, .)))|>
    dplyr::mutate(UF=dplyr::recode(UF,
                                   'Paraná'           ='PR',
                                   'Santa Catarina'   ='SC',
                                   'Rio Grande do Sul'='RS'))|>
    tidyr::drop_na()



## -----------------------------------------------------------------------------
dm <- function(var, type=c('Overall', 'byUF', 'byAno'))
{
    switch(
        type,
        'Overall'={
            out <- dat|>
                dplyr::summarise(
                           Variable       =var,
                           Mínimo         =min(     !!rlang::sym(var)),
                           '1o Quartil'   =quantile(!!rlang::sym(var),
                                                    probs=0.25),
                           Mediana        =median(  !!rlang::sym(var)),
                           Média          =mean(    !!rlang::sym(var)),
                           'Desvio Padrão'=sd(      !!rlang::sym(var)),
                           '3o Quartil'   =quantile(!!rlang::sym(var),
                                                    probs=0.75),
                           Maximo         =max(     !!rlang::sym(var)),
                           n              =length(  !!rlang::sym(var)))
        },
        'byUF'={
            out <- dat|>
                dplyr::group_by(UF)|>
                dplyr::summarise(
                           Variable       =var,
                           Mínimo         =min(     !!rlang::sym(var)),
                           '1o Quartil'   =quantile(!!rlang::sym(var),
                                                    probs=0.25),
                           Mediana        =median(  !!rlang::sym(var)),
                           Média          =mean(    !!rlang::sym(var)),
                           'Desvio Padrão'=sd(      !!rlang::sym(var)),
                           '3o Quartil'   =quantile(!!rlang::sym(var),
                                                    probs=0.75),
                           Maximo         =max(     !!rlang::sym(var)),
                           n              =length(  !!rlang::sym(var))
                       )|>
                dplyr::relocate(UF, .after=Variable)
        },
        'byAno'={
            out <- dat|>
                dplyr::group_by(Ano)|>
                dplyr::summarise(
                           Variable       =var,
                           Mínimo         =min(     !!rlang::sym(var)),
                           '1o Quartil'   =quantile(!!rlang::sym(var),
                                                    probs=0.25),
                           Mediana        =median(  !!rlang::sym(var)),
                           Média          =mean(    !!rlang::sym(var)),
                           'Desvio Padrão'=sd(      !!rlang::sym(var)),
                           '3o Quartil'   =quantile(!!rlang::sym(var),
                                                    probs=0.75),
                           Maximo         =max(     !!rlang::sym(var)),
                           n              =length(  !!rlang::sym(var))
                       )|>
                dplyr::relocate(Ano, .after=Variable)
        })
    return(out)
}



## -----------------------------------------------------------------------------
purrr::map_df(3:11, function(i) dm(newnames[i], type='Overall'))|>
    kableExtra::kbl(booktabs=TRUE, digits=3)|>
    kableExtra::kable_classic_2()



## -----------------------------------------------------------------------------
purrr::map_df(3:11, function(i) dm(newnames[i], type='byUF'))|>
    kableExtra::kbl(booktabs=TRUE, digits=3)|>
    kableExtra::kable_classic_2()



## -----------------------------------------------------------------------------
purrr::map_df(3:11, function(i) dm(newnames[i], type='byAno'))|>
    kableExtra::kbl(booktabs=TRUE, digits=3)|>
    kableExtra::kable_classic_2()



## -----------------------------------------------------------------------------
dplyr::bind_cols(' '=c('n', 'Percentual'), 
                 rbind(table(dat$UF), prop.table(table(dat$UF))))|>
    kableExtra::kbl(booktabs=TRUE, digits=3)|>
    kableExtra::kable_classic_2(full_width=FALSE)



## -----------------------------------------------------------------------------
dplyr::bind_cols(' '=c('n', 'Percentual'), 
                 rbind(table(dat$Ano), prop.table(table(dat$Ano))))|>
    kableExtra::kbl(booktabs=TRUE, digits=3)|>
    kableExtra::kable_classic_2(full_width=FALSE)



## -----------------------------------------------------------------------------
psych::pairs.panels(dat,
                    stars   =TRUE,
                    lm      =TRUE,
                    smoother=TRUE,
                    ci      =TRUE,
                    hist.col='gray80',
                    density =FALSE,
                    ellipses=FALSE)



## -----------------------------------------------------------------------------
mod <- lm(esc.ind~.-FC18.perc, dat);summary(mod)



## -----------------------------------------------------------------------------
mod <- MASS::stepAIC(mod, trace=FALSE);summary(mod)

par(mfrow=c(2, 2));plot(mod);layout(1)



## -----------------------------------------------------------------------------
as.data.frame(dat[c(2201, 2203, 2585), ])



## -----------------------------------------------------------------------------
dat2 <- dat[-c(2201, 2203, 2585), ]

mod <- update(mod, data=dat2)

mod <- MASS::stepAIC(mod, trace=FALSE);summary(mod)



## -----------------------------------------------------------------------------
par(mfrow=c(2, 2));plot(mod);layout(1)



## -----------------------------------------------------------------------------
as.data.frame(dat2[c(1551, 2322, 2583), ])



## -----------------------------------------------------------------------------
dat3 <- dat2[-c(1551, 2322, 2583), ]

mod <- update(mod, data=dat3)

mod <- MASS::stepAIC(mod, trace=FALSE);summary(mod)



## -----------------------------------------------------------------------------
par(mfrow=c(2, 2));plot(mod);layout(1)



## -----------------------------------------------------------------------------
dat.pca <- prcomp(dat3|>
                  dplyr::select(!c(UF,
                                   Ano,
                                   RDpCmeioSM.perc,
                                   deso.taxa,
                                   esc.ind,
                                   FC18.perc)))
summary(dat.pca)$importance[-1, ]



## -----------------------------------------------------------------------------
dat3 <- dat3|>
    dplyr::mutate(PC1=dat.pca$x[, 'PC1'],
                  PC1=(PC1-min(PC1))/(max(PC1)-min(PC1)))

summary(dat3$PC1)



## -----------------------------------------------------------------------------
f.pca <- esc.ind ~ UF + Ano + RDpCmeioSM.perc + deso.taxa + PC1

mod.pca <- lm(f.pca, dat3)

mod.pca <- MASS::stepAIC(mod.pca, trace=FALSE);summary(mod.pca)



## -----------------------------------------------------------------------------
par(mfrow=c(2, 2));plot(mod.pca);layout(1)



## -----------------------------------------------------------------------------
mod.bet <- betareg::betareg(f.pca, dat3)

par(mfrow=c(2, 2));plot(mod.bet);layout(1)



## -----------------------------------------------------------------------------
summary(mod.bet)



## -----------------------------------------------------------------------------

data.frame(Modelo=c('Regressão Linear',
                    'Regressão Linear (PCA)',
                    'Regressão Beta'),
           logLik=c(logLik(mod), logLik(mod.pca), logLik(mod.bet)),
           df    =c(12, 8, 8))



## -----------------------------------------------------------------------------

lmtest::coeftest(mod.pca)



## -----------------------------------------------------------------------------

equatiomatic::extract_eq(mod.pca,
                         wrap             =TRUE,
                         terms_per_line   =4,
                         operator_location='start',
                         use_coefs        =TRUE,
                         show_distribution=TRUE)



## -----------------------------------------------------------------------------
summary(marginaleffects::marginalmeans(mod.pca))



## -----------------------------------------------------------------------------
broom::tidy(marginaleffects::comparisons(mod.pca,
                                         variables=list(Ano='all')))



## -----------------------------------------------------------------------------
broom::tidy(marginaleffects::comparisons(mod.pca,
                                         variables=list(UF='all')))



## -----------------------------------------------------------------------------

lmtest::coeftest(mod.bet)



## -----------------------------------------------------------------------------

tt <- coef(summary(mod.bet))$mean

out <- cbind(exp(cbind(coef(mod.bet)[-8], confint(mod.bet)[-8, ])),
             tt[, 4])

colnames(out) <- c('Odds ratio', 'OR 2.5% CI', 'OR 97.5% CI', 'P-value')

out|>
    kableExtra::kbl(booktabs=TRUE, digits=3)|>
    kableExtra::kable_classic_2(full_width=FALSE)



## -----------------------------------------------------------------------------
p <- 0.1

q <- p / (1 - p) * out[-1, 'Odds ratio']

cbind(newp <- q / (1 + q))



## -----------------------------------------------------------------------------
p <- 0.5

q <- p / (1 - p) * out[-1, 'Odds ratio']

cbind(newp <- q / (1 + q))


