##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2022-jan-11 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## "Fatores associados ao sentimento de prejuízo no processo de
## aprendizagem"

if (!requireNamespace('pacman', quietly=TRUE)) install.packages('pacman')

pacman::p_load(readxl, dplyr, forcats, DHARMa, MASS, car)

dat <- readxl::read_xlsx('baseR.xlsx')|>
    dplyr::filter(Prejuízo != 'Não sei responder')|>
    dplyr::mutate(
               Prejuízo=dplyr::recode(Prejuízo, 'Não'=0, 'Sim'=1),
               Concentração=
                   dplyr::recode(
                              Concentração,
                              'Alta  (consigo  me concentrar)'='Alta',
                              'Baixa (distraio-me com  facilidade)'='Baixa',
                              'Parcial  (consigo me concentrar por um período)'='Parcial'
                          ),
               Aulas=
                   dplyr::recode(
                              Aulas,
                              'Prefiro as disciplinas cujas atividades são 100% assíncronas.'=
                                  '100% assíncronas', 
                              'Prefiro as disciplinas cujas atividades são 100% síncronas.'=
                                  '100% síncronas', 
                              'Prefiro as disciplinas cujas atividades são, em maior parte, assíncronas.'=
                                  'Em maior parte, assíncronas',
                              'Prefiro as disciplinas cujas atividades são, em maior parte, síncronas.'=
                                  'Em maior parte, síncronas'
                          ),
               Atendimento_professor=
                   dplyr::recode(Atendimento_professor,
                                 'Busquei atendimento e não obtive resposta.'='Insatisfatória',
                                 'Busquei atendimento e obtive resposta insatisfatória.'='Insatisfatória',
                                 'Busquei atendimento e obtive resposta satisfatória.'='Satisfatória',
                                 'Não tive necessidade de buscar atendimento.'='Sem necessidade'),
               UF=forcats::fct_collapse(UF,
                                        'Sul'=c('Paraná (PR)',
                                                'Santa Catarina (SC)',
                                                'Rio Grande do Sul (RS)'),
                                        'Sudeste'=c('Minas Gerais (MG)',
                                                    'São Paulo (SP)'),
                                        'Norte'=c('Amapá (AP)'),
                                        'Nordeste'=c('Rio Grande do Norte (RN)'),
                                        'Centro-Oeste'=c('Distrito Federal (DF)',
                                                         'Mato Grosso do Sul (MS)')
                                        ),
               Concentração=factor(Concentração,
                                   levels=c('Baixa', 'Parcial', 'Alta')),
               Aulas=factor(Aulas,
                            levels=c('100% assíncronas',
                                     'Em maior parte, assíncronas',
                                     'Em maior parte, síncronas', 
                                     '100% síncronas')),
               Interação_remota_professor=factor(Interação_remota_professor,
                                                 levels=c('Pouco',
                                                          'Parcialmente', 
                                                          'Muito'))
           )
names(dat)

m0 <- glm(Prejuízo ~ ., family=binomial(link='logit'), dat)

res.m0 <- DHARMa::simulateResiduals(m0);plot(res.m0)

m <- MASS::stepAIC(m0, direction='both', trace=FALSE)

res <- DHARMa::simulateResiduals(m);plot(res)

car::Anova(m)

invlogit <- function(x) exp(x)/(1 + exp(x))

Prob <- c(invlogit(coef(m)[1])[[1]], 
          invlogit(coef(m)[1] + coef(m)[2])[[1]], 
          invlogit(coef(m)[1] + coef(m)[3])[[1]], 
          invlogit(coef(m)[1] + coef(m)[4])[[1]], 
          invlogit(coef(m)[1] + coef(m)[5])[[1]], 
          invlogit(coef(m)[1] + coef(m)[6])[[1]], 
          invlogit(coef(m)[1] + coef(m)[7])[[1]], 
          invlogit(coef(m)[1] + coef(m)[8])[[1]], 
          invlogit(coef(m)[1] + coef(m)[9])[[1]], 
          invlogit(coef(m)[1] + coef(m)[10])[[1]], 
          invlogit(coef(m)[1] + coef(m)[11])[[1]])

dplyr::bind_cols(
           Variavel=names(coef(m)), coef(summary(m)), Prob=Prob,
           Chance=exp(coef(m)), exp(confint(m))
       )
