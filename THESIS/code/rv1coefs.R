##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-mar-01 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

v1_cs1_1 <- read.table('coefsv1_cs1_1.txt')[, -1]
v1_cs1_2 <- read.table('coefsv1_cs1_2.txt')[, -1]
v1_cs2_1 <- read.table('coefsv1_cs2_1.txt')[, -1]
v1_cs2_2 <- read.table('coefsv1_cs2_2.txt')[, -1]
v1_cs3_1 <- read.table('coefsv1_cs3_1.txt')[, -1]
v1_cs3_2 <- read.table('coefsv1_cs3_2.txt')[, -1]

coefs1 <- c('beta1'=3.0, 'beta2'=2.6,
            'gama1'=2.5, 'gama2'=4.0,
            'w1'=5, 'w2'=10,
            'logs2_1'=log(1), 'logs2_2'=log(0.6), 'rhoZ12'=atanh(0.1))

coefs2 <- c('beta1'=-2.0, 'beta2'=-1.5,
            'gama1'= 1.0, 'gama2'= 1.5,
            'w1'=3, 'w2'=4,
            'logs2_1'=log(1), 'logs2_2'=log(0.6), 'rhoZ12'=atanh(0.1))

colnames(v1_cs1_1) <- c(names(coefs1), 'conv', 'mll')
colnames(v1_cs1_2) <- c(names(coefs2), 'conv', 'mll')
colnames(v1_cs2_1) <- c(names(coefs1), 'conv', 'mll')
colnames(v1_cs2_2) <- c(names(coefs2), 'conv', 'mll')
colnames(v1_cs3_1) <- c(names(coefs1), 'conv', 'mll')
colnames(v1_cs3_2) <- c(names(coefs2), 'conv', 'mll')

## install.packages('pacman')
pacman::p_load(summarytools)

view(dfSummary(v1_cs1_1))
view(dfSummary(v1_cs1_2))
view(dfSummary(v1_cs2_1))
view(dfSummary(v1_cs2_2))
view(dfSummary(v1_cs3_1))
view(dfSummary(v1_cs3_2))
