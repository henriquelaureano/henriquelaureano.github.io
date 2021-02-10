##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                                            henriquelaureano.github.io
##                                      2021-fev-02 · Curitiba/PR/Brazil
##----------------------------------------------------------------------

pacman::p_load(tidyverse, patchwork, ggcorrplot)

coefs22 <- read.table('coefs22.txt')
coefs36 <- read.table('coefs36.txt')
coefs38 <- read.table('coefs38.txt')
coefs40 <- read.table('coefs40.txt')

c(table(coefs22$V16),
  table(coefs36$V18),
  table(coefs38$V16),
  table(coefs40$V16))

measures <- function(data) {
    data%>%summarise_all(mean)%>%
        add_row(data%>%summarize_all(quantile, c(0.025, 0.975)))%>%
        mutate(label=c('mean', 'q025', 'q975'))%>%
        select(-V1)
}
measures(coefs22)%>%select(V8:V15, label)
measures(coefs36)%>%select(V8:V17, label)
measures(coefs38)%>%select(V8:V15, label)
measures(coefs40)%>%select(V8:V15, label)

pathPORT <- '../'
old22 <- read.table(paste0(pathPORT, 'coefs22.txt'))
old36 <- read.table(paste0(pathPORT, 'coefs36.txt'))
old38 <- read.table(paste0(pathPORT, 'coefs38.txt'))
old40 <- read.table(paste0(pathPORT, 'coefs40.txt'))

bind_rows(coefs22%>%select(V2:V15),
          old22[1:5, ]%>%select(V2:V15))

bind_rows(coefs36%>%select(V2:V17),
          old36[1:5, ]%>%select(V2:V17))

bind_rows(coefs38%>%select(V2:V15),
          old38[1:5, ]%>%select(V2:V15))

bind_rows(coefs40%>%select(V2:V15),
          old40[1:5, ]%>%select(V2:V15))

## computing bias
true22 <- c(-2, -1.5, 1.2, 1, 3, 5,
            log(0.4), log(0.4), log(0.25), log(0.25),
            atanh(0.15/sqrt(0.4*0.4)),
            atanh(0.1/sqrt(0.25*0.25)),
            atanh(0.05/sqrt(0.4*0.25)),
            atanh(0.05/sqrt(0.4*0.25)))
true36 <- c(-2, -1.5, 1.2, 1, 3, 5,
            log(0.4), log(0.4), log(0.25), log(0.25),
            atanh(0.15/sqrt(0.4*0.4)),
            atanh(0.1/sqrt(0.25*0.25)),
            atanh(0.05/sqrt(0.4*0.25)),
            atanh(0.05/sqrt(0.4*0.25)), 
            atanh(0.2/sqrt(0.4*0.25)),
            atanh(0.2/sqrt(0.4*0.25)))
true38 <- c(-2, -1.5, 1.2, 1, 3, 5,
            log(0.4), log(0.4), log(0.25), log(0.25),
            atanh(0.15/sqrt(0.4*0.4)),
            atanh(0.1/sqrt(0.25*0.25)),
            atanh(0.1/sqrt(0.4*0.25)),
            atanh(0.1/sqrt(0.4*0.25)))
true40 <- c(-2, -1.5, 1.2, 1, 3, 5,
            log(0.4), log(0.4), log(0.25), log(0.25),
            atanh(0.05/sqrt(0.4*0.25)),
            atanh(0.05/sqrt(0.4*0.25)),
            atanh(0.2/sqrt(0.4*0.25)),
            atanh(0.2/sqrt(0.4*0.25)))

cerror <- function(coefs, true) {
    error <- coefs
    for (i in seq(ncol(coefs))) { error[i] <- true[i]-coefs[i] }
    out <- error%>%
        summarize_all(mean)%>%
        mutate(label='mean')%>%
        pivot_longer(!label, names_to='par', values_to='value')%>%
        pivot_wider(names_from=label, values_from=value)
    out$par <- as_factor(out$par)
    return(out%>%select(mean))
}

tibble(
    coefs=cerror(coefs22%>%select(V2:V15), true22), 
    old=cerror(old22[1:5, ]%>%select(V2:V15), true22)
)
tibble(
    coefs=cerror(coefs36%>%select(V2:V17), true36),
    old=cerror(old36[1:5, ]%>%select(V2:V17), true36)
)       
tibble(
    coefs=cerror(coefs38%>%select(V2:V15), true38),
    old=cerror(old38[1:5, ]%>%select(V2:V15), true38)
)
tibble(
    coefs=cerror(coefs40%>%select(V2:V15), true40),
    old=cerror(old40[1:5, ]%>%select(V2:V15), true40)
)
