pacman::p_load(tidyverse, patchwork, ggcorrplot)

SANN22 <- read.table('SANN22.txt')
SANN36 <- read.table('SANN36.txt')
SANN38 <- read.table('SANN38.txt')
SANN40 <- read.table('SANN40.txt')

## MAH NIGGA, CONVERGENCE 0 TO EVERYBODY
c(table(SANN22$V16),
  table(SANN36$V18),
  table(SANN38$V16),
  table(SANN40$V16))

## round(colMeans(SANN22), 2)
SANN22%>%summarise_all(mean)%>%round(2)%>%select(V2:V15)

pathPORT <- '../'
coefs22 <- read.table(paste0(pathPORT, 'coefs22.txt'))
coefs36 <- read.table(paste0(pathPORT, 'coefs36.txt'))
coefs38 <- read.table(paste0(pathPORT, 'coefs38.txt'))
coefs40 <- read.table(paste0(pathPORT, 'coefs40.txt'))

bind_rows(SANN22%>%select(V2:V15),
          coefs22[1:2, ]%>%select(V2:V15))%>%round(3)

bind_rows(SANN36%>%select(V2:V17),
          coefs36[1:2, ]%>%select(V2:V17))%>%round(3)

bind_rows(SANN38%>%select(V2:V15),
          coefs38[1:2, ]%>%select(V2:V15))%>%round(3)

bind_rows(SANN40%>%select(V2:V15),
          coefs40[1:2, ]%>%select(V2:V15))%>%round(3)

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

tibble(SANN=cerror(SANN22%>%select(V2:V15), true22), 
       PORT=cerror(coefs22%>%select(V2:V15), true22))

tibble(SANN=round(cerror(SANN36%>%select(V2:V17), true36), 5),
       PORT=cerror(coefs36%>%select(V2:V17), true36))
       
tibble(SANN=round(cerror(SANN38%>%select(V2:V15), true38), 5), 
       PORT=cerror(coefs38%>%select(V2:V15), true38))

tibble(SANN=round(cerror(SANN40%>%select(V2:V15), true40), 4), 
       PORT=cerror(coefs40%>%select(V2:V15), true40))
