##----------------------------------------------------------------------
##                                                     Henrique Laureano
##                      leg.ufpr.br/~henrique · github.com/mynameislaure
##                                      laureano@ufpr.br · @hap_laureano
##                     Laboratory of Statistics and Geoinformation (LEG)
##       2020-dez-26 · Federal University of Paraná · Curitiba/PR/Brazil
##----------------------------------------------------------------------

## packages-------------------------------------------------------------
library(TMB)
library(furrr);plan(multicore)
source('dataFunc.R')

## miscellaneous--------------------------------------------------------
model <- 'multiGLM'
compile(paste0(model, '.cpp'))
## J <- 3e4
J <- 15e2
t <- rep(seq(from=30, to=79.5, by=0.5), length.out=2*J)
Z <- Matrix::bdiag(replicate(J, rep(1, 2), simplify=FALSE))
S <- matrix(c(0.4, 0.15, 0.05, 0.2,
              0.15, 0.4, 0.2, 0.05,
              0.05, 0.2, 0.25, 0.1,
              0.2, 0.05, 0.1, 0.25), nrow=4, ncol=4)
## hmm <- 250
hmm <- 4

## simulating data-------------------------------------------------
y <- future_map(rep(J, hmm), ~datasimu(.x, t=t, Z=Z, S=S),
                .options=furrr_options(seed=NULL))

## getting initial guesses----------------------------------------------
initFixedlist <- future_map(y, ~fitGLM(.x, t=t, model=model))
initFixed <- matrix(unlist(initFixedlist), nrow=hmm, ncol=6, byrow=TRUE)

## saving---------------------------------------------------------------
save(y, initFixed, file='data36.RData', version=2)
## END------------------------------------------------------------------
