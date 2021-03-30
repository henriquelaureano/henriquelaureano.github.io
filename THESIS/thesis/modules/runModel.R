## choose the desired MODEL to fit (risk, time, block-diag, complete)
dll <- 'MODEL'

library(TMB)      ## install.packages('TMB')
library(parallel) ## install.packages('parallel')
library(Matrix)   ## install.packages('Matrix')

filename <- paste0(dll, '.cpp')
TMB::compile(filename)
dyn.load(TMB::dynlib(dll))
TMB::config(tape.parallel=FALSE, DLL=dll) ## saves a lot of memory usage
## if you want to make a multi-thread model fitting
TMB::openmp(parallel::detectCores())

J      <- 500 ## choose the number of clusters
cs     <- 2 ## choose the cluster sizes
time   <- runif(n=cs*J, min=30, max=79.9) ## generate the failure times
delta  <- 80
blocks <- replicate(J, rep(1, cs), simplify=FALSE)
Z      <- Matrix::bdiag(blocks) ## build the latent-effect design-matrix

## set the fixed-effect parameters
beta  <- c(beta1=-2, beta2=-1.5)
gamma <- c(gamma1=1.2, gamma2=1)
w     <- c(w1=3, w2=5)
## set the variances and correlations
s2_1  <- 1.0
s2_2  <- 0.6
s2_3  <- 0.7
s2_4  <- 0.9
rho12 <-  0.1
rho13 <- -0.5
rho14 <-  0.3
rho23 <-  0.3
rho24 <- -0.4
rho34 <-  0.2
## auxiliary function to build and check if the Sigma is
## positive-definite (PD)
buildSigma <- function(s2_1, s2_2, s2_3, s2_4,
                       rho12, rho13, rho14, rho23, rho24, rho34)
{
    cov12 <- rho12*sqrt(s2_1)*sqrt(s2_2)
    cov13 <- rho13*sqrt(s2_1)*sqrt(s2_3)
    cov14 <- rho14*sqrt(s2_1)*sqrt(s2_4)
    cov23 <- rho23*sqrt(s2_2)*sqrt(s2_3)
    cov24 <- rho24*sqrt(s2_2)*sqrt(s2_4)
    cov34 <- rho34*sqrt(s2_3)*sqrt(s2_4)
    Sigma <- matrix(c(s2_1, cov12, cov13, cov14,
                      cov12, s2_2, cov23, cov24,
                      cov13, cov23, s2_3, cov34,
                      cov14, cov24, cov34, s2_4), nrow=4)
    ## Sigma will only be returned if PD
    if (
        is.matrix(chol(Sigma))
    )
        return(Sigma)
}
Sigma <- buildSigma(s2_1, s2_2, s2_3, s2_4,
                    rho12, rho13, rho14, rho23, rho24, rho34)

## generate data via the function datasimu() from APPENDIX C, to make it
## simpler, you may save the function in a file and then load in the
## current section
source('datasimu.R')
dat <- datasimu(J=J, cs=cs, time=time,
                Z=Z, S=Sigma, delta=delta,
                beta=beta, gamma=gamma, w=w, seed1=1, seed2=2)
y <- as.matrix( dat%>%dplyr::select(y1:y3) )

## latent-effects matrix U filled with zeros (initial guesses)
## ncol has to be 2 or 4, depending of the chosen MODEL
U <- matrix(0, nrow=J, ncol=4)
## the model fit per se starts now
obj <- TMB::MakeADFun(data=list(Y=y, Z=Z, time=time, delta=delta),
                      parameters=list(beta1  =beta['beta1'],
                                      beta2  =beta['beta2'],
                                      gama1  =gamma['gamma1'],
                                      gama2  =gamma['gamma2'],
                                      w1     =w['w1'],
                                      w2     =w['w2'],
                                      logs2_1=log(s2_1),
                                      logs2_2=log(s2_2),
                                      logs2_3=log(s2_3),
                                      logs2_4=log(s2_4),
                                      rhoZ12 =atanh(rho12),
                                      rhoZ13 =atanh(rho13),
                                      rhoZ14 =atanh(rho14),
                                      rhoZ23 =atanh(rho23),
                                      rhoZ24 =atanh(rho24),
                                      rhoZ34 =atanh(rho34),
                                      U      =U),
                      DLL=dll, random='U', hessian=TRUE, silent=TRUE)
opt <- with(obj, nlminb(par, fn, gr))
