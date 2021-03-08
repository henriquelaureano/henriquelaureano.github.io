dll <- 'model'
filename <- paste0(dll, '.cpp')
writeLines({'// A LOGISTIC MIXED MODEL (RANDOM INTERCEPT)
#include <TMB.hpp> 
template<class Type>
Type objective_function<Type>::operator() ()
{
  // SPECIFY THE MODEL INPUTS AS DATA_
  DATA_VECTOR(y);
  DATA_SPARSE_MATRIX(Z);
  DATA_SCALAR(n);

  // SPECIFY THE MODEL PARAMETERS AND LATENT EFFECTS AS PARAMETER_
  PARAMETER(beta);
  PARAMETER(logsd);            Type sd = exp(logsd);
  PARAMETER_VECTOR(u); vector<Type> Zu = Z*u;

  // IMPLEMENT THE MODEL
  vector<Type>  risk = exp(beta+Zu);
  vector<Type> level = 1+risk;
  vector<Type>  prob = risk/level;

  // nll: NEGATIVE LOG-LIKELIHOOD
  parallel_accumulator<Type> nll(this); // DO THE MODEL IN PARALLEL
  nll -= dnorm(u, Type(0), sd, true).sum();
  nll -= dbinom(y, n, prob, true).sum();

  // TMB ALLOWS THE USER TO WRITE THE SIMULATION CODE AS AN INTEGRATED 
  // PART OF THE C++ MODEL TEMPLATE
  SIMULATE {
    y = rbinom(Type(100), prob);
    REPORT(y);
  }
  // WE MODEL THE LOG STANDARD DEVIATION (logsd) BUT TMB ALLOWS US TO
  // MAKE DIRECT INFERENCE TO PARAMETER TRANSFORMATIONS, IN THIS CASE
  // THE STANDARD DEVIATION (sd)
  ADREPORT(sd);

  return nll;
}'}, con=filename)

library(TMB) ## install.packages('TMB')
TMB::compile(filename)
dyn.load(TMB::dynlib(dll)) ## loading the C++ model to R

library(Matrix) ## install.packages('Matrix')
beta <- 2
sd   <- 1
cs   <- 3  ## cluster size
nc   <- 50 ## number of cluster
n    <- nc * cs
Z    <- Matrix::bdiag(replicate(nc, rep(1, cs), simplify=FALSE))
u    <- rnorm(nc, mean=0, sd=sd)
u0   <- numeric(nc) ## empty vector (initial guess)
risk <- exp(beta + Z %*% u)
prob <- risk/(1 + risk)
y    <- numeric(n)
## base::rbinom() is not vectorized, do a raw loop is an option
for (i in seq(y)) y[i] <- rbinom(n=1, size=n, prob=prob[i])

## building objective functions with derivatives based on the compiled
## C++ template
obj <- TMB::MakeADFun(data      =list(y=y, Z=Z, n=n),
                      parameters=list(beta=beta, logsd=log(sd), u=u0),
                      DLL       =dll,
                      random    ='u')
set.seed(1)    ## optional
obj$simulate() ## generating a simulation
## obj$simulate(complete=TRUE)
(opt <- nlminb(obj$par, obj$fn, obj$gr)) ## parameters estimation
 sdr <- TMB::sdreport(obj)               ## standard deviations
summary(sdr, select='fixed')             ## extracting model parameters 
summary(sdr, select='report')            ## ... reported variables
cbind(u, summary(sdr, select='random'))  ## ... random effects
