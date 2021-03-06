dll      <- 'model'
filename <- paste0(dll, '.cpp')
writeLines({'// A LOGISTIC GLMM WITH RANDOM INTERCEPT
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_SPARSE_MATRIX(Z);
  PARAMETER(beta);
  PARAMETER(logsd);            Type sd = exp(logsd);
  PARAMETER_VECTOR(u); vector<Type> Zu = Z*u;
  vector<Type>  risk = exp(beta+Zu);
  vector<Type> level = 1+risk;
  vector<Type>  prob = risk/level;
  parallel_accumulator<Type> nll(this);
  nll -=  dnorm(u,   Type(0),   sd, true).sum();
  nll -= dbinom(y, Type(100), prob, true).sum();
  SIMULATE {
    y = rbinom(Type(10), prob);
    REPORT(y);
  }
  ADREPORT(sd);
  return nll;
}'}, con=filename)

library(TMB)    ## install.packages('TMB')
library(Matrix) ## install.packages('Matrix')
TMB::compile(filename)
dyn.load(TMB::dynlib(dll))

TMB::MakeADFun(data=list(y=rbinom(n=10, size=100, prob=),
                         Z=Matrix::bdiag(replicate())),
               parameters=list(), DLL=dll, random='u')
