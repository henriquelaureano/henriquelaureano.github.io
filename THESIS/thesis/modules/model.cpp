// A LOGISTIC GLMM WITH RANDOM INTERCEPT
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
    y = rbinom(Type(100), prob);
    REPORT(y);
  }
  ADREPORT(sd);
  return nll;
}
