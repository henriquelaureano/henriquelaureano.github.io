// A LOGISTIC GLMM WITH RANDOM INTERCEPT
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y);
  DATA_SPARSE_MATRIX(Z);
  PARAMETER(beta);
  PARAMETER(logsd);            Type sd=exp(logsd);
  PARAMETER_VECTOR(u); vector<Type> Zu=Z*u;
  parallel_accumulator<Type> nll(this);
  nll -= dnorm(u, Type(0), sd, true).sum();
  vector<Type> pred=beta+Zu;
  nll -= dbinom_robust(y, Type(100), pred, true).sum();
  // SIMULATE {
  // }
  ADREPORT(sd);
  return nll;
}
