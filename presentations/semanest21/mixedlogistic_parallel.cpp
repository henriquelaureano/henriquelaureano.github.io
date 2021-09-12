// PARALLEL LOGISTIC MIXED MODEL
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(y); DATA_SPARSE_MATRIX(Z); DATA_SCALAR(n);
  PARAMETER(beta);
  PARAMETER(logsd);            Type sd = exp(logsd);
  PARAMETER_VECTOR(u); vector<Type> Zu = Z*u;
  vector<Type>  risk = exp(beta+Zu);
  vector<Type> level = 1+risk;
  vector<Type>  prob = risk/level;
  parallel_accumulator<Type> nll(this);
  nll -= dnorm(u, Type(0), sd, true).sum();
  nll -= dbinom(y, n, prob, true).sum();
  ADREPORT(sd);
  return nll;
}
