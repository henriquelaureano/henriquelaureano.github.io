// A LOGISTIC MIXED MODEL (RANDOM INTERCEPT)
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
}
