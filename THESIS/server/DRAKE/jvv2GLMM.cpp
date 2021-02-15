//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2021-fev-15 · Curitiba/PR/Brazil
//----------------------------------------------------------------------

// A STANDARD MULTINOMIAL GLMM WITH A RANDOM COMMON INTERCEPT

#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;

  DATA_MATRIX(Y);
  DATA_SPARSE_MATRIX(Z); // Matrix::bdiag()

  PARAMETER(beta1);
  PARAMETER(beta2);
  
  PARAMETER(logs2); Type s2=exp(logs2);

  PARAMETER_MATRIX(U);
  
  matrix<Type> LE=Z*U;

  Type risk1=0;
  Type risk2=0; Type level=0;

  vector<Type> y(Y.cols());
  vector<Type> prob(Y.cols());
  
  Type cov12=0 * sqrt(s2)*sqrt(s2);
  
  matrix<Type> Sigma(2, 2);
  Sigma.row(0) << s2, cov12;
  Sigma.row(1) << cov12, s2;
  
  parallel_accumulator<Type> nll(this);
  // Type nll=0;  
  for (int i=0; i<Y.rows(); i++) {

    risk1=exp(beta1 + LE(i, 0));
    risk2=exp(beta2 + LE(i, 1)); level=1 + risk1 + risk2;

    prob[0]=risk1/level;
    prob[1]=risk2/level; prob[2]=1 - prob[0] - prob[1];
    
    y=Y.row(i);
    nll -= dmultinom(y, prob, true) - MVNORM(Sigma)(LE.row(i));
  }
  ADREPORT(s2);
  REPORT(Sigma);
  
  return nll;
}
// END =================================================================
