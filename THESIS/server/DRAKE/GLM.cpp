//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2021-fev-12 · Curitiba/PR/Brazil
//----------------------------------------------------------------------
// A STANDARD MULTINOMIAL GLM
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;

  DATA_MATRIX(Y);

  PARAMETER(beta1);
  PARAMETER(beta2);

  vector<Type> y(Y.cols());
  
  Type risk1=exp(beta1);
  Type risk2=exp(beta2); Type level=1 + risk1 + risk2;
  
  vector<Type> prob(Y.cols());
  
  prob[0]=risk1/level;
  prob[1]=risk2/level; prob[2]=1 - prob[0] - prob[1];

  parallel_accumulator<Type> nll(this);
  // Type nll=0;  
  for (int i=0; i<Y.rows(); i++) {

    y=Y.row(i);
    nll -= dmultinom(y, prob, true);
  }
  return nll;
}
// END =================================================================
