//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2021-fev-18 · Curitiba/PR/Brazil
//----------------------------------------------------------------------

// A STANDARD MULTINOMIAL GLMM WITH INDEPENDENT RANDOM INTERCEPTS

#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  
  DATA_MATRIX(Y);
  DATA_SPARSE_MATRIX(Z); // Matrix::bdiag()

  PARAMETER(beta1);
  PARAMETER(beta2);
  
  PARAMETER(logs2_1); Type s2_1=exp(logs2_1);
  PARAMETER(logs2_2); Type s2_2=exp(logs2_2);
  
  PARAMETER_MATRIX(U); matrix<Type> ZU=Z*U; 

  Type risk1=0;
  Type risk2=0; Type level=0;

  vector<Type> y(Y.cols());
  vector<Type> prob(Y.cols());
  
  parallel_accumulator<Type> nll(this);
  // Type nll=0;

  vector<Type> Zu(ZU.cols());

  matrix<Type> Sigma(2, 2);
  Sigma.row(0) << s2_1, 0;
  Sigma.row(1) << 0, s2_2;

  MVNORM_t<Type> dmvnorm(Sigma);
  
  for (int i=0; i<Y.rows(); i++) {

    risk1=exp(beta1 + ZU(i, 0));
    risk2=exp(beta2 + ZU(i, 1)); level=1 + risk1 + risk2;

    prob(0)=risk1/level;
    prob(1)=risk2/level; prob(2)=1 - prob(0) - prob(1);
    
    y=Y.row(i);
    nll -= dmultinom(y, prob, true);

    Zu=ZU.row(i);
    nll += dmvnorm(Zu);
  }
  ADREPORT(s2_1);
  ADREPORT(s2_2);
  REPORT(Sigma);
  
  return nll;
}
// END =================================================================
