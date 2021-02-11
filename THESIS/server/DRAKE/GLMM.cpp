//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2021-fev-10 · Curitiba/PR/Brazil
//----------------------------------------------------------------------

// A STANDARD MULTINOMIAL GLMM WITH RANDOM CORRELATED INTERCEPTS

#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;

  DATA_MATRIX(Y);
  DATA_SPARSE_MATRIX(Z); // Matrix::bdiag()

  PARAMETER(beta1);
  PARAMETER(beta2);

  PARAMETER_MATRIX(R);
  
  PARAMETER_VECTOR(logs2); vector<Type> s2=exp(logs2);

  PARAMETER(rhoZ); Type rho=(exp(2*rhoZ)-1)/(exp(2*rhoZ)+1);
  
  matrix<Type> LE=Z*R;
  
  vector<Type> risk1=exp(beta1 + LE.col(0).array());
  vector<Type> risk2=exp(beta2 + LE.col(1).array());
  vector<Type> level=1 + risk1 + risk2;
  
  vector<Type> p1=risk1/level;
  vector<Type> p2=risk2/level;
  vector<Type> p3=1 - p1 - p2;
  
  matrix<Type> ps(Y.rows(), 3);
  ps.col(0)=p1;
  ps.col(1)=p2;
  ps.col(2)=p3;

  Type cov12=rho * sqrt(s2[0])*sqrt(s2[1]);
  
  matrix<Type> Sigma(2, 2);
  Sigma.row(0) << s2[0], cov12;
  Sigma.row(1) << cov12, s2[1];
  
  parallel_accumulator<Type> nll(this);
  // FOR PARALLELIZATION THE FOLLOWING LINE MUST BE COMMENTED
  // Type nll=0;
  
  vector<Type> y(Y.cols());
  vector<Type> prob(ps.cols());
  
  for (int i=0; i<Y.rows(); i++) {
    y=Y.row(i);
    prob=ps.row(i);
    nll -= dmultinom(y, prob, true);
  }
  for (int i=0; i<R.rows(); i++) {
    nll += 2*MVNORM(Sigma)(R.row(i));
  }

  ADREPORT(s2);
  ADREPORT(rho);
  REPORT(Sigma);
  return nll;
}
// END =================================================================
