//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2021-fev-03 · Curitiba/PR/Brazil
//----------------------------------------------------------------------

// multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA

// diag1: DIAGONAL VCOV WITH JUST ONE PARAMETER

#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;

  DATA_MATRIX(Y);
  DATA_SPARSE_MATRIX(Z); // Matrix::bdiag()
  DATA_VECTOR(time);
  DATA_SCALAR(delta);

  PARAMETER(beta1);
  PARAMETER(beta2);
  PARAMETER(gama1);
  PARAMETER(gama2);
  PARAMETER(w1);
  PARAMETER(w2);

  PARAMETER_MATRIX(R);
  PARAMETER(logs2); Type s2=exp(logs2);

  matrix<Type> LE=Z*R;
  
  vector<Type> risk1=exp(beta1 + LE.col(0).array());
  vector<Type> risk2=exp(beta2 + LE.col(1).array());
  vector<Type> level=1 + risk1 + risk2;
  
  // gt=atanh(2*time/delta-1); atanh(x)=0.5*log((1+x)/(1-x))
  vector<Type> gt=0.5*log(time/(delta-time));
  vector<Type> dgt=delta/(2*time*(delta-time));

  vector<Type> x1=w1*gt - gama1 - LE.col(2).array();
  vector<Type> x2=w2*gt - gama2 - LE.col(3).array();
  
  vector<Type> p1=risk1/level * w1*dgt * dnorm(x1, 0, 1, false);
  vector<Type> p2=risk2/level * w2*dgt * dnorm(x2, 0, 1, false);
  vector<Type> p3=1 - p1 - p2;
  
  matrix<Type> ps(Y.rows(), 3);
  ps.col(0)=p1;
  ps.col(1)=p2;
  ps.col(2)=p3;
  
  matrix<Type> Sigma(4, 4);
  Sigma.row(0) << s2, 0, 0, 0;
  Sigma.row(1) << 0, s2, 0, 0;
  Sigma.row(2) << 0, 0, s2, 0;
  Sigma.row(3) << 0, 0, 0, s2;
  
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
  REPORT(Sigma);
  return nll;
}
// END =================================================================
