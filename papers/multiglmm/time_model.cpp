//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2022-mar-18 · Curitiba/PR/Brazil
//----------------------------------------------------------------------
// multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA

// v2: CORRELATED LATENT EFFECTS ON TIME TRAJECTORY

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

  PARAMETER(logs2_3); Type s2_3=exp(logs2_3);
  PARAMETER(logs2_4); Type s2_4=exp(logs2_4);

  PARAMETER(rhoZ34); Type rho34=(exp(2*rhoZ34) - 1)/(exp(2*rhoZ34) + 1);

  PARAMETER_MATRIX(U); matrix<Type> ZU=Z*U;

  Type risk1=exp(beta1);
  Type risk2=exp(beta2);
  Type level=1 + risk1 + risk2;
  
  // gt=atanh(2*time/delta-1); atanh(x)=0.5*log((1+x)/(1-x))
  vector<Type> gt=0.5*log(time/(delta-time));

  vector<Type> dgt=delta/(2*time*(delta-time));

  Type x1=0;
  Type x2=0;
  vector<Type> y(Y.cols());
  vector<Type> prob(Y.cols());
  
  parallel_accumulator<Type> nll(this);
  // Type nll=0;

  vector<Type> u(U.cols());

  Type cov34=rho34 * sqrt(s2_3)*sqrt(s2_4);

  matrix<Type> Sigma(2, 2);
  Sigma.row(0) << s2_3, cov34;
  Sigma.row(1) << cov34, s2_4;

  MVNORM_t<Type> dmvnorm(Sigma);

  for (int i=0; i<U.rows(); i++) {

    u=U.row(i);
    nll += dmvnorm(u);
  }
  
  for (int i=0; i<Y.rows(); i++) {

    x1=w1*gt(i) - gama1 - ZU(i, 0);
    x2=w2*gt(i) - gama2 - ZU(i, 1);
    
    prob(0)=risk1/level * w1*dgt(i) * dnorm(x1, Type(0), Type(1), false);
    prob(1)=risk2/level * w2*dgt(i) * dnorm(x2, Type(0), Type(1), false);

    prob(2)=1 - prob(0) - prob(1);
    
    y=Y.row(i);
    nll -= dmultinom(y, prob, true);
  }
  ADREPORT(s2_3);
  ADREPORT(s2_4);
  ADREPORT(rho34);
  REPORT(Sigma);
  
  return nll;
}
// END -----------------------------------------------------------------
