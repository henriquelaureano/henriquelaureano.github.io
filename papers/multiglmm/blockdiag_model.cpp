//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2022-mar-18 · Curitiba/PR/Brazil
//----------------------------------------------------------------------
// multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA

// v3: CORRELATED LATENT EFFECTS ON BOTH RISK AND TIME TRAJECTORY

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

  PARAMETER(logs2_1); Type s2_1=exp(logs2_1);
  PARAMETER(logs2_2); Type s2_2=exp(logs2_2);
  PARAMETER(logs2_3); Type s2_3=exp(logs2_3);
  PARAMETER(logs2_4); Type s2_4=exp(logs2_4);

  PARAMETER(rhoZ12); Type rho12=(exp(2*rhoZ12) - 1)/(exp(2*rhoZ12) + 1);
  PARAMETER(rhoZ34); Type rho34=(exp(2*rhoZ34) - 1)/(exp(2*rhoZ34) + 1);

  PARAMETER_MATRIX(U1); matrix<Type> ZU1=Z*U1;
  PARAMETER_MATRIX(U2); matrix<Type> ZU2=Z*U2;

  Type risk1=0;
  Type risk2=0;
  Type level=0;

  // gt=atanh(2*time/delta-1); atanh(x)=0.5*log((1+x)/(1-x))
  vector<Type> gt=0.5*log(time/(delta-time));

  vector<Type> dgt=delta/(2*time*(delta-time));

  Type x1=0;
  Type x2=0;
  vector<Type> y(Y.cols());
  vector<Type> prob(Y.cols());
  
  parallel_accumulator<Type> nll(this);
  // Type nll=0;

  vector<Type> u1(U1.cols());
  vector<Type> u2(U2.cols());

  Type cov12=rho12 * sqrt(s2_1)*sqrt(s2_2);
  Type cov34=rho34 * sqrt(s2_3)*sqrt(s2_4);

  matrix<Type> Sigma1(2, 2);
  Sigma1.row(0) << s2_1, cov12;
  Sigma1.row(1) << cov12, s2_2;

  matrix<Type> Sigma2(2, 2);
  Sigma2.row(0) << s2_3, cov34;
  Sigma2.row(1) << cov34, s2_4;
  
  MVNORM_t<Type> dmvnorm1(Sigma1);
  MVNORM_t<Type> dmvnorm2(Sigma2);

  for (int i=0; i<U1.rows(); i++) {
    
    u1=U1.row(i);
    nll += dmvnorm1(u1);
  }
  for (int i=0; i<U2.rows(); i++) {
    
    u2=U2.row(i);
    nll += dmvnorm2(u2);
  }
  
  for (int i=0; i<Y.rows(); i++) {

    risk1=exp(beta1 + ZU1(i, 0));
    risk2=exp(beta2 + ZU1(i, 1));

    level=1 + risk1 + risk2;

    x1=w1*gt(i) - gama1 - ZU2(i, 0);
    x2=w2*gt(i) - gama2 - ZU2(i, 1);
    
    prob(0)=risk1/level * w1*dgt(i) * dnorm(x1, Type(0), Type(1), false);
    prob(1)=risk2/level * w2*dgt(i) * dnorm(x2, Type(0), Type(1), false);

    prob(2)=1 - prob(0) - prob(1);
    
    y=Y.row(i);
    nll -= dmultinom(y, prob, true);
  }
  ADREPORT(s2_1);
  ADREPORT(s2_2);
  ADREPORT(s2_3);
  ADREPORT(s2_4);

  ADREPORT(rho12);
  ADREPORT(rho34);

  REPORT(Sigma1);
  REPORT(Sigma2);
  
  return nll;
}
// END -----------------------------------------------------------------
