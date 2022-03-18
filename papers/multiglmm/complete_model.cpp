//----------------------------------------------------------------------
//                                                     Henrique Laureano
//                                            henriquelaureano.github.io
//                                      2022-mar-18 · Curitiba/PR/Brazil
//----------------------------------------------------------------------
// multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA

// v4: CORRELATED LATENT EFFECTS ON THE RISK, TIME TRAJECTORY, AND IN
//     ALL ITS INTERACTIONS

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
  PARAMETER(rhoZ13); Type rho13=(exp(2*rhoZ13) - 1)/(exp(2*rhoZ13) + 1);
  PARAMETER(rhoZ14); Type rho14=(exp(2*rhoZ14) - 1)/(exp(2*rhoZ14) + 1);
  PARAMETER(rhoZ23); Type rho23=(exp(2*rhoZ23) - 1)/(exp(2*rhoZ23) + 1);
  PARAMETER(rhoZ24); Type rho24=(exp(2*rhoZ24) - 1)/(exp(2*rhoZ24) + 1);
  PARAMETER(rhoZ34); Type rho34=(exp(2*rhoZ34) - 1)/(exp(2*rhoZ34) + 1);

  PARAMETER_MATRIX(U); matrix<Type> ZU=Z*U;

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

  vector<Type> u(U.cols());

  Type cov12=rho12 * sqrt(s2_1)*sqrt(s2_2);
  Type cov13=rho13 * sqrt(s2_1)*sqrt(s2_3);
  Type cov14=rho14 * sqrt(s2_1)*sqrt(s2_4);
  Type cov23=rho23 * sqrt(s2_2)*sqrt(s2_3);
  Type cov24=rho24 * sqrt(s2_2)*sqrt(s2_4);
  Type cov34=rho34 * sqrt(s2_3)*sqrt(s2_4);

  matrix<Type> Sigma(4, 4);
  Sigma.row(0) << s2_1, cov12, cov13, cov14;
  Sigma.row(1) << cov12, s2_2, cov23, cov24;
  Sigma.row(2) << cov13, cov23, s2_3, cov34;
  Sigma.row(3) << cov14, cov24, cov34, s2_4;

  MVNORM_t<Type> dmvnorm(Sigma);

  for (int i=0; i<U.rows(); i++) {

    u=U.row(i);
    nll += dmvnorm(u);
  }
  
  for (int i=0; i<Y.rows(); i++) {

    risk1=exp(beta1 + ZU(i, 0));
    risk2=exp(beta2 + ZU(i, 1));

    level=1 + risk1 + risk2;

    x1=w1*gt(i) - gama1 - ZU(i, 2);
    x2=w2*gt(i) - gama2 - ZU(i, 3);
    
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
  ADREPORT(rho13);
  ADREPORT(rho14);
  ADREPORT(rho23);
  ADREPORT(rho24);
  ADREPORT(rho34);

  REPORT(Sigma);
  
  return nll;
}
// END -----------------------------------------------------------------
