// multiGLMM: A MULTINOMIAL GLMM FOR CLUSTERED COMPETING RISKS DATA
// 2x2 LATENT STRUCTURE ON THE RISK LEVEL (RISK MODEL)
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  DATA_MATRIX(Y);
  DATA_SPARSE_MATRIX(Z);
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
  PARAMETER(rhoZ12); Type rho12=(exp(2*rhoZ12)-1)/(exp(2*rhoZ12)+1);

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

  Type cov12=rho12*sqrt(s2_1)*sqrt(s2_2);
  matrix<Type> Sigma(2, 2);
  Sigma.row(0) << s2_1, cov12;
  Sigma.row(1) << cov12, s2_2;

  MVNORM_t<Type> dmvnorm(Sigma);
  for (int i=0; i<U.rows(); i++) {
    u=U.row(i);
    nll += dmvnorm(u);
  }
  for (int i=0; i<Y.rows(); i++) {
    risk1=exp(beta1 + ZU(i, 0));
    risk2=exp(beta2 + ZU(i, 1));
    level=1 + risk1 + risk2;
    x1=w1*gt(i) - gama1;
    x2=w2*gt(i) - gama2;
    prob(0)=risk1/level * w1*dgt(i) * dnorm(x1, Type(0), Type(1), false);
    prob(1)=risk2/level * w2*dgt(i) * dnorm(x2, Type(0), Type(1), false);
    prob(2)=1 - prob(0) - prob(1);
    y=Y.row(i);
    nll -= dmultinom(y, prob, true);
  }
  ADREPORT(s2_1);
  ADREPORT(s2_2);
  ADREPORT(rho12);
  REPORT(Sigma);
  return nll;
}
