// MGLMM Beta
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  DATA_VECTOR(Y1);
  DATA_VECTOR(Y2);
  DATA_VECTOR(Y3);
  DATA_VECTOR(Y4);
  DATA_VECTOR(Y5);
  DATA_MATRIX(X);
  PARAMETER_VECTOR(beta1);
  PARAMETER_VECTOR(beta2);
  PARAMETER_VECTOR(beta3);
  PARAMETER_VECTOR(beta4);
  PARAMETER_VECTOR(beta5);
  PARAMETER_MATRIX(U);
  PARAMETER_VECTOR(rho);
  PARAMETER_VECTOR(sigma);
  PARAMETER_VECTOR(phi);
  
  // Preparing
  vector<Type> rho_temp(10);
  rho_temp = rho;
  
  vector<Type> sigma_temp(5);
  sigma_temp = sigma;
  
  vector<Type> mu1(Y1.size());
  vector<Type> mu2(Y2.size());
  vector<Type> mu3(Y3.size());
  vector<Type> mu4(Y4.size());
  vector<Type> mu5(Y5.size());
  
  vector<Type> phi_temp(5);
  phi_temp = phi;
  
  vector<Type> shapeA1(Y1.size());
  vector<Type> shapeA2(Y2.size());
  vector<Type> shapeA3(Y3.size());
  vector<Type> shapeA4(Y4.size());
  vector<Type> shapeA5(Y5.size());
  
  vector<Type> shapeB1(Y1.size());
  vector<Type> shapeB2(Y2.size());
  vector<Type> shapeB3(Y3.size());
  vector<Type> shapeB4(Y4.size());  
  vector<Type> shapeB5(Y5.size());  
  
  // Linear predictor for mean
  mu1 = exp(X*beta1 + U.col(0).array())/(1 + exp(X*beta1 + U.col(0).array()));
  mu2 = exp(X*beta2 + U.col(1).array())/(1 + exp(X*beta2 + U.col(1).array())); 
  mu3 = exp(X*beta3 + U.col(2).array())/(1 + exp(X*beta3 + U.col(2).array())); 
  mu4 = exp(X*beta4 + U.col(3).array())/(1 + exp(X*beta4 + U.col(3).array())); 
  mu5 = exp(X*beta5 + U.col(4).array())/(1 + exp(X*beta5 + U.col(4).array())); 
  
  // Shape A (Beta)
  shapeA1 = mu1*exp(phi(0));
  shapeA2 = mu2*exp(phi(1));
  shapeA3 = mu3*exp(phi(2));
  shapeA4 = mu4*exp(phi(3));
  shapeA5 = mu5*exp(phi(4));
  
  // Shape B (Beta)
  shapeB1 = (1-mu1)*exp(phi(0));
  shapeB2 = (1-mu2)*exp(phi(1));
  shapeB3 = (1-mu3)*exp(phi(2));
  shapeB4 = (1-mu4)*exp(phi(3));
  shapeB5 = (1-mu5)*exp(phi(4));
  
  // Full Log-likelihood  
  Type nll1 = 0;
  for(int i=0; i<Y1.size(); i++)
    nll1 -= dbeta(Y1(i), shapeA1(i), shapeB1(i), true);
  Type nll2 = 0;
  for(int i=0; i<Y2.size(); i++)
    nll2 -= dbeta(Y2(i), shapeA2(i), shapeB2(i), true);
  Type nll3 = 0;
  for(int i=0; i<Y3.size(); i++)
    nll3 -= dbeta(Y3(i), shapeA3(i), shapeB3(i), true);
  Type nll4 = 0;
  for(int i=0; i<Y4.size(); i++)
    nll4 -= dbeta(Y4(i), shapeA4(i), shapeB4(i), true);
  Type nll5 = 0;
  for(int i=0; i<Y5.size(); i++)
    nll5 -= dbeta(Y5(i), shapeA5(i), shapeB5(i), true);
  
 
 Type nll = 0;
 for(int i = 0; i < Y1.size(); i++)
   nll += VECSCALE(UNSTRUCTURED_CORR(rho), sigma)(U.row(i));
 
 matrix<Type> Cor(5,5);
 Cor = UNSTRUCTURED_CORR(rho).cov();
 REPORT(Cor);
  return nll1 + nll2 + nll3 + nll4 + nll5 + nll;
}
