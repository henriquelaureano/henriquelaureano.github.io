// Beta Univariate regression model
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Y);           // Observations
  DATA_MATRIX(X);           // Fixed effect design matrix  
  PARAMETER_VECTOR(beta);   // Fixed effects vector
  PARAMETER(logphi);        // Precision parameter (Beta)
  
  // Preparing
  Type phi = exp(logphi);
  
  // Linear predictor for mean
  vector<Type> mu = exp(X*beta)/(1 + exp(X*beta));
  
  // Log-likelihood
  Type nll = 0;
  for(int i=0; i < Y.size(); i++)
    nll -= dbeta(Y(i), mu(i)*phi, (1 - mu(i))*phi, true);
  
  // Report (Delta method)  
  ADREPORT(phi);

  return nll;
}

