// Beta mixed regression model 
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Y);         // Observations
  DATA_SPARSE_MATRIX(X);  // Fixed effect design matrix
  PARAMETER_VECTOR(beta); // Fixed effects vector
  DATA_SPARSE_MATRIX(Z);  // Random effect design matrix
  PARAMETER_VECTOR(u);    // Random effects vector
  PARAMETER(logsigma);    // Random effect standard deviation
  PARAMETER(logphi);      // Precision parameter (Beta)

  // Preparing
  Type phi = exp(logphi);
  Type sigma = exp(logsigma);
  
  // Distribution of random effect (u):
  Type nll = 0;
  nll -= dnorm(u, Type(0), sigma, true).sum();

  // Linear predictor for mean
  vector<Type> mu = exp(X * beta + Z * u)/(1 + exp(X * beta + Z * u)); 
    
  // Log-likelihood
    for(int i=0; i < Y.size(); i++)
    nll -= dbeta(Y(i), mu(i)*phi, (1 - mu(i))*phi, true);
  
  // Delta method 
    ADREPORT(sigma);
    ADREPORT(phi);
  return nll;
}

