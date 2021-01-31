// [[Rcpp::depends(RcppArmadillo)]]

#include "DataPairs.h"

// Constructors for class "data"
DataPairs::DataPairs(unsigned ncauses, imat causes, mat alpha, mat dalpha, mat beta, mat gamma) {
  this->ncauses = ncauses;
  ncol = (double)ncauses*2;
  nrow = 1;

  this->causes = causes;
  this->alpha = alpha;
  this->dalpha = dalpha;
  this->beta = beta;
  this->gamma = gamma;
  pi = arma::mat(nrow,ncol);
  dpidu = arma::mat(nrow*(double)ncauses,ncol);
  dlogpidu = arma::mat(nrow*(double)ncauses,ncol);
};

DataPairs::DataPairs(unsigned ncauses) {
  this->ncauses = ncauses;
  ncol = (double)ncauses*2;
  nrow = 1;

  mat x0 = arma::mat(nrow,ncol);
  mat x1 = arma::mat(nrow*(double)ncauses,ncol);
  x0.zeros();
  x1.zeros();

  causes = arma::imat(nrow,2);
  causes.zeros();
  alpha = x0;
  dalpha = x0;
  beta = x0;
  gamma = x0;
  pi = x0;
  dpidu = x1;
  dlogpidu = x1;
}

// Member functions for class DataPairs object causes
irowvec DataPairs::causes_get(int i) const {
  return(causes.row(i));
};

int DataPairs::causesMarg_get(int i, int indiv) const {
  int pos = indiv;
  return(causes(i,pos));
};

// Member functions for class DataPairs object alpha
rowvec DataPairs::alpha_get(int i, irowvec bothcauses) const {
  uvec row(1); row(0) = i;
  int causefirst = bothcauses(0);
  int causesecond = bothcauses(1);
  uvec pos(2);
  pos(0) = causefirst - 1;
  pos(1) = ncauses + causesecond - 1;
  return(alpha.submat(row,pos));
};

double DataPairs::alphaMarg_get(int i, int cause, int indiv) const {
  int pos;
    if (indiv==0){
      pos = cause - 1;
    }
    else {
      pos = ncauses + cause - 1;
    }
    return(alpha(i,pos));
};

// Member functions for class DataPairs object dalpha
rowvec DataPairs::dalpha_get(int i, irowvec bothcauses) const {
  uvec row(1); row(0) = i;
  int causefirst = bothcauses(0);
  int causesecond = bothcauses(1);
  uvec pos(2);
  pos(0) = causefirst - 1;
  pos(1) = ncauses + causesecond - 1;
  return(dalpha.submat(row,pos));
};

double DataPairs::dalphaMarg_get(int i, int cause, int indiv) const {
  int pos;
    if (indiv==0){
      pos = cause - 1;
    }
    else {
      pos = ncauses + cause - 1;
    }
    return(dalpha(i,pos));
};

// Member functions for class DataPairs object beta
rowvec DataPairs::beta_get(int i, irowvec bothcauses) const {
  uvec row(1); row(0) = i;
  int causefirst = bothcauses(0);
  int causesecond = bothcauses(1);
  uvec pos(2);
  pos(0) = causefirst - 1;
  pos(1) = ncauses + causesecond - 1;
  return(beta.submat(row,pos));
};

double DataPairs::betaMarg_get(int i, int cause, int indiv) const {
  int pos;
    if (indiv==0){
      pos = cause - 1;
    }
    else {
      pos = ncauses + cause - 1;
    }
    return(beta(i,pos));
};

// Member functions for class DataPairs object gamma
rowvec DataPairs::gamma_get(int i, irowvec bothcauses) const {
  uvec row(1); row(0) = i;
  int causefirst = bothcauses(0);
  int causesecond = bothcauses(1);
  uvec pos(2);
  pos(0) = causefirst - 1;
  pos(1) = ncauses + causesecond - 1;
  return(gamma.submat(row,pos));
};

double DataPairs::gammaMarg_get(int i, int cause, int indiv) const {
  int pos;
    if (indiv==0){
      pos = cause - 1;
    }
    else {
      pos = ncauses + cause - 1;
    }
    return(gamma(i,pos));
};

// Member functions for class DataPairs object pi
rowvec DataPairs::pi_get(int i, irowvec bothcauses) const {
  // Restriction: cause > 0
  uvec row(1); row(0) = i;
  int causefirst = bothcauses(0);
  int causesecond = bothcauses(1);
  uvec pos(2);
  pos(0) = causefirst - 1;
  pos(1) = ncauses + causesecond - 1;
  return(pi.submat(row,pos));
};

double DataPairs::piMarg_get(int i, int cause, int indiv) const {
  // Restriction: cause > 0
  int pos;
    if (indiv==0){
      pos = cause - 1;
    }
    else {
      pos = ncauses + cause - 1;
    }
    return(pi(0,pos));
};

// Member functions for class DataPairs object dpidu
double DataPairs::dpiduMarg_get(int i, int cause, int indiv, int dcause) const {
  // Restriction: cause > 0
  int pos1;
  int pos2;
  pos1 = dcause - 1;
    if (indiv==0){
      pos2 = cause - 1;
    }
    else {
      pos2 = ncauses + cause - 1;
    }
    return(dpidu(pos1,pos2));
};

rowvec DataPairs::dpiduMarg_get(int i, int cause, int indiv) const {
  // Restriction: cause > 0
  int cause_du;
  if (indiv==0){
    cause_du = cause - 1;
  }
  else {
    cause_du = ncauses + cause - 1;
    }
  colvec out = dpidu.col(cause_du);
  return(out.t());
};


// Member functions for class DataPairs object dlogpidu
double DataPairs::dlogpiduMarg_get(int i, int cause, int indiv, int dcause) const {
  // Restriction: cause > 0
  int pos1;
  int pos2;
  pos1 = dcause - 1;
    if (indiv==0){
      pos2 = cause - 1;
    }
    else {
      pos2 = ncauses + cause - 1;
    }
    return(dlogpidu(pos1,pos2));
};

rowvec DataPairs::dlogpiduMarg_get(int i, int cause, int indiv) const {
  // Restriction: cause > 0
  int cause_du;
  if (indiv==0){
    cause_du = cause - 1;
    }
  else {
    cause_du = ncauses + cause - 1;
    }
  colvec out = dlogpidu.col(cause_du);
  return(out.t());
};

// Generating pi based on beta and u
void DataPairs::pi_gen(int i, vec u){
  for (unsigned j=0; j<2; j++){
    for (unsigned k=1; k<=ncauses; k++){

      double num = exp(betaMarg_get(i, k, j)+u(k-1)) ;
      double denum = 1;

      for (unsigned l=1; l<=ncauses; l++){
	denum += exp(betaMarg_get(i, l, j)+u(l-1));
      };
      double pi = num/denum;

      if (j==0){
	unsigned pos = k - 1;
	this->pi(0,pos) = pi;
      }
      else {
	unsigned pos = ncauses + k - 1;
	this->pi(0,pos) = pi;
      };
    };
  };
};

// Generating dpidu based on beta and u
void DataPairs::dpidu_gen(int i, vec u){
  for (unsigned j=0; j<2; j++){
    for (unsigned k=1; k<=ncauses; k++){
      double num1 = exp(betaMarg_get(i, k, j)+u(k-1));
      double denum = 1;
      for (unsigned l=1; l<=ncauses; l++){
	denum += exp(betaMarg_get(i, l, j)+u(l-1));
      };
      for (unsigned d=1; d<=ncauses; d++){
	double dpidu = 0;
	if (d==k){
	  dpidu = num1/denum-pow(num1,2)/pow(denum,2);
	}
	else {
	  double num2 = exp(betaMarg_get(i, d, j)+u(d-1));
	  dpidu = -(num1*num2)/pow(denum,2);
	};
	if (j==0){
	  unsigned pos = k - 1;
	  this->dpidu(d-1,pos) = dpidu;
	}
	else {
	  unsigned pos = ncauses + k - 1;
	  this->dpidu(d-1,pos) = dpidu;
	};
      };
    };
  };
};

// Generating dlogpidu based on beta and u
void DataPairs::dlogpidu_gen(int i, vec u){
  for (unsigned j=0; j<2; j++){
    for (unsigned k=1; k<=ncauses; k++){
      double denum = 1;
      for (unsigned l=1; l<=ncauses; l++){
	denum += exp(betaMarg_get(i, l, j)+u(l-1));
      };
      for (unsigned d=1; d<=ncauses; d++){
	double dlogpidu = 0;
	if (d==k){
	  double num = exp(betaMarg_get(i, k, j)+u(k-1));
	  dlogpidu = 1-num/denum;
	}
	else {
	  double num = exp(betaMarg_get(i, d, j)+u(d-1));
	  dlogpidu = -num/denum;
	};
	if (j==0){
	  unsigned pos = k - 1;
	  this->dlogpidu(d-1,pos) = dlogpidu;
	}
	else {
	  unsigned pos = ncauses + k - 1;
	  this->dlogpidu(d-1,pos) = dlogpidu;
	};
      };
    };
  };
};

