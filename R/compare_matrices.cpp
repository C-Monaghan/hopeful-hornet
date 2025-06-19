// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame compare_matrices_rcpp(const arma::mat& Obs,
                                const arma::mat& Sim) {
  arma::mat D = Obs - Sim;
  arma::vec d = arma::vectorise(D);
  
  double n       = d.n_elem;
  double sum_abs = arma::accu(arma::abs(d));
  double sum_sq  = arma::accu(d % d);
  double max_abs = arma::abs(d).max();
  
  arma::vec x = arma::vectorise(Obs);
  arma::vec y = arma::vectorise(Sim);
  
  double sum_x  = arma::accu(x),    sum_y  = arma::accu(y);
  double sum_xy = arma::accu(x % y);
  double sum_x2 = arma::accu(x % x), sum_y2 = arma::accu(y % y);
  
  double frob = std::sqrt(sum_sq);
  double rmse = std::sqrt(sum_sq / n);
  double corr = (n * sum_xy - sum_x * sum_y) /
    std::sqrt((n * sum_x2 - sum_x*sum_x) *
      (n * sum_y2 - sum_y*sum_y));
  
  double eps  = 1e-10;
  double kl   = arma::accu((x + eps) % arma::log((x+eps)/(y+eps)));
  
  return DataFrame::create(
    _[ "metric" ] = CharacterVector::create("Frobenius","Manhattan","Max",
                                      "MeanAbs","RMSE","Correlation","KL"),
                                      _[ "value"  ] = NumericVector::create( frob,
                                                                        sum_abs,
                                                                        max_abs,
                                                                        sum_abs/n,
                                                                        rmse,
                                                                        1 - corr,
                                                                        kl )
  );
}

