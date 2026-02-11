// Informs Rcpp to link against the RcppArmadillo library for linear algebra support.
// [[Rcpp::depends(RcppArmadillo)]]

// Include Armadillo and Rcpp headers for matrix and vector operations.
#include <RcppArmadillo.h> 
using namespace Rcpp;

// Export the following function to R using Rcpp
// [[Rcpp::export]]
DataFrame compare_matrices_rcpp(const arma::mat& Obs,
                                const arma::mat& Sim) {
  // Compute the difference matrix D = Obs - Sim
  arma::mat D = Obs - Sim;
  
  // Flatten the difference matrix into a column vector 
  // (column-wise vectorisation)
  arma::vec d = arma::vectorise(D);
  
  // Get the number of elements (scalar count of entries in d)
  double n       = d.n_elem;
  
  // Compute the Manhattan norm: sum of absolute differences
  double sum_abs = arma::accu(arma::abs(d));
  
  // Compute the squared error (sum of squared differences)
  double sum_sq  = arma::accu(d % d);
  
  // Compute the maximum absolute error
  double max_abs = arma::abs(d).max();
  
  // Vectorise original matrices into column vectors for correlation and KL
  arma::vec x = arma::vectorise(Obs);
  arma::vec y = arma::vectorise(Sim);
  
  // Compute dot products and sums needed for correlation calculation
  double sum_x  = arma::accu(x),    sum_y  = arma::accu(y);
  double sum_xy = arma::accu(x % y);
  double sum_x2 = arma::accu(x % x), sum_y2 = arma::accu(y % y);
  
  // Frobenius norm: square root of the sum of squared differences
  double frob = std::sqrt(sum_sq);
  
  // RMSE: root mean square error
  double rmse = std::sqrt(sum_sq / n);
  
  // Pearson correlation formula (manual implementation using sums)
  double denom_corr = std::sqrt((n * sum_x2 - sum_x * sum_x) *
                                (n * sum_y2 - sum_y * sum_y));
  double corr = NA_REAL;
  if (denom_corr != 0 && !std::isnan(denom_corr) && !std::isinf(denom_corr)) {
    corr = (n * sum_xy - sum_x * sum_y) / denom_corr;
  }
  
  // Kullback–Leibler divergence (approximated): sum of x * log(x / y)
  double eps  = 1e-10;
  double kl   = arma::accu((x + eps) % arma::log((x+eps)/(y+eps)));
  
  // Return all metrics in an R DataFrame
  return DataFrame::create(
    _[ "metric" ] = CharacterVector::create(
      "Frobenius","Manhattan","Max", "MeanAbs","RMSE","Correlation","KL"),
      _[ "value"  ] = NumericVector::create(
        frob,           // Frobenius norm
        sum_abs,        // Manhattan (L1) distance
        max_abs,        // Maximum absolute error
        sum_abs/n,      // Mean absolute error
        rmse,           // Root mean square error
        (NumericVector::is_na(corr) ? NA_REAL : 1 - corr),       // 1 - Pearson correlation (interpreted as dissimilarity)
        kl             // KL divergence
        )
  );
}

