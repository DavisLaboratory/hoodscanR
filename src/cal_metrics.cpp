#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame calculate_metrics(NumericMatrix probs) {

  int n_rows = probs.nrow();
  int n_cols = probs.ncol();

  NumericVector entropy(n_rows);
  NumericVector perplexity(n_rows);
  //NumericVector mean(n_rows);
  //NumericVector variance(n_rows);
  //NumericVector sd(n_rows);

  for (int i = 0; i < n_rows; i++) {

    // calculate entropy
    entropy[i] = 0;
    for (int j = 0; j < n_cols; j++) {
      if (probs(i, j) > 0) {
        entropy[i] -= probs(i, j) * log2(probs(i, j));
      }
    }

    // calculate perplexity
    perplexity[i] = pow(2, entropy[i]);

    // calculate mean
    //mean[i] = 0;
    //for (int j = 0; j < n_cols; j++) {
    //  mean[i] += (j+1) * probs(i, j);
    //}

    // calculate variance
    //variance[i] = 0;
    //for (int j = 0; j < n_cols; j++) {
    //  variance[i] += pow((j+1) - mean[i], 2) * probs(i, j);
    //}

    // calculate standard deviation
    //sd[i] = sqrt(variance[i]);

  }
  // create output data frame
  DataFrame output = DataFrame::create(
    _["entropy"] = entropy,
    _["perplexity"] = perplexity
    //_["mean"] = mean,
    //_["variance"] = variance,
    //_["sd"] = sd
  );

  return output;
}
