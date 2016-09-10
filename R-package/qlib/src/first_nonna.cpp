
#include <Rcpp.h>
using namespace Rcpp;

//' Return the index of the first non-NA value
//'
//' @param v numeric vector
//' @return position of the first non-NA value. Return \code{length(v)+1} if the array only contains NAs.
//'
//' @export
// [[Rcpp::export]]
unsigned long first_nonna(NumericVector v) {
  unsigned long n;
  for (n = 0; n < v.size(); n++) {
    if (!NumericVector::is_na(v[n])) 
      break;
  }
  return n+1;
}
