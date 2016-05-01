#include <Rcpp.h>
#include <ql/version.hpp>


// [[Rcpp::export]]
std::string getQuantLibVersion() {
  return std::string(QL_VERSION);
}
