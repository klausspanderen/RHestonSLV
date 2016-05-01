.onLoad <- function(libname, pkgname) {

  library(Rcpp)

  ## if it is not interactive (as eg in testing or cronjobs), do nothing
  if (!interactive()) return(invisible(NULL))

  ## else let's test the QuantLib version, and then the intraday capability
  qlver <- getQuantLibVersion()
  if (compareVersion(qlver, "1.8")) {
    packageStartupMessage("QuantLib version ", qlver, " detected which is older than 1.8.")
    packageStartupMessage("Heston Stochastic Local Volatility models are not supported.")
  }
  else {
    packageStartupMessage("Sufficient QuantLib version ", qlver, " found.")
  }

  loadModule("HestonSLVModule", TRUE)
}
