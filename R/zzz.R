#  Copyright (C) 2016 Klaus Spanderen
#
#  This file is part of RHestonSLV.
#
#  RHestonSLV is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, version 2 of the License
#
#  RHestonSLV is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with RHestonSLV.  If not, see <http:#www.gnu.org/licenses/>.

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
