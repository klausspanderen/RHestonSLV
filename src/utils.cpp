//  Copyright (C) 2016 Klaus Spanderen
//
//  This file is part of RHestonSLV.
//
//  RHestonSLV is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, version 2 of the License
//
//  RHestonSLV is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with RHestonSLV.  If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
#include <ql/version.hpp>


// [[Rcpp::export]]
std::string getQuantLibVersion() {
  return std::string(QL_VERSION);
}
