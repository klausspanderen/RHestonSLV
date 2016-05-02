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

# simple abbreviation for a constant function
cf <- function(r) { function(t) { r } }

# remove extrapolation from leverage function
leverageValue <- function(model, t, strike, extrapolation) {
  value <- model$leverageFunction(t, strike)
  valueUp <- model$leverageFunction(t, strike*1.0001)

  if (!extrapolation && (is.nan(value) || is.nan(valueUp) || (value == valueUp)))
    NA
  else
    value
}

leverageFunction <- function(model, extrapolation) {
  function(t, s) {
    l <- leverageValue(model, t, s, extrapolation)
    if (is.nan(l))
      0.5
    else
      l
  }
}
