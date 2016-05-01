
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
