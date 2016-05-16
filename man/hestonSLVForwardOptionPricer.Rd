\name{hestonSLVForwardOptionPricer}
\alias{hestonSLVForwardOptionPricer}
\alias{hestonSLVForwardOptionPricer.default}
\title{Vanilla Forward Starting Option Pricer for the Heston SLV Model}
\description{
  The \code{hestonSLVForwardOptionPricer} function evaluates a forward starting vanilla european option
  under the Heston Stochastic Volatility model using Monte-Carlo simulations.
  The option value and the implied Black-Scholes-Merton volatility are returned.
}

\usage{
  \code{hestonSLVForwardOptionPricer}(referenceDate, strike, resetDate, optionType, maturityDate,
                               nSimulations, hestonProcess, leverageFunction)
}
\arguments{
  \item{referenceDate}{a date setting the reference date for the calculation }
  \item{strike}{the relative strike of the option. The absolute strike is spot value at reset date times the relative strike}
  \item{resetDate}{the reset date at which the absolute strike is fixed}
  \item{optionType}{a string with one of the values \dQuote{call} or \dQuote{put}}
  \item{maturityDate}{the maturity date}
  \item{nSimulation}{number of Monte-Carlo simulations used to calculate the option value}
  \item{hestonProcess}{the Heston model part of the HestonSLV specification}
  \item{leverageFunction}{the leverage function of the HestonSLV model}
}
\value{
  The \code{hestonSLVForwardOptionPricer} function returns a list with the following
  components:
    \item{value}{Value of option}
  \item{impliedVol}{implied Black-Scholes-Merton volatility of the option}
}
\examples{
process <- HestonProcess(function(t,s) {0.05}, function(t,s) {0.02},
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

leverageFct <- function(t, s) { exp(-t)*(s+70)/100.0 }

s <- seq(0.7, 1.5, 0.1)
plot(s, sapply(s, function(strike) {
  hestonSLVForwardOptionPricer(Sys.Date(), strike, Sys.Date()+182, "call", Sys.Date()+365,
                               4000, process, leverageFct)$impliedVol
                }), type="b",lty=2, ylab="Implied Volatility",xlab="Strike"
)
}
\keyword{misc}

