\name{hestonSLVOptionPricer}
\alias{hestonSLVOptionPricer}
\alias{hestonSLVOptionPricer.default}
\title{Vanilla Option Pricer for the Heston SLV Model}
\description{
  The \code{hestonSLVOptionPricer} function evaluates a vanilla option
  with European or American exercise
  under the Heston Stochastic Volatility model using Finite Difference methods.
  The option value and the common first
  derivatives ("Greeks") are returned.
}
\usage{
  \code{hestonSLVOptionPricer}(referenceDate, strike, optionType, exerciseType,
                        maturityDate, hestonProcess, leverageFunction, tGrid=51, xGrid=401,
                        vGrid=51, dampingSteps=0, fdmScheme = "ModifiedCraigSneyd")
}
\arguments{
  \item{referenceDate}{a date setting the reference date for the calculation }
  \item{strike}{the strike price of the option}
  \item{optionType}{a string with one of the values \dQuote{call} or \dQuote{put}}
  \item{exerciseType}{a string with one of the values \dQuote{european} or \dQuote{american}}
  \item{maturityDate}{the maturity date}
  \item{hestonProcess}{the Heston model part of the HestonSLV specification}
  \item{leverageFunction}{the leverage function of the HestonSLV model}
  \item{tGrid}{number of time steps for the Finite Difference scheme}
  \item{xGrid}{number of grid points in spot direction}
  \item{vGrid}{number of grid points in variance direction}
  \item{dampingSteps}{number of damping steps to avoid spurious oscillations}
  \item{fdmScheme}{the Finite Difference scheme, a string with one of the values \dQuote{Hundsdorfer}, \dQuote{ModifiedHundsdorfer}, \dQuote{Douglas}, \dQuote{CraigSneyd}, \dQuote{ModifiedCraigSneyd}, \dQuote{ImplicitEuler} or \dQuote{ExplicitEuler}}
}
\value{
  The \code{hestonSLVOptionPricer} function returns  a list with the following
  components:
    \item{value}{npv of option}
  \item{delta}{change in option value for a change in the underlying}
  \item{gamma}{change in option delta for a change in the underlying}
  \item{theta}{change in option value for a change in t}
  \item{impliedVol}{implied Black-Scholes-Merton volatility of the option}
}
\examples{
process <- HestonProcess(function(t,s) {0.05}, function(t,s) {0.02},
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

leverageFct <- function(t, s) { exp(-t)*(s+70)/100.0 }

s <- seq(50, 200, 10)
plot(s, sapply(s, function(strike) {
  hestonSLVOptionPricer(Sys.Date(), strike, "call", "european",
                        Sys.Date()+182, process, leverageFct)$impliedVol
                }), type="b",lty=2, ylab="Implied Volatility",xlab="Strike"
)
}
\keyword{misc}

