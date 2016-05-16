\name{hestonSLVBarrierPricer}
\alias{hestonSLVBarrierPricer}
\alias{hestonSLVBarrierPricer.default}
\title{Barrier Option Pricer for the Heston SLV Model}
\description{
  The \code{hestonSLVBarrierPricer} function evaluates a barrier option
  with European exercise under the Heston Stochastic Volatility model using Finite Difference methods.
  The option value and the common first derivatives ("Greeks") are returned.
}

\usage{
  \code{hestonSLVBarrierPricer}(referenceDate, barrier, rebate, barrierType, strike, optionType,
                         maturityDate, hestonProcess, leverageFunction, tGrid=51, xGrid=401,
                         vGrid=51, dampingSteps=0, fdmScheme = "ModifiedCraigSneyd")
}
\arguments{
  \item{referenceDate}{a date setting the reference date for the calculation }
  \item{barrier}{ the barrier level }
  \item{rebate}{ rebate if barrier is knocked out }
  \item{barrierType}{ a string with one of the values \dQuote{downin}, \dQuote{downout},
  \dQuote{upin} or \dQuote{upout}}
  \item{strike}{the strike price of the option}
  \item{optionType}{a string with one of the values \dQuote{call} or \dQuote{put}}
  \item{maturityDate}{the maturity date of the barrier option}
  \item{hestonProcess}{the Heston model part of the HestonSLV specification}
  \item{leverageFunction}{the leverage function of the HestonSLV model}
  \item{tGrid}{number of time steps for the Finite Difference scheme}
  \item{xGrid}{number of grid points in spot direction}
  \item{vGrid}{number of grid points in variance direction}
  \item{dampingSteps}{number of damping steps to avoid spurious oscillations}
  \item{fdmScheme}{the Finite Difference scheme, a string with one of the values \dQuote{Hundsdorfer}, \dQuote{ModifiedHundsdorfer}, \dQuote{Douglas}, \dQuote{CraigSneyd}, \dQuote{ModifiedCraigSneyd}, \dQuote{ImplicitEuler} or \dQuote{ExplicitEuler}}
}
\value{
  The \code{hestonSLVBarrierPricer} function returns  a list with the following
  components:
    \item{value}{npv of option}
  \item{delta}{change in option value for a change in the underlying}
  \item{gamma}{change in option delta for a change in the underlying}
  \item{theta}{change in option value for a change in t}
  \item{impliedVol}{implied Black-Scholes-Merton volatility of the option}
defined}
}
\examples{
process <- HestonProcess(function(t,s) {0.05}, function(t,s) {0.02},
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

leverageFct <- function(t, s) { exp(-t)*(s+70)/100.0 }

b <- seq(50, 100, 5)
plot(b, sapply(b, function(barrier) {
  hestonSLVBarrierPricer(Sys.Date(), barrier, 0.0, "downout", 100, "put",
                        Sys.Date()+365, process, leverageFct)$gamma
  }), type="b",lty=2, ylab="NPV",xlab="Strike"
)


}
\keyword{misc}

