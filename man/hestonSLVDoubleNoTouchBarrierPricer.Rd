\name{hestonSLVDoubleNoTouchBarrierPricer}
\alias{hestonSLVDoubleNoTouchBarrierPricer}
\alias{hestonSLVDoubleNoTouchBarrierPricer.default}
\title{Double No Touch Barrier Option Pricer for the Heston SLV Model}
\description{
  The \code{hestonSLVDoubleNoTouchBarrierPricer} function evaluates a double-no-touch barrier option
  with European exercise under the Heston Stochastic Volatility model using Finite Difference methods.
  The option value and the common first derivatives ("Greeks") are returned.
}

\usage{
  \code{hestonSLVDoubleNoTouchBarrierPricer}(
       referenceDate, barrier_lo, barrier_hi, rebate, barrierType, strike, optionType,
       payoffType, maturityDate, hestonProcess, leverageFunction, tGrid=51, xGrid=401,
       vGrid=51, dampingSteps=0, fdmScheme = "ModifiedCraigSneyd")
}

\arguments{
  \item{referenceDate}{a date setting the reference date for the calculation }
  \item{barrier_lo}{the lower barrier}
  \item{barrier_lo}{the upper barrier}
  \item{rebate}{ rebate if barrier is knocked out }
  \item{barrierType}{ a string with one of the values \dQuote{KnockIn}, \dQuote{KnockOut},
  \dQuote{KIKO} or \dQuote{KOKI}}
  \item{strike}{the strike price of the option}
  \item{optionType}{a string with one of the values \dQuote{call} or \dQuote{put}}
  \item{payofftype}{a string with one of the values \dQuote{PlainVanilla}, \dQuote{CashOrNothing} or \dQuote{AssetOrNothing}}
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
  The \code{hestonSLVDoubleNoTouchBarrierPricer} function returns  a list with the following
  components:
    \item{value}{value of option}
}
\examples{
process <- HestonProcess(function(t,s) {0.05}, function(t,s) {0.02},
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

leverageFct <- function(t, s) { exp(-t)*(s+70)/100.0 }

s <- seq(50, 150, 5)
plot(s, sapply(s, function(spot) {
  process["spot"] <- spot
  hestonSLVDoubleNoTouchBarrierPricer(Sys.Date(), 50, 150, 0.0, "KnockOut", 0, "call",
                                      "CashOrNothing", Sys.Date()+365, process,
                                      leverageFct)$value
  }), type="b",lty=2, ylab="NPV",xlab="Strike"
)
}
\keyword{misc}

