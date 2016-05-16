\name{HestonSLVMCModel}
\Rdversion{1.1}
\docType{class}
\alias{HestonSLVMCModel}

\title{Class \code{"HestonSLVMCModel"}}
\description{
Calibration of the Heston Stochastic Local Volatility model
\deqn{
\begin{array}{ll}
  d x_t  = \left( r_t - q_t - \frac{{L^2(t, x_t)}}{2}\nu_t\right) dt + {L(t, x_t)}\sqrt{\nu_t}dW^{x}_t  \\
  d\nu_t =\kappa\left(\theta-\nu_t\right)dt
			            +{\eta}\sigma \sqrt{\nu_t}dW^{\nu}_t \\
\rho dt =  dW^{\nu}_t dW^x_t
\end{array}
}
via Monte-Carlo simulations.
}
\section{Objects from the Class}{
An object of this class calibrates a Heston Stochastic Local Volatility model to a given local volatility model w.r.t. a Heston process. Objects can be created by calls of the form \cr
\code{new("HestonSLVMCModel", referenceDate, maxCalibrationDate,\cr
localVolFunction, hestonProcess, hestonSLVMCParams)}
}
\arguments{
  \item{referenceDate}{a date setting the reference date for the calibration}
  \item{maxCalibrationDate}{a date setting the end date of the calibration}
  \item{localVolFunction}{a function in (time, underlying) defining the local volatility function.
  The HestonSLVMCModel calculates the leverage function such that the Heston SLV model defined by the Heston
  process and the leverage function gives the same prices as the local volatility model.  }
  \item{hestonProcess}{an object of the class HestonProcess}
  \item{hestonSLVMCParams}{an object of the class HestonSLVMCParams}
}
\references{
A.Stoep, L. Grzelak, C. Oosterlee, The Heston Stochastic-Local Volatility Model:
Efficient Monte Carlo Simulation,
\url{http://ta.twi.tudelft.nl/mf/users/oosterle/oosterlee/anton1.pdf}\cr
Johannes Goettker-Schnetmann, Klaus Spanderen,
Calibrating the Heston Stochastic Local Volatility Model using the Fokker-Planck Equation,
 \url{http://hpc-quantlib.de/src/slv.pdf}
}

\examples{
#flat local volatility surface
localVol <- function(t, s) { 0.3 }

#Heston process with r=0.05, c=0.02, spot=100, v0=0.09, kappa=1, theta=0.06, sigma=0.4 and rho=-0.75
process <- HestonProcess(function(t,s) {0.05}, function(t,s) {0.02},
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

params <- HestonSLVMCParams(TRUE, 90, 50, 10000)

# calibrate HestonSLV model for the next year
mcModel <- new (HestonSLVMCModel,
                Sys.Date(), Sys.Date()+365,
                localVol, process, params)

s <- seq(50, 150, 1)
plot(s, sapply(s, function(spot) { mcModel$leverageFunction(1.0, spot) }), type='l',
     ylab="Leverage Function", xlab="Spot", main="Leverage Function in One Year")}
\keyword{classes}
