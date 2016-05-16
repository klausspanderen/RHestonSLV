\name{HestonSLVFDMParams}
\Rdversion{1.1}
\docType{class}
\alias{HestonSLVFDMParams}

\title{Class \code{"HestonSLVFDMParams"}}
\description{
Defines the parameter for a calibration of the Heston Stochastic Local Volatility model
via the Fokker-Planck forward equation. }
\section{Objects from the Class}{
An object of this class defines the parameter for a calibration	based
on the Fokker-Planck equation via Finite Difference methods.
Objects can be created by calls of the form
\code{new("HestonSLVFDMParams", ...)} or
\code{HestonSLVFDMParams(...)}.
}
\section{Slots}{
  \describe{
    \item{\code{xGrid}:}{number of the grid points in spot direction }
    \item{\code{vGrid}:}{number of the grid points in variance direction }
    \item{\code{tMaxStepsPerYear}:}{maximum number of time steps per year }
    \item{\code{tMinStepsPerYear}:}{minimum number of time steps per year }
    \item{\code{tStepNumberDecay}:}{rate of decay for time steps per year }
    \item{\code{predictionCorrectionSteps}:}{number of prediction/correction steps }
    \item{\code{x0Density}:}{density factor at origin for mesher in spot direction }
    \item{\code{localVolEpsProb}:}{ mesher stopping condition in spot direction }
    \item{\code{maxIntegrationIterations}:}{ maximum number of integration steps }
    \item{\code{vLowerEps}:}{mesher stopping condition in variance direction for the lower bound}
    \item{\code{vUpperEps}:}{mesher stopping condition in variance direction for the upper bound}
    \item{\code{vMin}:}{lower bound for the mesher in variance direction }
    \item{\code{v0Density}:}{density factor for mesher in variance direction at the origin }
    \item{\code{vLowerBoundDensity}:}{density factor for mesher in variance direction at the lower bound }
    \item{\code{vUpperBoundDensity}:}{density factor for mesher in variance direction at the upper bound }
    \item{\code{leverageFctPropEps}:}{extrapolate leverage function if probability is below this value }
    \item{\code{greensAlgorithm}:}{algorithm for the approximation of the Greens function at t=0. (\dQuote{ZeroCorrelation}, \dQuote{Gaussian} or \dQuote{SemiAnalytical})  }
    \item{\code{transformationType}:}{coordinate transformation in spot direction (\dQuote{Plain}, \dQuote{Power} or \dQuote{Log})}
    \item{\code{fdmSchemeType}:}{discretization scheme in time direction (\dQuote{Hundsdorfer}, \dQuote{ModifiedHundsdorfer}, \dQuote{Douglas}, \dQuote{CraigSneyd}, \dQuote{ModifiedCraigSneyd}, \dQuote{ImplicitEuler} or \dQuote{ExplicitEuler})}
  }
}
\references{
Johannes Goettker-Schnetmann, Klaus Spanderen \url{http://hpc-quantlib.de/src/slv.pdf}
Calibrating the Heston Stochastic Local Volatility Model using the Fokker-Planck Equation
}

\examples{
> params <- new ("HestonSLVFDMParams")
> params
HestonSLVFDMParams
  xGrid                    :  301
  vGrid                    :  601
  tMaxStepsPerYear         :  2000
  tMinStepsPerYear         :  30
  tStepNumberDecay         :  2
  predictionCorrectionSteps:  2
  x0Density                :  0.1
  localVolEpsProb          :  1e-04
  maxIntegrationIterations :  10000
  vLowerEps                :  1e-05
  vUpperEps                :  1e-05
  vMin                     :  2.5e-06
  v0Density                :  1
  vLowerBoundDensity       :  0.1
  vUpperBoundDensity       :  0.9
  leverageFctPropEps       :  1e-05
  greensAlgorithm          :  Gaussian
  transformationType       :  Log
  fdmSchemeType            :  ModifiedCraigSneyd

> params["fdmSchemeType"] <- "Hundsdorfer"
> params["localVolEpsProb"] <- 1e-6
> params["greensAlgorithm"] <- "ZeroCorrelation"
> params["greensAlgorithm"]
[1] "ZeroCorrelation"
}
\keyword{classes}
