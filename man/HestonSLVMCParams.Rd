\name{HestonSLVMCParams}
\Rdversion{1.1}
\docType{class}
\alias{HestonSLVMCParams}

\title{Class \code{"HestonSLVMCParams"}}
\description{
Defines the parameter for a Monte-Carlo calibration of the Heston
Stochastic Local Volatility Model.
}
\section{Objects from the Class}{
An instance of the class defines the numerical parameter for a Monte-Carlo calibration.
Objects can be created by calls of the form \code{new("HestonSLVFDMParams", ...)} or \cr
\code{HestonSLVMCParams(qmc, timeStepsPerYear, nBins, calibrationPaths)}.
}
\section{Slots}{
  \describe{
    \item{\code{qmc}:}{logical, defines if Sobol Quasi Monte-Carlo numbers in conjunction with Brownian bridges should be used to draw the paths. }
    \item{\code{timeStepsPerYear}:}{number of time steps per year }
    \item{\code{nBins}:}{number of bins in ever time step}
    \item{\code{calibrationPaths}:}{number of Monte-Carlo paths to be used for the calibration }
  }
}


\references{
A.Stoep, L. Grzelak, C. Oosterlee, The Heston Stochastic-Local Volatility Model:
Efficient Monte Carlo Simulation,
\url{http://ta.twi.tudelft.nl/mf/users/oosterle/oosterlee/anton1.pdf}
}

\examples{
> params <- HestonSLVMCParams(TRUE, 90, 100, 32767)
> params
HestonSLVMCParams
  qmc             :  TRUE
  timeStepsPerYear:  90
  nBins           :  100
  calibrationPaths:  32767

> params["qmc"]
[1] TRUE
> params["nBins"] <- 200
> params["nBins"]
[1] 200
}
\keyword{classes}
