\name{HestonLocalVolSurface}
\Rdversion{1.1}
\docType{class}
\alias{HestonLocalVolSurface}

\title{Class \code{"HestonLocalVolSurface"}}
\description{
Translates a Heston model into the corresponding local volatility surface with the same option prices.
}
\section{Objects from the Class}{
An instance of the class calculates the corresponding local volatility surface for a given the Heston model, which results in the same option prices.
Objects can be created by calls of the form \cr
\code{new("HestonLocalVolSurface", referenceDate, maxDate, hestonProcess)}
}

\arguments{
  \item{referenceDate}{a date setting the reference date for the calculation}
  \item{maxDate}{a date setting the end date of the calculation}
  \item{hestonProcess}{an object of the class HestonProcess defining the Heston model}
}


\examples{
#Heston process with r=0.05, c=0.02, spot=100, v0=0.09, kappa=1, theta=0.06, sigma=0.4 and rho=-0.75
process <- HestonProcess(function(t,s) {0.05}, function(t,s) {0.02},
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

hestonLocalVol <- new(HestonLocalVolSurface, Sys.Date(),Sys.Date()+365, process)

s <- seq(50, 200, 1)
plot(s, sapply(s, function(spot) { hestonLocalVol$localVol(1.0, spot) }), type='l',
     ylab="Local Volatility", xlab="Spot", main="Local Volatility in One Year")
     }
