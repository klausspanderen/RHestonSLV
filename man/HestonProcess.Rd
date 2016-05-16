\name{HestonProcess}
\Rdversion{1.1}
\docType{class}
\alias{HestonProcess}

\title{Class \code{"HestonProcess"}}
\description{
The Heston process is give by
\deqn{
\begin{array}{ll}
dS_t =  (r_t-q_t)S_tdt + \sqrt{\nu_t}S_tdW_t^S \\
d\nu_t =  \kappa(\theta-\nu_t)dt+\sigma\sqrt{\nu_t}dW_t^\nu \\
\rho dt = dW_t^S dW_t^\nu
\end{array}
}
}
\section{Objects from the Class}{
An object represents the paramter of a Heston process.
Objects can be created by calls of the form \code{new("HestonProcess", ...)} or
\code{HestonProcess(r, q, spot, v0, kappa, theta, sigma, rho)}.
}
\section{Slots}{
  \describe{
    \item{\code{r}:}{domestic interest rate as a function of time (continuous compounding, Actual365Fixed) }
    \item{\code{q}:}{divident rate or foreign interest rate as a function of time (continuous compounding, Actual365Fixed) }
    \item{\code{spot}:}{current price of the underlying stock or fx rate }
    \item{\code{v0}:}{spot variance \eqn{\nu_{t=0}}}
    \item{\code{kappa}:}{rate at which \eqn{\nu_t} reverts to \eqn{\theta} }
    \item{\code{theta}:}{mean reversion level of the variance \eqn{\nu_t}}
    \item{\code{sigma}:}{volatility of volatility}
    \item{\code{rho}:}{correlation between spot and variance increments }
  }
}
\examples{
> process <- HestonProcess(function(t) { 0.02 },
                           function(t) { 0.01 },
                           100, 0.09, 2.0, 0.06, 0.4, -0.75)

> process
HestonProcess
  r(t=0):  0.02
  q(t=0):  0.01
  spot  :  100
  v0    :  0.09
  kappa :  2
  theta :  0.06
  sigma :  0.4
  rho   :  -0.75

> process["rho"]
[1] -0.75
> process["sigma"] <- 0.2
> process["sigma"]
[1] 0.2
> process["sigma"] <- -0.2
 Error in validObject(x) :
   invalid class "HestonProcess" object: negative sigma was given.
> process["rho"] <- 2
 Error in validObject(x) :
   invalid class "HestonProcess" object: correlation rho must stay between [-1,1].

}

\keyword{classes}
