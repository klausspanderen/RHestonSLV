#  Copyright (C) 2016 Klaus Spanderen
#
#  This file is part of RHestonSLV.
#
#  RHestonSLV is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, version 2 of the License
#
#  RHestonSLV is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with RHestonSLV.  If not, see <http:#www.gnu.org/licenses/>.

setClass("HestonProcess",
  slots=c(r    ="function",
          q    ="function",
          spot ="numeric",
          v0   ="numeric",
          kappa="numeric",
          theta="numeric",
          sigma="numeric",
          rho  ="numeric"),
  validity = function(object) {
    if (object@spot <= 0)
      return("negative or zero spot was given.")
    if (object@v0 < 0)
      return("negative v0 was given.")
    if (object@kappa < 0)
      return("negative kappa was given.")
    if (object@theta < 0)
      return("negative theta was given.")
    if (object@sigma < 0)
      return("negative sigma was given.")
    if (object@rho < -1 || object@rho > 1)
      return("correlation rho must stay between [-1,1}.")

    return(TRUE)
  }
)


HestonProcess <- function(r, q, spot, v0, kappa, theta, sigma, rho) {
  new (Class="HestonProcess",
       r=r, q=q, spot=spot, v0=v0, kappa=kappa, theta=theta, sigma=sigma, rho=rho)
}

setMethod(f="show",
  signature="HestonProcess",
  definition=function(object) {
    cat("HestonProcess\n")
    cat("  r(t=0): ", paste(object@r(0)),"\n")
    cat("  q(t=0): ", paste(object@q(0)),"\n")
    cat("  spot  : ", paste(object@spot),"\n")
    cat("  v0    : ", paste(object@v0),"\n")
    cat("  kappa : ", paste(object@kappa),"\n")
    cat("  theta : ", paste(object@theta),"\n")
    cat("  sigma : ", paste(object@sigma),"\n")
    cat("  rho   : ", paste(object@rho),"\n")
  }
)


setMethod(f="[",
  signature="HestonProcess",
  definition=function(x, i, j, drop=TRUE) {
    switch(i,
       r     = { return(x@r(0)) },
       q     = { return(x@q(0)) },
       spot  = { return(x@spot) },
       v0    = { return(x@v0)   },
       kappa = { return(x@kappa)},
       theta = { return(x@theta)},
       sigma = { return(x@sigma)},
       rho   = { return(x@rho)  }
    )
  }
)


setReplaceMethod(f="[",
  signature="HestonProcess",
  definition=function(x, i, j, value) {
    switch(i,
      r     = { x@r     <- value },
      q     = { x@q     <- value },
      spot  = { x@spot  <- value },
      v0    = { x@v0    <- value },
      kappa = { x@kappa <- value },
      theta = { x@theta <- value },
      sigma = { x@sigma <- value },
      rho   = { x@rho   <- value }
    )
    validObject(x)
    return(x)
  }
)


setClass("HestonSLVMCParams",
  slots=c(qmc              = "logical",
          timeStepsPerYear = "numeric",
          nBins            = "numeric",
          calibrationPaths = "numeric"
          ),
  prototype = list(
      qmc=TRUE,
      timeStepsPerYear = 181,
      nBins = 100,
      calibrationPaths=32768
  ),
  validity = function(object) {
    if (object@timeStepsPerYear <= 0)
      return("negative or zero time steps per year given.")
    if (object@nBins < 0)
      return("negative number of bins was given.")
    if (object@calibrationPaths < 0)
      return("negative number of calibration paths was given.")
      return(TRUE)
  }
)

HestonSLVMCParams <- function(qmc, timeStepsPerYear, nBins, calibrationPaths) {
  new (Class="HestonSLVMCParams",
       qmc=qmc, timeStepsPerYear=timeStepsPerYear,
       nBins=nBins, calibrationPaths=calibrationPaths)
}

setMethod(f="show",
  signature="HestonSLVMCParams",
  definition=function(object) {
    cat("HestonSLVMCParams\n")
    cat("  qmc             : ", paste(object@qmc),"\n")
    cat("  timeStepsPerYear: ", paste(object@timeStepsPerYear),"\n")
    cat("  nBins           : ", paste(object@nBins),"\n")
    cat("  calibrationPaths: ", paste(object@calibrationPaths),"\n")
  }
)

setMethod(f="[",
  signature="HestonSLVMCParams",
  definition=function(x, i, j, drop=TRUE) {
    switch(i,
           qmc = { return(x@qmc) },
           timeStepsPerYear = { return(x@timeStepsPerYear) },
           nBins = { return(x@nBIns) },
           calibrationPaths = { return(x@calibrationPaths)   }
    )
  }
)

setReplaceMethod(f="[",
  signature="HestonSLVMCParams",
  definition=function(x, i, j, value) {
    switch(i,
           qmc              = { x@qmc <- value },
           timeStepsPerYear = { x@timeStepsPerYear <- value },
           nBins            = { x@nBins <- value },
           calibrationPaths = { x@calibrationPaths <- value }
    )
    validObject(x)
    return(x)
  }
)


setClass("HestonSLVFDMParams",
   slots=c(xGrid= "numeric",
           vGrid = "numeric",
           tMaxStepsPerYear = "numeric",
           tMinStepsPerYear = "numeric",
           tStepNumberDecay = "numeric",
           predictionCorrectionSteps = "numeric",
           x0Density = "numeric",
           localVolEpsProb = "numeric",
           maxIntegrationIterations = "numeric",
           vLowerEps = "numeric",
           vUpperEps = "numeric",
           vMin = "numeric",
           v0Density = "numeric",
           vLowerBoundDensity = "numeric",
           vUpperBoundDensity = "numeric",
           leverageFctPropEps = "numeric",
           greensAlgorithm = "character",
           transformationType = "character",
           fdmSchemeType = "character"
   ),
   prototype = list(
     xGrid= 301,
     vGrid = 601,
     tMaxStepsPerYear = 2000,
     tMinStepsPerYear = 30,
     tStepNumberDecay = 2.0,
     predictionCorrectionSteps = 2,
     x0Density = 0.1,
     localVolEpsProb = 1e-4,
     maxIntegrationIterations = 10000,
     vLowerEps = 1e-5,
     vUpperEps = 1e-5,
     vMin = 0.0000025,
     v0Density = 1.0,
     vLowerBoundDensity = 0.1,
     vUpperBoundDensity = 0.9,
     leverageFctPropEps = 1e-5,
     greensAlgorithm = "Gaussian",
     transformationType = "Log",
     fdmSchemeType = "ModifiedCraigSneyd"
   )
)

HestonSLVFDMParams <- function(
  xGrid, vGrid, tMaxStepsPerYear, tMinStepsPerYear, tStepNumberDecay,
  predictionCorrectionSteps, x0Density, localVolEpsProb, maxIntegrationIterationms,
  vLowerEps, vUpperEps, vMin, v0Density, vLowerBoundDensity,
  vUpperBoundDensity, leverageFctPropEps, greensAlgorithm,
  transformationType, fdmSchemeType) {
  new (Class="HestonSLVFDMParams",
       xGrid=xGrid, vGrid=vGrid, tMaxStepsPerYear=tMaxStepsPerYear,
       tMinStepsPerYear=tMinStepsPerYear, tStepNumberDecay=tStepNumberDecay,
       predictionCorrectionSteps=predictionCorrectionSteps,
       x0Density=x0Density, localVolEpsProb=localVolEpsProb,
       maxIntegrationIterations=maxIntegrationIterations,
       vLowerEps=vLowerEps, vUpperEps=vUpperEps, vMin=vMin,
       v0Density=v0Density, vLowerBoundDensity=vLowerBoundDensity,
       vUpperBoundDensity=vUpperBoundDensity, leverageFctPropEps=leverageFctPropEps,
       greensAlgorithm=greensAlgorithm,transformationType=transformationType,
       fdmSchemeType=fdmSchemeType)
}

setReplaceMethod(f="[",
  signature="HestonSLVFDMParams",
  definition=function(x, i, j, value) {
    switch(i,
           xGrid = { x@xGrid <- value },
           vGrid = { x@vGrid <- value },
           tMaxStepsPerYear = { x@tMaxStepsPerYear <- value },
           tMinStepsPerYear = { x@tMinStepsPerYear <- value },
           tStepNumberDecay = { x@tStepNumberDecay <- value },
           predictionCorrectionSteps = { x@predictionCorrectionSteps <- value },
           x0Density = { x@x0Density <- value },
           localVolEpsProb = { x@localVolEpsProb <- value },
           maxIntegrationIterations = { x@maxIntegrationIterations <- value },
           vLowerEps = { x@vLowerEps <- value },
           vUpperEps = { x@vUpperEps <- value },
           vMin = { x@vMin <- value },
           v0Density = { x@v0Density <- value },
           vLowerBoundDensity = { x@vLowerBoundDensity <- value },
           vUpperBoundDensity = { x@vUpperBoundDensity <- value },
           leverageFctPropEps = { x@leverageFctPropEps <- value },
           greensAlgorithm = { x@greensAlgorithm <- value },
           transformationType = { x@transformationType <- value },
           fdmSchemeType = { x@fdmSchemeType <- value }
    )
    validObject(x)
    return(x)
  }
)


setMethod(f="show",
  signature="HestonSLVFDMParams",
  definition=function(object) {
    cat("HestonSLVFDMParams\n")
    cat("  xGrid                    : ", paste(object@xGrid),"\n")
    cat("  vGrid                    : ", paste(object@vGrid),"\n")
    cat("  tMaxStepsPerYear         : ", paste(object@tMaxStepsPerYear),"\n")
    cat("  tMinStepsPerYear         : ", paste(object@tMinStepsPerYear),"\n")
    cat("  tStepNumberDecay         : ", paste(object@tStepNumberDecay),"\n")
    cat("  predictionCorrectionSteps: ", paste(object@predictionCorrectionSteps),"\n")
    cat("  x0Density                : ", paste(object@x0Density),"\n")
    cat("  localVolEpsProb          : ", paste(object@localVolEpsProb),"\n")
    cat("  maxIntegrationIterations : ", paste(object@maxIntegrationIterations),"\n")
    cat("  vLowerEps                : ", paste(object@vLowerEps),"\n")
    cat("  vUpperEps                : ", paste(object@vUpperEps),"\n")
    cat("  vMin                     : ", paste(object@vMin),"\n")
    cat("  v0Density                : ", paste(object@v0Density),"\n")
    cat("  vLowerBoundDensity       : ", paste(object@vLowerBoundDensity),"\n")
    cat("  vUpperBoundDensity       : ", paste(object@vUpperBoundDensity),"\n")
    cat("  leverageFctPropEps       : ", paste(object@leverageFctPropEps),"\n")
    cat("  greensAlgorithm          : ", paste(object@greensAlgorithm),"\n")
    cat("  transformationType       : ", paste(object@transformationType),"\n")
    cat("  fdmSchemeType            : ", paste(object@fdmSchemeType),"\n")
  }
)

