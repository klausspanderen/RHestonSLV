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

library(RHestonSLV)
library(parallel)
library(RQuantLib)

vol <- 0.3
localVol <- function(t, s) { vol }

params <- new ("HestonSLVFDMParams")
params["xGrid"] <- 401
params["vGrid"] <- 801
params["localVolEpsProb"] <- 1e-6
params["leverageFctPropEps"] <- 1.5e-5
params["vMin"] <- 1e-6

barriers <- seq(0, 100, 5)

slvBarrierDelta <- function(eta) {
  process <- HestonProcess(cf(0.05), cf(0.02),
                           100, 0.09, 1.0, 0.06, eta * 0.4,-0.75)
  
  model <- new (HestonSLVFDMModel,
                Sys.Date(),
                Sys.Date() + 1095,
                localVol,
                process,
                params)
  
  initialize <- leverageFunction(model, TRUE)(0.1, process["spot"])
  
  sapply(barriers, function(barrier) {
    hestonSLVBarrierPricer(
      Sys.Date(),
      max(0.001, barrier),
      0.0,
      "DownOut",
      process["spot"],
      "Put",
      Sys.Date() + 1095,
      process,
      leverageFunction(model, TRUE),
      201, 801, 201, 0, "ModifiedCraigSneyd"
    )$delta
  })
}

cl <- makeCluster(detectCores(), "FORK")

deltas <- parSapply(cl=cl, c(1.0,0.5,0.25, 0.1), function(eta) {
  slvBarrierDelta(eta)
})

stopCluster(cl)

lvBarrierDelta <- sapply(barriers, function(barrier) {
  barrierPrice <- function(underlying, barrier) { 
    BarrierOption(
      "downout", "put",
      underlying=underlying,
      strike=100,
      dividendYield=0.02,
      riskFreeRate=0.05,
      maturity=3.0,
      volatility=0.3,
      barrier=max(0.001, barrier))$value
  }
  
  10*(barrierPrice(100.1, barrier)-barrierPrice(100.0, barrier))
})

s1 <- spline(barriers, lvBarrierDelta-deltas[,1], n=100, method="natural")
s2 <- spline(barriers, lvBarrierDelta-deltas[,2], n=100, method="natural")
s3 <- spline(barriers, lvBarrierDelta-deltas[,3], n=100, method="natural")
s4 <- spline(barriers, lvBarrierDelta-deltas[,4], n=100, method="natural")

par(mar=c(5.1,4.6,4.1,2.1))

plot(s1$x, s1$y, type='l', ,xlim=c(0,100), xlab="Barrier", 
    ylab=expression(paste('\U0394'[local], ' - \U0394'[SLV])),
    main=expression(paste('Barrier Option \U0394'[local], ' vs \U0394'[SLV])),lty=2,
    cex.main=1.4,cex.lab=1.3)

points(s2$x, s2$y, type='l', lty=2)
points(s3$x, s3$y, type='l', lty=2)
points(s4$x, s4$y, type='l', lty=2)

points(barriers, lvBarrierDelta-deltas[,1],pch=1)
points(barriers, lvBarrierDelta-deltas[,2],pch=2)
points(barriers, lvBarrierDelta-deltas[,3],pch=4)
points(barriers, lvBarrierDelta-deltas[,4],pch=5)

legend(x=70, y=-0.025, legend=c("\U03B7 = 1.0", "\U03B7 = 0.5", "\U03B7 = 0.2", "\U03B7 = 0.1"),
       lty=2, pch=c(1,2,4,5), seg.len=3,cex=1.32)
