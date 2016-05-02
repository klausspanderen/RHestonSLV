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

process <- HestonProcess(cf(0.02), cf(0.01),
                         100, 0.09, 1.0, 0.06, 0.8, -0.8)

implVol = hestonSLVOptionPricer(Sys.Date(), 100, "call", "european", Sys.Date()+365, process,
                                function(s, t) { 1.0 }, 101, 401, 101, 0, "ModifiedCraigSneyd")$impliedVol

lv <- new (HestonLocalVolSurface, Sys.Date(), Sys.Date()+365, process)

localVol <- function(t, s) { lv$localVol(t, s) }

params <- new ("HestonSLVFDMParams")
params["xGrid"] <- 401
params["vGrid"] <- 801
params["localVolEpsProb"] <- 1e-6
params["leverageFctPropEps"] <- 1.5e-5
params["vMin"] <- 1e-6

barriers <- seq(4, 100, 0.5)

slvDoubleBarrierPrices <- function(eta) {
  modProcess <- HestonProcess(cf(0.02), cf(0.01),
                              100, 0.09, 1.0, 0.06, eta * 0.8,-0.8)

  model <- new (HestonSLVFDMModel,
                Sys.Date(),
                Sys.Date() + 365,
                lovalVolFct,
                modProcess,
                params)
  initialize <- leverageFunction(model, TRUE)(0.1, process["spot"])

  sapply(barriers, function(barrier) {
    hestonSLVDoubleBarrierPricerNoTouchPrice(
      Sys.Date(),
      max(0.01, process["spot"] - barrier),
      process["spot"]  + barrier,
      0.0,
      "KnockOut",
      0.0,
      "Call",
      "CashOrNothing",
      Sys.Date() + 365,
      modProcess,
      leverageFunction(model, TRUE),
      101, 151, 51, 0, "ModifiedCraigSneyd"
    )$value
  })
}

cl <- makeCluster(detectCores(), "FORK")

#slvPrices <- parSapply(cl=cl, c(1.0, 0.75, 0.5, 0.25, 0.001), function(eta) {
#  slvDoubleBarrierPrices(eta)
#})

stopCluster(cl)

bsNPV <- unlist(sapply(barriers, function(barrier) {
  bsDoubleBarrierPricerNoTouchPrice(
    Sys.Date(),
    max(0.01, 100 - barrier),
    100  + barrier,
    0.0,
    "KnockOut",
    0.0,
    "Call",
    "CashOrNothing",
    Sys.Date() + 365,
    100,
    0.01, 0.02, implVol)
}))

par(mar=c(5.1,4.6,4.1,2.1))

plot(bsNPV, slvPrices[,1]-bsNPV,
     xlab=expression(paste('NPV'[BS])),
     ylab=expression(paste('NPV'[SLV], ' - NPV'[BS])),
     main=expression("Double No Touch Option"),
     type='l',ylim=c(-0.05, 0.16), cex.main=1.4,cex.lab=1.3)

mtext("Stochastic Local Volatility vs. Black-Scholes Prices", 3)

points(bsNPV, slvPrices[,2]-bsNPV,type='l',lty=2)
points(bsNPV, slvPrices[,3]-bsNPV,type='l',lty=3)
points(bsNPV, slvPrices[,4]-bsNPV,type='l',lty=4)
points(bsNPV, slvPrices[,5]-bsNPV,type='l',lty=5)

legend(0.7,0.14,legend=c(expression(paste(eta,"=1.00")), expression(paste(eta,"=0.75")),
                         expression(paste(eta,"=0.50")), expression(paste(eta,"=0.25")),
                         expression(paste(eta,"=0.00"))), lty=c(1,2,3,4,5), cex=1.32)

