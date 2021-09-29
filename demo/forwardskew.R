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

library(RQuantLib)
library(RHestonSLV)
#library(parallel)

vol <- 0.3
localVol <- function(t, s) { vol }

params <- new ("HestonSLVFDMParams")
params["xGrid"] <- 401
params["vGrid"] <- 801
params["localVolEpsProb"] <- 1e-6
params["leverageFctPropEps"] <- 1.5e-5
params["vMin"] <-1e-6

strikes <- c(50, 70, 80, 90, 100, 110 ,125, 150, 175, 200)/100.0

impliedVols <- function(eta) {
  process <- HestonProcess(cf(0.05), cf(0.02),
                           100, 0.09, 1.0, 0.06, eta*0.4, -0.75)

  model <- new (HestonSLVFDMModel,
                Sys.Date(), Sys.Date()+2*365,
                localVol, process, params)

  initialize <- leverageFunction(model, TRUE)(0.1, process["spot"])

  sapply(strikes, function(strike) {
    type <- if (strike > 1.0) "call" else "put"

    hestonSLVForwardOptionPricer(
      Sys.Date(), strike, Sys.Date()+365, type,  Sys.Date() + 730,
      2000, process, leverageFunction(model, TRUE))$impliedVol
  })
}

#cl <- makeCluster(detectCores(), "FORK")

vols <- sapply(c(1.0, 0.5, 0.25), function(eta) {
  impliedVols(eta)
})

#stopCluster(cl)

iv100 <- vols[,1]*100
iv050 <- vols[,2]*100
iv025 <- vols[,3]*100
iv000 <- rep(vol, length(strikes))*100


plot(strikes,iv100,type='b',lty=2,ylab="Implied Forward Volatility (in %)",
     cex.lab=1.4, cex.main=1.5, xlab=expression(paste("Strike ", alpha)),
     main=expression(paste("Forward Starting Option: max(0, ",S["2y"], " - ",  alpha, "*", S["1y"],")")),
     pch=1, ylim=c(26, 38))

points(strikes,iv050,lty=2,type='b',pch=2)
points(strikes,iv025,lty=2,type='b',pch=3)
points(strikes,iv000,lty=2,type='b',pch=4)

legend(x=1.5, y=36,cex=1.42,legend=c(expression(paste(eta,"=1.00")), expression(paste(eta,"=0.50")),
                              expression(paste(eta,"=0.25")), expression(paste(eta,"=0.00"))), lty=2,pch=c(
                                1,2,3,4))



