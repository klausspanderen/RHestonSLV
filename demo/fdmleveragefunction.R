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
library(lattice)

localVol <- function(t, s) { 0.3 }

process <- HestonProcess(cf(0.05), cf(0.02),
                         100, 0.3^2, 1.0, 0.06, 0.4, -0.75)

params <- new ("HestonSLVFDMParams")
params["xGrid"] <- 201
params["vGrid"] <- 301

fdmModel <- new (HestonSLVFDMModel,
              Sys.Date(), Sys.Date()+365*5,
              localVol, process, params)

times <- seq(0.01, 5, 0.12)
strikes <- seq(1.75, 7.25, 0.12)

g <- expand.grid(strikes=strikes, times=times)

g$vol <- unlist(as.list(
  sapply(times, function(time) {
    sapply(strikes, function(strike) { leverageValue(fdmModel, time, exp(strike), FALSE) })
  }) ))

g$vol[1] <- 0.3
g$vol[2] <- 2.75

wireframe(vol ~ strikes*times, g,
          ylab=list(label="Time", cex=1.5, rot=00),
          xlab=list(label="ln(S)",cex=1.5, rot=0),
          zlab=list("Leverage Function L(x,t)",rot=90, cex=1.5),
          drape=TRUE,
          col.regions=rainbow(1024,start=0.1, end=0.70,alpha=0.75),
          screen = list(z = 25, x = -55),
          scales=list(arrows=FALSE, cex=1, tck=0.8, font=1),
          zoom=0.95,
          main=list(label=paste("Fokker-Planck Forward Equation, \U03B7=",sprintf("%3.2f",1.0), sep=""),
                    cex=2.0, font=1),
          zlim=c(0.25, 2.75),
          colorkey=list(space="right", width=3, labels=list(cex=1.5), clim=range(c(0.1, 0.8)))
          )

