library(slv)
library(parallel)

vol <- 0.3
localVol <- function(t, s) { vol }

process <- HestonProcess(cf(0.05), cf(0.02),
                         100, 0.09, 1.0, 0.06, 0.4, -0.75)

params <- new ("HestonSLVFDMParams")
params["xGrid"] <- 401
params["vGrid"] <- 801
params["localVolEpsProb"] <- 1e-6
params["leverageFctPropEps"] <- 1.5e-5
params["vMin"] <-1e-6

mcParams <- HestonSLVMCParams(TRUE, 180, 400, 80000)

modelmc <- new (HestonSLVMCModel,
                Sys.Date(), Sys.Date()+365,
                localVol, process, mcParams)

modelfd <- new (HestonSLVFDMModel,
                 Sys.Date(), Sys.Date()+365,
                 localVol, process, params)

initialize <- sapply(c(modelmc, modelfd), function(model) {
  leverageFunction(model, TRUE)(0.1, process["spot"])
})

impliedVol <- function(strike, maturity, model) {
  days <- floor(maturity*365)
  type = if (strike > process["spot"]) "call" else "put"

  hestonSLVOptionPricer(
    Sys.Date(), strike, type, "european", Sys.Date()+days, process,
    leverageFunction(model, TRUE), max(31, maturity*51), 401, 101, 0,
    "ModifiedCraigSneyd", TRUE)$impliedVol
}

strikes <- seq(50, 250, 10)

cl <- makeCluster(detectCores(), "FORK")

vfd <-parSapply(cl=cl, strikes, function(strike) {
      impliedVol(strike, 1.0, modelfd) })

vmc <-parSapply(cl=cl, strikes, function(strike) {
  impliedVol(strike, 1.0, modelmc) })

stopCluster(cl)

plot(strikes, vmc*100, main="Round-Trip Error for 1Y Maturity",
     xlab="Strike", ylab="Implied Volatility (in %)",
     type='b',ylim=c(29.9, 30.1), lty=2, cex.main=1.4,cex.lab=1.3)
points(strikes, 1e2*(vfd), type='b',pch=2,lty=2, cex.main=1.3)
legend(106, 30.09, legend=c("Monte-Carlo", "Fokker-Planck"), pch=c(1,2),lty=2,cex=1.32)
