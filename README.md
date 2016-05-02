## RHestonSLV

### About

The RHestonSLV package makes the implementation of the Heston
Stochastic Local Volatility model in QuantLib 
[QuantLib](https://github.com/lballabio/quantlib) visible to the R
user. 

Local Stochastic Volatility (LSV) models have become the industry standard
for FX and equity markets. The local volatility extension of the popular Heston 
stochastic volatility model is a promising candidate within the zoo of LSV models.
But the calibration of this model is not only computational demanding but also tricky 
from an algorithmic point of view, especially if the Feller constraint is violated 
The two main solutions to tackle the calibration problem are either solving the 
Fokker-Planck forward equation via finite difference methods or based on 
efficient Monte-Carlo simulations. Pricing and greek calculation for vanilla and more 
exotic options is also supported. 


### Authors

Klaus Spanderen

### License

GPL2

