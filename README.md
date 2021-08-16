# precipr

A collection of some R functions that are useful to me 
    in manipulating precipitation, and for calculating metrics of precipitation
    intensity. I use some of these functions inside the `rSFSTEP2` wrapper when running 
    simulations with increased intensity. Additionally, I've included a 
    slightly simplified version of the WGEN (Richardson and Wright 1984,
    Richardson, 1981) weather generator, which I have not ended up using in simulations 
    (it allows for altering the shape of the precipitation
    distribution (gamma distribution), and rain frequency, but keeps total 
    precip the same). 
    
    Disclaimer: Much of this code has not been extensively tested or is not full developed. 

## Install

`devtools::install_github("MartinHoldrege/precipr")`

## Functions

### `adjust_coeffs()`

The key function used in `rSFSTEP2` for adjusting coefficients for the weather generator is `adjust_coeffs()`. 

Example data set (need `rSOILWAT2` installed).
`data <- data.frame(rSOILWAT2::dbW_weatherData_to_dataframe(rSOILWAT2::weatherData))`

Calculate the coefficients for the markov weather generator.

`coeffs <- rSOILWAT2::dbW_estimate_WGen_coefs(data, imputation_type = "mean")`

Adjust the coefficients (in this example mean event size is doubled). 

`adjust_coeffs(coeffs, data, mean_mult = 2)`
