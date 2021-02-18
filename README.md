# Precipr

Just a collection of some simple R functions that are useful to me 
    in manipulating precipitation, and for providing metrics of precipitation
    intensity. I will use these inside the rSFSTEP2 wrapper when running 
    simulations with increased intensity. Additionally, I've included a 
    slightly simplified version of the WGEN (Richardson and Wright 1984,
    Richardson, 1981) weather generator. This allows for altering the precipitation
    distribution (gamma distribution), and rain frequency, to but keep total 
    precip the same.. 

## Install

`devtools::install_github("MartinHoldrege/precipr")`
