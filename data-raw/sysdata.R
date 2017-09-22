## Generate the internal data for the package
## This must be sourced as a script because of the way devtools::use_data works.
## Source it from the top level of a development copy of the package.


source('data-raw/gdpdef.R')
gdpdef <- calc.gdpdef('data-raw/GDPDEF.csv')

source('data-raw/energyconv.R')
energyconv <- prep.energyconv()

devtools::use_data(gdpdef, energyconv, internal=TRUE, overwrite=TRUE,
                   compress='xz')

