## Generate the internal data for the package
## This must be sourced as a script because of the way devtools::use_data works.
## Source it from the top level of a development copy of the package.


source('data-raw/gdpdef.R')
gdpdef <- calc.gdpdef('data-raw/GDPDEF.csv')

source('data-raw/pm_emissions_factors.R')
pm_emissions_factors <- calc.pm_emissions_factors('data-raw/pm_emissions_factors.csv')

source('data-raw/annual_mileage.R')
annual_mileage <- calc.annual_mileage('data-raw/annual_mileage.csv')

source('data-raw/energyconv.R')
energyconv <- prep.energyconv()

source('data-raw/countconv.R')
countconv <- prep.countconv()

source('data-raw/emissionsconv.R')
emissionsconv <- prep.emissionsconv()

source('data-raw/weightconv.R')
massconv <- prep.weightconv()

gwp_ar4 <- readr::read_csv('data-raw/GWP_AR4.csv')
gwp_ar5 <- readr::read_csv('data-raw/GWP_AR5.csv')

devtools::use_data(gdpdef, pm_emissions_factors, annual_mileage,
                   energyconv, countconv, emissionsconv, massconv,
                   gwp_ar4, gwp_ar5,
                   internal=TRUE, overwrite=TRUE,
                   compress='xz')

