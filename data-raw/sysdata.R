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

source('data-raw/load_factor.R')
load_factor <- calc.load_factor('data-raw/LoadFactor.csv')

gwp_ar4 <- readr::read_csv('data-raw/GWP_AR4.csv')
gwp_ar5 <- readr::read_csv('data-raw/GWP_AR5.csv')
ghg_gas_type <- readr::read_csv('data-raw/GHG_gas_type.csv')
ghg_sector <- readr::read_csv('data-raw/ghg_sector.csv')
ghg_subsector <- readr::read_csv('data-raw/ghg_subsector.csv')

final_energy_fuel <- readr::read_csv('data-raw/final_energy_fuel.csv')
final_energy_end_use_sector <- readr::read_csv('data-raw/final_energy_end_use_sector.csv')

elec_fuel_type <- readr::read_csv('data-raw/electricity_fuel_type.csv')
primary_fuel_type <- readr::read_csv('data-raw/primary_fuel_type.csv')
elec_capacity_factors <- readr::read_csv('data-raw/elec_capacity_factors.csv')
trn_subsector_map <- readr::read_csv('data-raw/trn_subsector_map.csv')

devtools::use_data(gdpdef, pm_emissions_factors, annual_mileage,
                   energyconv, countconv, emissionsconv, massconv,
                   gwp_ar4, gwp_ar5, ghg_gas_type, final_energy_fuel,
                   final_energy_end_use_sector, elec_fuel_type, primary_fuel_type,
                   elec_capacity_factors, load_factor, ghg_sector,
                   internal=TRUE, overwrite=TRUE,
                   compress='xz')

