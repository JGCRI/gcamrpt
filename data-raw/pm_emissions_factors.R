## Process PM Emissions Factors data from ICCT Global Transportation Roadmap Model


calc.pm_emissions_factors <- function(filename) {
    # read in data
    pm_emissions_factors <- readr::read_csv(filename)

    pm_emissions_factors <- tidyr::gather(pm_emissions_factors,
                                          key=year ,
                                          value=value,
                                          -service, -mode, -submode, -Units)
    pm_emissions_factors$year <- as.integer(pm_emissions_factors$year) # in order to join with other df's

    pm_emissions_factors

}
