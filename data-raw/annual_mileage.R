## Process Annual Mileage data from GCAM input data system


calc.annual_mileage <- function(filename) {
    # read in data
    annual_mileage <- readr::read_csv(filename)

    annual_mileage <- tidyr::gather(annual_mileage,
                                          key=year ,
                                          value=value,
                                          -region, -service, -mode, -submode, -subsector, -Units)
    # annual_mileage <- annual_mileage %>%
    #     dplyr::group_by(region, service, mode, submode, year, Units) %>%
    #     dplyr::summarise(value=sum(value)) %>%
    #     dplyr::ungroup()
    annual_mileage$year <- as.integer(annual_mileage$year) # in order to join with other df's

    annual_mileage

}
