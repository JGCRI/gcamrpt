## Process Annual Mileage data from GCAM input data system


calc.annual_mileage <- function(filename) {
    # read in data
    annual_mileage <- readr::read_csv(filename)

    annual_mileage <- tidyr::gather(annual_mileage,
                                          key=year ,
                                          value=value,
                                          -service, -mode, -submode, -Units)
    annual_mileage$year <- as.integer(annual_mileage$year) # in order to join with other df's

    annual_mileage

}
