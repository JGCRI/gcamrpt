## Process Annual Mileage data from GCAM input data system


calc.annual_mileage <- function(filename) {
    # read in data
    calc.annual_mileage <- readr::read_csv(filename)

    calc.annual_mileage <- tidyr::gather(calc.annual_mileage,
                                          key=year ,
                                          value=value,
                                          -service, -mode, -submode, -Units)
    calc.annual_mileage$year <- as.integer(calc.annual_mileage$year) # in order to join with other df's

    calc.annual_mileage

}
