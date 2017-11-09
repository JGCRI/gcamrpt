## Process Annual Mileage data from GCAM input data system


calc.annual_mileage <- function(filename) {
    # read in data
    annual_mileage <- readr::read_csv(filename)

    annual_mileage <- tidyr::gather(annual_mileage,
                                          key=year ,
                                          value=value,
                                          -region, -service, -mode, -submode, -size.class, -Units)
    annual_mileage <- annual_mileage %>%
        dplyr::group_by(service, mode, submode, year, Units) %>%
        dplyr::summarise(value=sum(value)) %>%
        dplyr::ungroup() %>% 
        dplyr::mutate(year = as.integer(year)) %>%
        dplyr::mutate(submode = gsub("LDV_", "", submode))
    
    annual_mileage
}
