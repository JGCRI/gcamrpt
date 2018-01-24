## Process Load Factor data from GCAM input data system


calc.load_factor <- function(filename) {
    # read in data
    load_factor <- readr::read_csv(filename) %>%
        dplyr::rename(sector = supplysector,
               subsector = tranSubsector,
               technology = stub.technology)

    load_factor

}
