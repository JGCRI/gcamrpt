#### Data modules for the end-use sectors group

#' Final Energy by Sector and Fuel Data Module
#'
#' Produce total final by sector and fuel by region.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.final_energy_sector_fuel <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Final energy by detailed end-use sector and fuel'
    }
    else {
        ## silence notes on package checks
        sector <- input <- NULL

        message('Function for processing variable: Final Energy by detailed end-use sector and fuel')

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_fuel, by = 'input') %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector') %>%
            dplyr::mutate(sector = rname_final_energy(sector),
                          input = rname_final_energy(input))
        final_energy <- filter(final_energy, years, filters)
        final_energy <- aggregate(final_energy, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_energy(final_energy$Units[1], ounit)
            if(!is.na(cfac)) {
                final_energy$value <- final_energy$value *cfac
                final_energy$Units <- ounit
            }
        }
        final_energy
    }
}

# Rename sectors and inputs for final energy to have more meaningful names
rname_final_energy <- function(var) {
    var <- gsub('elect[\\s_].*', 'electricity ', var)
    var <- gsub('delivered\\s', '', var)
    var <- gsub('wholesale\\s', '', var)
    var <- gsub('regional\\s', '', var)
    var <- gsub('.*H2.*', 'hydrogen', var)
    var <- gsub('.*refined liquids.*', 'refined liquids', var)
    var <- gsub('comm\\s', 'commercial ', var)
    var <- gsub('resid\\s', 'residential ', var)
    var <- gsub('.*cement', 'cement', var)
    var <- gsub('.*aviation_intl', 'international aviation', var)
    var <- gsub('.*shipping_intl', 'international shipping', var)
    var <- gsub('.*freight.*', 'freight', var)
    var <- gsub('.*pass.*', 'passenger', var)
}
