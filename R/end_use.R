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
                              filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Final energy by detailed end-use sector and fuel'
    }
    else {
        message('Function for processing variable: Final Energy by detailed end-use sector and fuel')

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_fuel, by = 'input') %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector')
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

#' Building Floorspace Module
#'
#' Produce building floorspace by region.
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

module.building_floorspace <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Building floorspace'
    }
    else {
        message('Function for processing variable: Building floorspace')

        floorspace <- allqueries$'Building floorspace' %>%
            dplyr::select(-nodeinput, -`building-node-input`)
        floorspace <- filter(floorspace, years, filters)
        floorspace <- aggregate(floorspace, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cfac <- unitconv_counts(floorspace$Units[1], ounit)
            if(!is.na(cfac)) {
                floorspace$value <- floorspace$value *cfac
                floorspace$Units <- ounit
            }
        }
        floorspace
    }
}

#' Cement Production Module
#'
#' Produce cement production by region.
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

module.cement_production <- function(mode, allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Cement production by region'
    }
    else {
        message('Function for processing variable: Cement production')

        cement <- allqueries$'Cement production by region'
        cement <- filter(cement, years, filters)
        cement <- aggregate(cement, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cfac <- unitconv_counts(cement$Units[1], ounit)
            if(!is.na(cfac)) {
                cement$value <- cement$value *cfac
                cement$Units <- ounit
            }
        }
        cement
    }
}
