#### Data modules for the primary energy group

#' Primary Energy (Direct Equivalent) Data Module
#'
#' Produce primary energy by fuel
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

module.primary_energy_direct <- function(mode, allqueries, aggkeys, aggfn, years,
                                         filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'Primary Energy Consumption (Direct Equivalent)'
    }
    else {
        message('Function for processing variable: Primary Energy')
        pe <- allqueries$'Primary Energy Consumption (Direct Equivalent)'
        pe <- filter(pe, years, filters, filter_operator)
        pe <- aggregate(pe, aggfn, aggkeys)
        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_energy(pe$Units[1], ounit)
            if(!is.na(cfac)) {
                pe$value <- pe$value * cfac
                pe$Units <- ounit
            }
        }
        pe
    }
}
