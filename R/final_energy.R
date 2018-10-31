#### Data modules for the emissions group

#' Final Energy Data Module

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

module.final_energy <- function(mode, allqueries, aggkeys, aggfn, years,
                                              filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Total final energy by detailed end-use sector'
    }
    else {
        message('Function for processing variable: Total final energy by detailed end-use sector')
        final_e <- allqueries$'Total final energy by detailed end-use sector'
        final_e <- filter(final_e, years, filters, filter_operator)
        final_e <- aggregate(final_e, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_energy(final_e$Units[1], ounit)
            if(!is.na(cfac)) {
                final_e$value <- final_e$value * cfac
                final_e$Units <- ounit
            }
        }

        final_e
    }
}
