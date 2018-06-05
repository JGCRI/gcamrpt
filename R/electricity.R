#### Data modules for the electricity queries group

#' Electricity Data Module
#'
#' Produce electricity by region.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{subsector}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.electricity <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Electricity'
    }
    else {
        message('Function for processing variable: Electricity')

        electricity <- allqueries$'Electricity'
        electricity <- filter(electricity, years, filters, filter_operator)
        electricity <- aggregate(electricity, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_energy(electricity$Units[1], ounit)
            if(!is.na(cfac)) {
                electricity$value <- electricity$value * cfac
                electricity$Units <- ounit
            }
        }
        electricity
    }
}
