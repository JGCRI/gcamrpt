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
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Electricity'
    }
    else {
        message('Function for processing variable: Electricity')

        electricity <- allqueries$'Electricity'
        electricity <- filter(electricity, years, filters)
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

#' Electricity Shares Data Module
#'
#' Produce electricity shares by region.
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

module.electricity_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Electricity'
    }
    else {
        message('Function for processing variable: Electricity Shares')

        electricity_shares <- allqueries$'Electricity'
        electricity_shares <- filter(electricity_shares, years, filters)
        electricity_shares <- aggregate(electricity_shares, aggfn, aggkeys)

        electricity_shares
    }
}
