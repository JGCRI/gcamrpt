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

        # Add CCS to subsector
        elct <- allqueries$Electricity %>%
                dplyr::mutate(subsector = sub('rooftop_pv', 'solar', subsector),
                              subsector = paste0(subsector,
                                                 dplyr::if_else(grepl('CCS', technology), ' CCS', '')))
        elct <- filter(elct, years, filters)
        elct <- aggregate(elct, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_energy(elct$Units[1], ounit)
            if(!is.na(cfac)) {
                elct$value <- elct$value * cfac
                elct$Units <- ounit
            }
        }
        elct
    }
}
