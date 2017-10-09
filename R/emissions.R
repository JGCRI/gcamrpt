#### Data modules for the emissions group

#' CO2 emissions Data Module
#'
#' Produce service output by technology and vintage
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

module.co2_emissions <- function(mode, allqueries, aggkeys, aggfn, years,
                                  filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'CO2 emissions'
    }
    else {
        message('Function for processing variable: CO2 emissions')
        co2 <- allqueries$'CO2 emissions'
        co2 <- semiaggregate(co2)
        co2 <- parse_sector(co2, hasvintage=FALSE, hasfuel=FALSE)
        co2 <- filter(co2, years, filters)
        co2 <- aggregate(co2, aggfn, aggkeys)
        co2 <- unitconv_counts(co2, ounit)
        co2
    }
}
