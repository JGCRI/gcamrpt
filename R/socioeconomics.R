#### Data modules for the socioeconomics group

#' Population Data Module
#'
#' Produce population by region.
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

module.population <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Population'
    }
    else {
        message('Function for processing variable: Population')

        population <- allqueries$'Population'
        population <- filter(population, strtyr, endyr, filters)
        population <- aggregate(population, aggfn, aggkeys)
        population <- unitconv_counts(population, ounit)
        population
    }
}

#' GDP MER Data Module
#'
#' Produce GDP MER by region.
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
module.gdp_mer_ <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'GDP(MER)'
    }
    else {
        message('Function for processing variable: GDP MER')

        gdp_mer <- allqueries$'GDP(MER)'
        gdp_mer <- filter(gdp_mer, strtyr, endyr, filters)
        gdp_mer <- aggregate(gdp_mer, aggfn, aggkeys)
        gdp_mer <- unitconv_usdollar(gdp_mer, ounit)
        gdp_mer
    }
}

#' Per capita GDP PPP Data Module
#'
#' Produce GDP PPP by region.
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
module.pcgdp_ppp_ <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                           filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'pcGDP(PPP)'
    }
    else {
        message('Function for processing variable: Per capita GDP PPP')

        gdp_ppp <- allqueries$'pcGDP(PPP)'
        gdp_ppp <- filter(gdp_ppp, strtyr, endyr, filters)
        gdp_ppp <- aggregate(gdp_ppp, aggfn, aggkeys)
        gdp_ppp <- unitconv_usdollar(gdp_ppp, ounit)
        gdp_ppp
    }
}

