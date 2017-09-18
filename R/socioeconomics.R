#### Data modules for the socioeconomics group

#' Population Data Module
#'
#' Produce population by region.
#'
#' Columns:
#' \itemize{
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{units}
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
        population <- aggregate(population, aggfn, aggkeys)
        population <- filter(population, strtyr, endyr, filters)
        population <- unitconv_counts(population, ounit)
        population
    }
}

#' GDP MER Data Module
#'
#' Produce GDP MER by region.
#'
#' Columns:
#' \itemize{
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{units}
#' }
#'
#' @keywords internal

module.gdp_mer <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'GDP MER'
    }
    else {
        message('Function for processing variable: GDP MER')

        gdp_mer <- allqueries$'gdp_mer'
        gdp_mer <- aggregate(gdp_mer, aggfn, aggkeys)
        gdp_mer <- filter(gdp_mer, strtyr, endyr, filters)
        gdp_mer <- unitconv_counts(gdp_mer, ounit)
        gdp_mer
    }
}

#' GDP PPP Data Module
#'
#' Produce GDP PPP by region.
#'
#' Columns:
#' \itemize{
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{units}
#' }
#'
#' @keywords internal

module.gdp_ppp <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                           filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'GDP PPP'
    }
    else {
        message('Function for processing variable: GDP PPP')

        gdp_ppp <- allqueries$'gdp_ppp'
        gdp_ppp <- aggregate(gdp_ppp, aggfn, aggkeys)
        gdp_ppp <- filter(gdp_ppp, strtyr, endyr, filters)
        gdp_ppp <- unitconv_counts(gdp_ppp, ounit)
        gdp_ppp
    }
}

