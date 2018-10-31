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

module.population <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Population'
    }
    else {
        message('Function for processing variable: Population')

        population <- allqueries$'Population'
        population <- filter(population, years, filters, filter_operator)
        population <- aggregate(population, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_counts(population$Units[1], ounit)
            if(!is.na(cfac)) {
                population$value <- population$value *cfac
                population$Units <- ounit
            }
        }
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
module.gdp_mer_ <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'GDP(MER)'
    }
    else {
        ## silence notes on package check
        value <- NULL

        message('Function for processing variable: GDP MER')

        gdp_mer <- allqueries$'GDP(MER)'
        gdp_mer <- filter(gdp_mer, years, filters, filter_operator)
        gdp_mer <- aggregate(gdp_mer, aggfn, aggkeys)
        if(grepl('US', ounit)) {
            cf1 <- unitconv_counts(gdp_mer$Units[1], ounit)
            cf2 <- unitconv_usdollar(gdp_mer$Units[1], ounit)
            gdp_mer <- dplyr::mutate(gdp_mer, value=value*cf1*cf2, Units=ounit)
        }
        else if (grepl('Rupee', ounit)) {
            ## This is hacked together and needs to be fixed.
            gdp_mer <- unitconv_rupee(gdp_mer, ounit)
        }

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
module.pcgdp_ppp_ <- function(mode, allqueries, aggkeys, aggfn, years,
                           filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'pcGDP(PPP)'
    }
    else {
        ## silence notes on package check
        value <- NULL

        message('Function for processing variable: Per capita GDP PPP')

        gdp_ppp <- allqueries$'pcGDP(PPP)'
        gdp_ppp <- filter(gdp_ppp, years, filters, filter_operator)
        gdp_ppp <- aggregate(gdp_ppp, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cf <- unitconv_usdollar(gdp_ppp$Units, ounit)
            if(!any(is.na(cf))) {
                gdp_ppp <- dplyr::mutate(gdp_ppp, value=value*cf, Units=ounit)
            }
        }
        gdp_ppp
    }
}

