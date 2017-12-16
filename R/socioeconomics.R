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
        population <- filter(population, years, filters)
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
                              filters, ounit)
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
        gdp_mer <- filter(gdp_mer, years, filters)
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
                           filters, ounit)
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
        gdp_ppp <- filter(gdp_ppp, years, filters)
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

#' Population Growth Data Module
#'
#' Produce population growth by region.
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

module.population_growth <- function(mode, allqueries, aggkeys, aggfn, years,
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
        population <- filter(population, years, filters)
        population <- aggregate(population, aggfn, aggkeys)

        population_growth <- population %>%
            dplyr::group_by(scenario, region) %>%
            # Calculate annual growth: (value / prev_value)^(1 / year_diff) - 1
            dplyr::mutate(value = (value / dplyr::lag(value, n = 1L, order_by = year)) ^
                              (1 / (year - dplyr::lag(year, n = 1L, order_by = year))),
                          # Convert to annual percent growth
                          value = 100 * (value - 1),
                          # Change Units here
                          Units = "Annual Percentage Growth")

        if(!is.na(ounit)) {
            cfac <- unitconv_counts(population_growth$Units[1], ounit)
            if(!is.na(cfac)) {
                population_growth$value <- population_growth$value *cfac
                population_growth$Units <- ounit
            }
        }
        population_growth
    }
}

#' GDP PPP Data Module
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
module.gdp_ppp <- function(mode, allqueries, aggkeys, aggfn, years,
                            filters, ounit)
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
        gdp_mer <- filter(gdp_mer, years, filters)
        gdp_mer <- aggregate(gdp_mer, aggfn, aggkeys)
        if(grepl('US', ounit)) {
            cf <- unitconv_usdollar(gdp_mer$Units, ounit)
            gdp_mer <- dplyr::mutate(gdp_mer, value=value*cf, Units=ounit)
        }
        else if (grepl('Rupee', ounit)) {
            ## This is hacked together and needs to be fixed.
            gdp_mer <- unitconv_rupee(gdp_mer, ounit)
        }

        gdp_mer
    }
}
