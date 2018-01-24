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
        # Select vars to group by in case user has aggregated globally
        grp_vars <- names(population)[!(names(population) %in%
                                         c('Units', 'year', 'value', 'rundate'))]
        population_growth <- population %>%
            dplyr::group_by_(.dots = grp_vars) %>%
            # Calculate annual growth: (value / prev_value)^(1 / year_diff) - 1
            dplyr::mutate(value = (value / dplyr::lag(value, n = 1L, order_by = year)) ^
                              (1 / (year - dplyr::lag(year, n = 1L, order_by = year))),
                          # Convert to annual percent growth
                          value = 100 * (value - 1),
                          # Change Units here
                          Units = "Annual Percentage Growth") %>%
            dplyr::ungroup()

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
            cf <- unitconv_counts(gdp_mer$Units[1], ounit) *
                unitconv_usdollar(gdp_mer$Units[1], ounit)
            gdp_mer <- dplyr::mutate(gdp_mer, value=value*cf, Units=ounit)
        }
        else if (grepl('Rupee', ounit)) {
            ## This is hacked together and needs to be fixed.
            gdp_mer <- unitconv_counts(gdp_mer$Units[1], ounit) *
                unitconv_rupee(gdp_mer$Units[1], ounit)
        }

        gdp_mer
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
        c('pcGDP(PPP)', 'Population')
    }
    else {
        message('Function for processing variable: GDP PPP')

        gdp_ppp_pc <- allqueries$'pcGDP(PPP)'
        pop <- allqueries$'Population'
        gdp_ppp <- gdp_ppp_pc %>%
            dplyr::left_join(pop, by = c('scenario', 'region', 'year', 'rundate')) %>%
            dplyr::mutate(value = value.x * value.y * 1000,
                   Units = substr(Units.x, 1, regexpr('/', Units.x)[1]-1)) %>%
            dplyr::select(Units, scenario, region, year, value, rundate)
        gdp_ppp <- filter(gdp_ppp, years, filters)
        gdp_ppp <- aggregate(gdp_ppp, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cf <- unitconv_counts(gdp_ppp$Units[1], ounit)*
                unitconv_usdollar(gdp_ppp$Units[1], ounit)
            if(!any(is.na(cf))) {
                gdp_ppp <- dplyr::mutate(gdp_ppp, value=value*cf, Units=ounit)
            }
        }
        gdp_ppp
    }
}

#' GDP PPP Growth Data Module
#'
#' Produce gdp ppp growth by region.
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

module.gdp_ppp_growth <- function(mode, allqueries, aggkeys, aggfn, years,
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('pcGDP(PPP)', 'Population')
    }
    else {
        message('Function for processing variable: GDP PPP Growth')

        gdp_ppp_pc <- allqueries$'pcGDP(PPP)'
        pop <- allqueries$'Population'
        gdp_ppp <- gdp_ppp_pc %>%
            dplyr::left_join(pop, by = c('scenario', 'region', 'year', 'rundate')) %>%
            dplyr::mutate(value = value.x * value.y * 1000,
                   Units = substr(Units.x, 1, regexpr('/', Units.x)[1]-1)) %>%
            dplyr::select(Units, scenario, region, year, value, rundate)
        gdp_ppp <- filter(gdp_ppp, years, filters)
        gdp_ppp <- aggregate(gdp_ppp, aggfn, aggkeys)
        # Select vars to group by in case user has aggregated globally
        grp_vars <- names(gdp_ppp)[!(names(gdp_ppp) %in%
                                       c('Units', 'year', 'value', 'rundate'))]

        gdp_ppp_growth <- gdp_ppp %>%
            dplyr::group_by_(.dots = grp_vars) %>%
            # Calculate annual growth: (value / prev_value)^(1 / year_diff) - 1
            dplyr::mutate(value = (value / dplyr::lag(value, n = 1L, order_by = year)) ^
                              (1 / (year - dplyr::lag(year, n = 1L, order_by = year))),
                          # Convert to annual percent growth
                          value = 100 * (value - 1),
                          # Change Units here
                          Units = "Annual Percentage Growth") %>%
            dplyr::ungroup()

        gdp_ppp_growth
    }
}
