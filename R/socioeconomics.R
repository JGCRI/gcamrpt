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
                              filters, ounit, region, agg_region, add_global)
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
        population <- region_agg(population, region, agg_region, add_global)


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
                              filters, ounit, region, agg_region, add_global)
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
        gdp_mer <- region_agg(gdp_mer, region, agg_region, add_global)
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
                           filters, ounit, region, agg_region, add_global)
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
                              filters, ounit, region, agg_region, add_global)
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
        population <- region_agg(population, region, agg_region, add_global)
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
                              filters, ounit, region, agg_region, add_global)
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
        gdp_ppp <- region_agg(gdp_ppp, region, agg_region, add_global)

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
                                     filters, ounit, region, agg_region, add_global)
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
        gdp_ppp <- region_agg(gdp_ppp, region, agg_region, add_global)

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

module.agprod_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                                      filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('ag production by crop type', 'Population')
    }
    else {
        message('Function for processing variable: Ag production per capita')

        agprod <- allqueries$'ag production by crop type' %>%
            dplyr::select(-rundate)
        # Aggregating here allows us to aggregate by region, sector, subsector, technology
        # Need to be careful about adding in population if we aggregate globally though
        agprod <- aggregate(agprod, aggfn, aggkeys)
        agprod <- region_agg(agprod, region, agg_region, add_global)

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)

        # If aggkeys are provided and are being summed globally, then we need to get the global population here
        if (!is.na(aggkeys) & !grepl('region', aggkeys)){
            pop <- aggregate(pop, aggfn, 'scenario')
        }
        pop <- region_agg(pop, region, agg_region, add_global)

        agprod_capita <- agprod %>%
            dplyr::left_join(pop, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        agprod_capita <- filter(agprod_capita, years, filters)


        if(is.na(ounit))
            return(agprod_capita)

        iunit <- agprod_capita$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(agprod_capita, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            agprod_capita
        }
    }
}
