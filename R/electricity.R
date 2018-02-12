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

        electricity_shares <- allqueries$'Electricity' %>%
            # Aggregate by subsector
            dplyr::group_by(Units, scenario, region, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(elec_fuel_type, by = 'subsector') %>%
            # Rearrange columns for climateworks format
            dplyr::select(Units, scenario, region, fuel_type, subsector, year, value)

        electricity_shares <- filter(electricity_shares, years, filters)
        electricity_shares <- aggregate(electricity_shares, aggfn, aggkeys)

        grouping_vars <- names(electricity_shares)[!(names(electricity_shares) %in%
                                                         c('subsector', 'value', 'fuel_type'))]
        electricity_shares <- electricity_shares %>%
            dplyr::group_by_(.dots = grouping_vars) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = '% share')


        electricity_shares
    }
}

#### Data modules for the electricity queries group

#' Electricity Capacity Data Module
#'
#' Produce electricity capacity by region.
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

module.electricity_capacity <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Electricity'
    }
    else {
        message('Function for processing variable: Electricity Capacity')

        electricity <- allqueries$'Electricity' %>%
            # Add in capacity factor---inner_join drops hydrogen and cogen technologies
            dplyr::inner_join(elec_capacity_factors, by = c('region', 'technology')) %>%
            # To convert to capacity, divide by seconds in a year and capacity factor
            dplyr::mutate(value = 1e9 * value / (365 * 24 * 60 * 60 * capacity.factor),
                          Units = 'GW') %>%
            dplyr::select(-capacity.factor) %>%
            dplyr::left_join(elec_fuel_type, by = 'subsector')
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

#' Electricity Per Capita Data Module
#'
#' Produce electricity per capita by region.
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

module.electricity_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('Electricity', 'Population')
    }
    else {
        message('Function for processing variable: Electricity')

        electricity <- allqueries$'Electricity' %>%
            dplyr::select(-rundate)
        # Aggregating here allows us to aggregate by region, sector, subsector, technology
        # Need to be careful about adding in population if we aggregate globally though
        electricity <- aggregate(electricity, aggfn, aggkeys)

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)

        # If aggkeys are provided and are being summed globally, then we need to get the global population here
        if (!is.na(aggkeys) & !grepl('region', aggkeys)){
            pop <- aggregate(pop, aggfn, 'scenario')
        }

        elec_capita <- electricity %>%
            dplyr::left_join(pop, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        elec_capita <- filter(elec_capita, years, filters)


        if(is.na(ounit))
            return(elec_capita)

        iunit <- elec_capita$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_energy(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(elec_capita, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            elec_capita
        }
    }
}
