#### Data modules for the primary energy queries group

#' Resource Production Data Module
#'
#' Produce resource production by region.
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

module.resource_production <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Resource production'
    }
    else {
        message('Function for processing variable: Resource production')

        resources <- allqueries$'Resource production'
        resources <- filter(resources, years, filters)
        resources <- aggregate(resources, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_energy(resources$Units[1], ounit)
            if(!is.na(cfac)) {
                resources$value <- resources$value * cfac
                resources$Units <- ounit
            }
        }
        resources
    }
}

#' Primary Energy (Direct Equivalent) Data Module
#'
#' Produce primary energy by fuel
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

module.primary_energy_direct <- function(mode, allqueries, aggkeys, aggfn, years,
                                         filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'Primary Energy Consumption (Direct Equivalent)'
    }
    else {
        message('Function for processing variable: Primary Energy')
        pe <- allqueries$'Primary Energy Consumption (Direct Equivalent)'
        pe <- filter(pe, years, filters)
        pe <- aggregate(pe, aggfn, aggkeys)
        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_energy(pe$Units[1], ounit)
            if(!is.na(cfac)) {
                pe$value <- pe$value * cfac
                pe$Units <- ounit
            }
        }
        pe
    }
}

#' Resource Production Per Capita Data Module
#'
#' Produce resource production per capita by region.
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

module.resource_production_pc <- function(mode, allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('Resource production', 'Population')
    }
    else {
        message('Function for processing variable: Resource production per capita')

        resources <- allqueries$'Resource production'
        # Aggregating here allows us to sum globally
        resources <- aggregate(resources, aggfn, aggkeys)

        pop <- allqueries$'Population'
        # Aggregating here allows us to sum globally
        pop <- aggregate(pop, aggfn, aggkeys)

        join_cats <- names(pop)[!(names(pop) %in% c('value', 'Units'))]
        resource_pc <- resources %>%
            dplyr::left_join(pop, by = join_cats) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        resource_pc <- filter(resource_pc, years, filters)

        if(is.na(ounit))
            return(resource_pc)

        iunit <- resource_pc$Units[1]
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
            dplyr::mutate(resource_pc, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            resource_pc
        }
    }
}

#' Oil Production Type Share Data Module
#'
#' Produce oil production share by type by region.
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

module.oil_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                                          filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Resource production'
    }
    else {
        message('Function for processing variable: Oil Production Type')

        oil_shares <- allqueries$'Resource production' %>%
            dplyr::filter(grepl('oil', resource))
        # Aggregating here allows us to sum globally
        oil_shares <- aggregate(oil_shares, aggfn, aggkeys)

        grp_vars <- names(oil_shares)[!(names(oil_shares) %in% c('value', 'resource'))]
        oil_shares <- oil_shares %>%
            dplyr::group_by_(.dots = grp_vars) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "% of Oil Production")

        oil_shares <- filter(oil_shares, years, filters)

        oil_shares
    }
}

#' Primary Energy (Direct Equivalent) Data Module
#'
#' Produce primary energy by fuel
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

module.primary_energy_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                                         filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'Primary Energy Consumption (Direct Equivalent)'
    }
    else {
        message('Function for processing variable: Primary Energy')
        pe <- allqueries$'Primary Energy Consumption (Direct Equivalent)' %>%
            dplyr::left_join(primary_fuel_type, by = "fuel") %>%
            dplyr::select(-rundate)
        pe <- filter(pe, years, filters)
        pe <- aggregate(pe, aggfn, aggkeys)

        grp_cats <- names(pe)[!(names(pe) %in% c('fuel', 'fuel_type', 'value'))]

        pe_shares <- pe %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage')

        pe_shares
    }
}

#' Primary Energy (Direct Equivalent) Per Capita Data Module
#'
#' Produce primary energy per capita by fuel
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

module.primary_energy_direct_pc <- function(mode, allqueries, aggkeys, aggfn, years,
                                         filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Primary Energy Consumption (Direct Equivalent)', 'Population')
    }
    else {
        message('Function for processing variable: Primary Energy')
        pe <- allqueries$'Primary Energy Consumption (Direct Equivalent)' %>%
            dplyr::select(-rundate)
        # Aggregating here allows us to sum globally
        pe <- aggregate(pe, aggfn, aggkeys)

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)
        # Aggregating here allows us to sum globally
        pop <- aggregate(pop, aggfn, aggkeys)

        join_cats <- names(pop)[!(names(pop) %in% c('value', 'Units'))]
        pe_pc <- pe %>%
            dplyr::left_join(pop, by = join_cats) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        pe_pc <- filter(pe_pc, years, filters)

        if(is.na(ounit))
            return(pe_pc)

        iunit <- pe_pc$Units[1]
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
            dplyr::mutate(pe_pc, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            pe_pc
        }
    }
}
