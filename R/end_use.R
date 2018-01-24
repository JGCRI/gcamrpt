#### Data modules for the end-use sectors group

#' Final Energy by Sector and Fuel Data Module
#'
#' Produce total final by sector and fuel by region.
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

module.final_energy_sector_fuel <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Final energy by detailed end-use sector and fuel'
    }
    else {
        message('Function for processing variable: Final Energy by detailed end-use sector and fuel')

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_fuel, by = 'input') %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector')
        final_energy <- filter(final_energy, years, filters)
        final_energy <- aggregate(final_energy, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_energy(final_energy$Units[1], ounit)
            if(!is.na(cfac)) {
                final_energy$value <- final_energy$value *cfac
                final_energy$Units <- ounit
            }
        }
        final_energy
    }
}

#' Building Floorspace Module
#'
#' Produce building floorspace by region.
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

module.building_floorspace <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Building floorspace'
    }
    else {
        message('Function for processing variable: Building floorspace')

        floorspace <- allqueries$'Building floorspace' %>%
            dplyr::select(-nodeinput, -`building-node-input`)
        floorspace <- filter(floorspace, years, filters)
        floorspace <- aggregate(floorspace, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cfac <- unitconv_counts(floorspace$Units[1], ounit)
            if(!is.na(cfac)) {
                floorspace$value <- floorspace$value *cfac
                floorspace$Units <- ounit
            }
        }
        floorspace
    }
}

#' Cement Production Module
#'
#' Produce cement production by region.
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

module.cement_production <- function(mode, allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Cement production by region'
    }
    else {
        message('Function for processing variable: Cement production')

        cement <- allqueries$'Cement production by region'
        cement <- filter(cement, years, filters)
        cement <- aggregate(cement, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cfac <- unitconv_mass(cement$Units[1], ounit)
            if(!is.na(cfac)) {
                cement$value <- cement$value *cfac
                cement$Units <- ounit
            }
        }
        cement
    }
}

#' Final Energy per Capita Module
#'
#' Produce total final per capita by region.
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

module.final_energy_per_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('Final energy by detailed end-use sector and fuel', 'Population')
    }
    else {
        message('Function for processing variable: Final Energy per capita')

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_fuel, by = 'input') %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector') %>%
            # Aggregating here allows us to sum globally, but not by sector/fuel
            aggregate(aggfn, aggkeys)

        pop <- allqueries$'Population' %>%
            # Aggregating here allows us to sum globally, but not by sector/fuel
            aggregate(aggfn, aggkeys)

        if ('region' %in% names(final_energy) & 'region' %in% names(pop)){
        final_energy_cap <- final_energy %>%
            dplyr::left_join(pop, by = c('scenario', 'region', 'year'))
        } else {
            final_energy_cap <- final_energy %>%
                dplyr::left_join(pop, by = c('scenario', 'year'))
        }

        final_energy_cap <- filter(final_energy_cap, years, filters) %>%
            dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
            dplyr::mutate(value = value.x / value.y) %>%
            dplyr::select(Units, scenario, region, year, value)

        if(is.na(ounit))
            return(final_energy_cap)

        iunit <- final_energy_cap$Units[1]
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
            dplyr::mutate(final_energy_cap, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            final_energy_cap
        }
    }
}

#' Final Energy Per GDP Data Module
#'
#' Produce final energy per GDP by region, converted to EJ/$ with AR4 GWPs
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

module.final_energy_per_gdp <- function(mode, allqueries, aggkeys, aggfn, years,
                                         filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Final energy by detailed end-use sector and fuel', 'GDP(MER)')
    }
    else {
        message('Function for processing variable: Final energy per GDP by region')

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_fuel, by = 'input') %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector') %>%
            # Aggregating here allows us to sum globally, but not by sector/fuel
            aggregate(aggfn, aggkeys)

        gdp <- allqueries$'GDP(MER)' %>%
            # Aggregating here allows us to sum globally, but not by sector/fuel
            aggregate(aggfn, aggkeys)

        if ('region' %in% names(gdp) & 'region' %in% names(final_energy)){
            final_energy_gdp <- final_energy %>%
                dplyr::left_join(gdp, by = c('scenario', 'region', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, region, year, value)
        } else {
            final_energy_gdp <- final_energy %>%
                dplyr::left_join(gdp, by = c('scenario', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, year, value)
        }


        final_energy_gdp <- filter(final_energy_gdp, years, filters)

        if(is.na(ounit))
            return(final_energy_gdp)

        iunit <- final_energy_gdp$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_energy(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE) *
            unitconv_usdollar(mmat[1,3], mmat[2,3], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(final_energy_gdp, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            final_energy_gdp
        }
    }
}

#' Final Energy Fuel Shares by Sector Data Module
#'
#' Produce final energy fuel shares by sector.
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

module.final_energy_fuel_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Final energy by detailed end-use sector and fuel'
    }
    else {
        message('Function for processing variable: Final Energy Fuel Shares by Sector')

        final_energy_shares <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_fuel, by = 'input') %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector') %>%
            dplyr::group_by(Units, scenario, region, year, rundate, fuel, end_use_sector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Can aggregate globally or by end use sector here
        final_energy_shares <- aggregate(final_energy_shares, aggfn, aggkeys)

        grp_vars <- names(final_energy_shares[!(names(final_energy_shares) %in% c('value', 'fuel'))])
        final_energy_shares <- final_energy_shares %>%
            dplyr::group_by_(.dots = grp_vars) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage Share of Fuel')

        final_energy_shares <- filter(final_energy_shares, years, filters)

        final_energy_shares
    }
}

#' Cement Production Per Capita Module
#'
#' Produce cement production per capita by region.
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

module.cement_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('Cement production by region', 'Population')
    }
    else {
        message('Function for processing variable: Cement production per capita')

        cement <- allqueries$'Cement production by region' %>%
            dplyr::select(-rundate)
        # Can only aggregate globally
        cement <- aggregate(cement, aggfn, aggkeys)

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)
        pop <- aggregate(pop, aggfn, aggkeys)

        # Determine columns to join by
        join_cats <- names(pop)[!(names(pop) %in% c('Units', 'value'))]

        cement_capita <- cement %>%
            dplyr::left_join(pop, by = join_cats) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)
        cement_capita <- filter(cement_capita, years, filters)

        if(is.na(ounit))
            return(cement_capita)

        iunit <- cement_capita$Units[1]
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
            dplyr::mutate(cement_capita, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            cement_capita
        }

    }
}

#' Building Floorspace Per Capita Module
#'
#' Produce building floorspace per capita by region.
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

module.building_flrsp_cap <- function(mode, allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        c('Building floorspace', 'Population')
    }
    else {
        message('Function for processing variable: Building floorspace per capita')

        floorspace <- allqueries$'Building floorspace' %>%
            dplyr::select(-nodeinput, -`building-node-input`, -rundate)
        floorspace <- aggregate(floorspace, aggfn, aggkeys)

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)
        if (!is.na(aggkeys) & !(grepl('region', aggkeys))){
            pop <- aggregate(pop, aggfn, aggkeys)
        }
        # Determine columns to join by
        join_cats <- names(pop)[!(names(pop) %in% c('Units', 'value'))]

        floorspace_cap <- floorspace %>%
            dplyr::left_join(pop, by = join_cats) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        floorspace_cap <- filter(floorspace_cap, years, filters)

        if(is.na(ounit))
            return(floorspace_cap)

        iunit <- floorspace_cap$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+)? *[A-z0-9\\^]+ */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_counts(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(floorspace_cap, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            floorspace_cap
        }
    }
}

#' Refined Liquids Shares Module
#'
#' Produce shares of refined liquids by fuel type by region.
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

module.liquid_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Refined Liquids'
    }
    else {
        message('Function for processing variable: Refined liquids fuel shares')

        liquids <- allqueries$'Refined Liquids'
        liquids <- filter(liquids, years, filters)
        liquids <- aggregate(liquids, aggfn, aggkeys)

        grp_cats <- names(liquids)[!(names(liquids) %in% c('subsector', 'value'))]

        liquids <- liquids %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage')

        liquids
    }
}

