#### Data modules for the emissions group

#' Emissions Data Module

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
                                  filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'CO2 emissions'
    }
    else {
        ## silence notes on package check
        Units <- scenario <- region <- year <- service <- submode <- value <-
            NULL
        warning('The CO2 emissions module appears not to have been tested.')
        message('Function for processing variable: CO2 emissions')
        co2 <- allqueries$'CO2 emissions'
        co2 <- trans_standardize(co2) %>% # function stored in transp modules group
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        co2 <- filter(co2, years, filters, filter_operator)
        co2 <- aggregate(co2, aggfn, aggkeys)
        co2 <- unitconv_co2(co2, ounit)
        co2
    }
}

module.bc_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'BC emissions by subsector'
    }
    else {
        message('Function for processing variable: BC emissions by subsector')
        bc_subsector <- allqueries$'BC emissions by subsector'
        bc_subsector <- filter(bc_subsector, years, filters, filter_operator)
        bc_subsector <- aggregate(bc_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(bc_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                bc_subsector$value <- bc_subsector$value * cfac
                bc_subsector$Units <- ounit
            }
        }

        bc_subsector
    }
}

module.ch4_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'CH4 emissions by subsector'
    }
    else {
        message('Function for processing variable: CH4 emissions by subsector')
        ch4_subsector <- allqueries$'CH4 emissions by subsector'
        ch4_subsector <- filter(ch4_subsector, years, filters, filter_operator)
        ch4_subsector <- aggregate(ch4_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(ch4_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                ch4_subsector$value <- ch4_subsector$value * cfac
                ch4_subsector$Units <- ounit
            }
        }

        ch4_subsector
    }
}

module.co_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                              filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'CO emissions by subsector'
    }
    else {
        message('Function for processing variable: CO emissions by subsector')
        CO_subsector <- allqueries$'CO emissions by subsector'
        CO_subsector <- filter(CO_subsector, years, filters, filter_operator)
        CO_subsector <- aggregate(CO_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(CO_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                CO_subsector$value <- CO_subsector$value * cfac
                CO_subsector$Units <- ounit
            }
        }

        CO_subsector
    }
}

module.co2_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'CO2 emissions by subsector'
    }
    else {
        message('Function for processing variable: CO2 emissions by subsector')
        CO2_subsector <- allqueries$'CO2 emissions by subsector'
        CO2_subsector <- filter(CO2_subsector, years, filters, filter_operator)
        CO2_subsector <- aggregate(CO2_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_co2(CO2_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                CO2_subsector$value <- CO2_subsector$value * cfac
                CO2_subsector$Units <- ounit
            }
        }

        CO2_subsector
    }
}

module.ghg_emissions_by_region <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'GHG emissions by region'
    }
    else {
        message('Function for processing variable: GHG emissions by region')
        ghg_region <- allqueries$'GHG emissions by region'
        ghg_region <- filter(ghg_region, years, filters, filter_operator)
        ghg_region <- aggregate(ghg_region, aggfn, aggkeys)

        if((!is.na(ounit))&(grepl('CO2', filters))) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_co2(ghg_region$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                ghg_region$value <- ghg_region$value * cfac
                ghg_region$Units <- ounit
            }
        }

        else if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(ghg_region$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                ghg_region$value <- ghg_region$value * cfac
                ghg_region$Units <- ounit
            }
        }



        ghg_region
    }
}

module.ghg_emissions_by_technology <- function(mode, allqueries, aggkeys, aggfn, years,
                                           filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'GHG emissions by technology'
    }
    else {
        message('Function for processing variable: GHG emissions by technology')
        ghg_tech <- allqueries$'GHG emissions by technology'
        ghg_tech <- filter(ghg_tech, years, filters, filter_operator)
        ghg_tech <- aggregate(ghg_tech, aggfn, aggkeys)

        if((!is.na(ounit))&(grepl('CO2', filters))) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_co2(ghg_tech$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                ghg_tech$value <- ghg_tech$value * cfac
                ghg_tech$Units <- ounit
            }
        }

        else if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(ghg_tech$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                ghg_tech$value <- ghg_tech$value * cfac
                ghg_tech$Units <- ounit
            }
        }

        ghg_tech
    }
}

module.nh3_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'NH3 emissions by subsector'
    }
    else {
        message('Function for processing variable: NH3 emissions by subsector')
        NH3_subsector <- allqueries$'NH3 emissions by subsector'
        NH3_subsector <- filter(NH3_subsector, years, filters, filter_operator)
        NH3_subsector <- aggregate(NH3_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(NH3_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                NH3_subsector$value <- NH3_subsector$value * cfac
                NH3_subsector$Units <- ounit
            }
        }

        NH3_subsector
    }
}

module.nmvoc_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'NMVOC emissions by subsector'
    }
    else {
        message('Function for processing variable: NMVOC emissions by subsector')
        NMVOC_subsector <- allqueries$'NMVOC emissions by subsector'
        NMVOC_subsector <- filter(NMVOC_subsector, years, filters, filter_operator)
        NMVOC_subsector <- aggregate(NMVOC_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(NMVOC_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                NMVOC_subsector$value <- NMVOC_subsector$value * cfac
                NMVOC_subsector$Units <- ounit
            }
        }

        NMVOC_subsector
    }
}

module.nox_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'NOx emissions by subsector'
    }
    else {
        message('Function for processing variable: NOx emissions by subsector')
        NOx_subsector <- allqueries$'NOx emissions by subsector'
        NOx_subsector <- filter(NOx_subsector, years, filters, filter_operator)
        NOx_subsector <- aggregate(NOx_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(NOx_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                NOx_subsector$value <- NOx_subsector$value * cfac
                NOx_subsector$Units <- ounit
            }
        }

        NOx_subsector
    }
}

module.oc_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'OC emissions by subsector'
    }
    else {
        message('Function for processing variable: OC emissions by subsector')
        OC_subsector <- allqueries$'OC emissions by subsector'
        OC_subsector <- filter(OC_subsector, years, filters, filter_operator)
        OC_subsector <- aggregate(OC_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(OC_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                OC_subsector$value <- OC_subsector$value * cfac
                OC_subsector$Units <- ounit
            }
        }

        OC_subsector
    }
}

module.so2_emissions_by_subsector <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, filter_operator, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'SO2 emissions by subsector'
    }
    else {
        message('Function for processing variable: SO2 emissions by subsector')
        SO2_subsector <- allqueries$'SO2 emissions by subsector'
        SO2_subsector <- filter(SO2_subsector, years, filters, filter_operator)
        SO2_subsector <- aggregate(SO2_subsector, aggfn, aggkeys)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            if(grepl('Mt', ounit)) {
                ounit_convert <- 'MT'
            }
            else {
                ounit_convert <- ounit
            }
            cfac <- unitconv_mass(SO2_subsector$Units[1], ounit_convert)
            if(!is.na(cfac)) {
                SO2_subsector$value <- SO2_subsector$value * cfac
                SO2_subsector$Units <- ounit
            }
        }

        SO2_subsector
    }
}
