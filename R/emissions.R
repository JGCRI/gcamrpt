#### Data modules for the emissions group

#' CO2 emissions Data Module
#'
#' Produce CO2 emissions by technology and vintage
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
                                  filters, ounit)
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
        co2 <- filter(co2, years, filters)
        co2 <- aggregate(co2, aggfn, aggkeys)
        co2 <- unitconv_co2(co2, ounit)
        co2
    }
}

#' GHG Emissions Data Module
#'
#' Produce ghg emissions by subsector, converted to MTCO2e with AR4 GWPs
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

module.ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                 filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'GHG emissions by subsector'
    }
    else {
        message('Function for processing variable: GHG emissions by subsector')

        ghg <- allqueries$'GHG emissions by subsector'
        ghg <- filter(ghg, years, filters) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP)
        ghg <- aggregate(ghg, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cfac <- unitconv_counts(ghg$Units[1], ounit)
            if(!is.na(cfac)) {
                ghg$value <- ghg$value *cfac
                ghg$Units <- ounit
            }
        }
        ghg
    }
}

#' GHG Emissions Data Module
#'
#' Produce ghg emissions by technology, converted to MTCO2e with AR5 GWPs
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

module.ghg_emissions_ar5 <- function(mode, allqueries, aggkeys, aggfn, years,
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'GHG emissions by technology'
    }
    else {
        message('Function for processing variable: GHG emissions by technology')

        ghg <- allqueries$'GHG emissions by technology'
        ghg <- filter(ghg, years, filters) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar5, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP)
        ghg <- aggregate(ghg, aggfn, aggkeys)
        if(!is.na(ounit)) {
            cfac <- unitconv_counts(ghg$Units[1], ounit)
            if(!is.na(cfac)) {
                ghg$value <- ghg$value *cfac
                ghg$Units <- ounit
            }
        }
        ghg
    }
}



#' GHG Emissions Intensity Data Module
#'
#' Produce ghg emissions intensity (of final energy) by region, converted to MTCO2e/EJ with AR4 GWPs
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

module.ghg_emissions_intensity_region <- function(mode, allqueries, aggkeys, aggfn, years,
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Final energy by detailed end-use sector and fuel')
    }
    else {
        message('Function for processing variable: GHG emissions intensity by region')

        ghg <- allqueries$'GHG emissions by subsector' %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(emissions = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP, -value)

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::rename(energy = value)

        ghg_intensity <- ghg %>%
            dplyr::left_join(final_energy, by = c('scenario', 'region', 'subsector', 'sector', 'year')) %>%
            group_by(scenario, ghg, region, year) %>%
            summarise(intensity = sum(emissions)/sum(energy)) %>%
            ungroup() %>%
            mutate(Units = 'MtCO2e/EJ')

        ghg_intensity <- filter(ghg_intensity, years, filters)
        ghg_intensity <- aggregate(ghg_intensity, aggfn, aggkeys)

        if(is.na(ounit))
            return(ghg_intensity)

        iunit <- ghg_intensity$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) */ *(\\w+)? +(\\w+) *- *(\\w+)'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_energy(mmat[1,3], mmat[2,3], inverse=TRUE)

        if(mmat[1,5] != mmat[2,5]) {
            ## Figure out what the new output unit will be if we don't convert
            ## length
            newounit <- sub(paste0('\\b', mmat[2,5], '\\b'), mmat[1,5])
            warning('Attempting to change length unit in unit conversion from ',
                    iunit, ' to ', ounit,
                    '.  This is not supported.  Units will be reported as ',
                    newounit)
            ounit <- newounit
        }

        if(!is.na(cfac)) {
            dplyr::mutate(ghg_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            ghg_intensity
        }
    }
}
