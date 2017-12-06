#### Data modules for the emissions group

#' CO2 emissions Data Module
#'
#' Produce service output by technology and vintage
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
