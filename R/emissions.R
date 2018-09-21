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
        c('Final energy', 'Refined liquids')
    }
    else {
        message('Function for processing variable: CO2 emissions')
        # 2018/09/21 GPK modification: model-reported CO2 emissions include bio-derived carbon and as such are not the
        # correct quantities for reporting purposes. This method replaces the CO2 query aggregation with a bottom-up
        # calculation of energy consumption by fuel (excluding biofuels) multiplied by exogenous fuel carbon contents.
        energy <- allqueries$'Final energy'
        refining <- allqueries$'Refined liquids'
        energy <- mapfuel(energy, refining) #replaces input/technology with fuel & liquid_type

        # Make a table with the fuel carbon contents used in GCAM
        fuel_carbon_contents <- data.frame(
            fuel = c("Coal", "Natural Gas", "Electricity", "Hydrogen", "Liquids"),
            Ccoef = c(27.3, 14.2, 0, 0, 19.6),                 # These need to be the same as the values assumed in GCAM
            stringsAsFactors = FALSE )

        co2 <- subset(energy, liquid_type != "biomass") %>%     # Exclude biofuels from calculation of CO2 emissions
            dplyr::left_join(fuel_carbon_contents, by = "fuel") %>%
            dplyr::mutate(value = value * Ccoef,
                          Units = "MTC") %>%                    # Set the unit to what GCAM normally reports
            dplyr::select(-Ccoef)
        co2 <- normalize(co2) %>% # function stored in transp modules group
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        co2 <- filter(co2, years, filters)
        co2 <- aggregate(co2, aggfn, aggkeys)
        co2 <- unitconv_co2(co2, ounit)
        co2
    }
}

#' PM emissions Data Module
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

module.pm_emissions <- function(mode, allqueries, aggkeys, aggfn, years,
                                 filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Service output', 'Load factors')
    }
    else {
        message('Function for processing variable: PM emissions')
        # data prep
        serviceOutput <- allqueries$'Service output'
        serviceOutput <- normalize(serviceOutput) %>%
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        ldfctr <- allqueries$'Load factors'
        ldfctr <- normalize(ldfctr) %>%
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>% # average over LHDT
            dplyr::summarise(value=mean(value)) %>%
            dplyr::ungroup()

        # calculation
        vkm <- serviceOutput %>%
            dplyr::inner_join(ldfctr,
                              by = c('scenario', 'region', 'service', 'mode', 'submode', 'year')) %>%
            dplyr::rename(pkm=value.x, lf=value.y) %>%
            dplyr::mutate(vkm = pkm/lf, Units='million vehicle-km') %>%
            # relying on native units of 'million pass-km'/'million ton-km'
            dplyr::select(-pkm, -Units.x, -lf, -Units.y)  # depending on units of pm coefficients, need to convert million vehicle-km

        pm <- pm_emissions_factors %>% #sysdata
            dplyr::rename(pmfac=value) %>%
            dplyr::inner_join(vkm,
                              by = c('service', 'mode', 'submode', 'year')) %>%
            dplyr::mutate(pm_emissions = vkm*pmfac, Units='Mg') %>%
            dplyr::select(-pmfac, -Units.x, -vkm, -Units.y) %>%
            dplyr::rename(value=pm_emissions)

        pm <- filter(pm, years, filters)
        pm <- aggregate(pm, aggfn, aggkeys)
        pm <- unitconv_weight(pm, ounit)
        pm
    }
}
