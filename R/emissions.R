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
                                  filters, ounit, region, agg_region, add_global)
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
        co2 <- region_agg(co2, region, agg_region, add_global)
        co2 <- unitconv_co2(co2, ounit)
        co2
    }
}

#' CO2 emissions by end-use sector Data Module
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
module.co2_emissions_end_use <- function(mode, allqueries, aggkeys, aggfn, years,
                                         filters, ounit, region, agg_region, add_global)
{
  if(mode == GETQ) {
    # Return titles of necessary queries
    # For more complex variables, will return multiple query titles in vector
    'CO2 Emissions by enduse'
  }
  else {
    message('Function for processing variable: CO2 Emissions by enduse')
    co2 <- allqueries$'CO2 Emissions by enduse'
    co2 <- filter(co2, years, filters)
    co2 <- aggregate(co2, aggfn, aggkeys)
    co2 <- region_agg(co2, region, agg_region, add_global)

    if(!is.na(ounit)) {
      cfac <- unitconv_co2(co2$Units[1], ounit)
      if(!is.na(cfac)) {
        co2$value <- co2$value *cfac
        co2$Units <- ounit
      }
    }
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
                                     filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Land Use Change Emission (future)')
    }
    else {
        message('Function for processing variable: GHG emissions by subsector')

        luc <- allqueries$'Land Use Change Emission (future)' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "MTC",
                          ghg = "LUC CO2",
                          sector = "LUC",
                          subsector = "LUC")

        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::bind_rows(luc) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP) %>%
            # Add in gas type
            dplyr::left_join(ghg_gas_type, by = 'ghg')

        ghg <- filter(ghg, years, filters)
        ghg <- aggregate(ghg, aggfn, aggkeys)
        ghg <- region_agg(ghg, region, agg_region, add_global)

        if(is.na(ounit))
            return(ghg)

        iunit <- ghg$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(ghg, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            ghg
        }
    }
}

#' GHG Emissions Data Module
#'
#' Produce ghg emissions by subsector, converted to MTCO2e with AR5 GWPs
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
                                     filters, ounit, region, agg_region, add_global)
{
  if(mode == GETQ) {
    # Return titles of necessary queries
    # For more complex variables, will return multiple query titles in vector
      c('GHG emissions by subsector', 'Land Use Change Emission (future)')
  }
  else {
    message('Function for processing variable: GHG emissions by subsector')

    luc <- allqueries$'Land Use Change Emission (future)' %>%
        dplyr::group_by(Units, scenario, region, year) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Units = "MTC",
                    ghg = "LUC CO2",
                    sector = "LUC",
                    subsector = "LUC")

    ghg <- allqueries$'GHG emissions by subsector' %>%
      dplyr::bind_rows(luc) %>%
      # Add in GWP, and remove gases without GWP
      dplyr::right_join(gwp_ar5, by = c('ghg', 'Units')) %>%
      # Convert to MTCO2e
      dplyr::mutate(value = value * GWP,
                    Units = 'MT CO2e') %>%
      dplyr::select(-GWP) %>%
      # Add in gas type
      dplyr::left_join(ghg_gas_type, by = 'ghg')

    ghg <- filter(ghg, years, filters)
    ghg <- aggregate(ghg, aggfn, aggkeys)
    ghg <- region_agg(ghg, region, agg_region, add_global)

    if(is.na(ounit))
        return(ghg)

    iunit <- ghg$Units[1]
    ## This doesn't currently allow for converting m^2
    pat <- '(\\w+)+ *(\\w+)+ *'
    mmat <- stringr::str_match(c(iunit, ounit), pat)
    cfac <-
        unitconv_mass(mmat[1,2], mmat[2,2]) *
        unitconv_co2(mmat[1,3], mmat[2,3])
    if(!is.na(cfac)) {
        dplyr::mutate(ghg, value=cfac*value, Units=ounit)
    }
    else {
        ## If any conversions failed, the warning will already have been
        ## issued, so just return the result unconverted.
        ghg
    }
  }
}

#' GHG Emissions Intensity By Region Data Module
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
                                     filters, ounit, region, agg_region, add_global){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Final energy by detailed end-use sector and fuel',
          'Land Use Change Emission (future)')
    }
    else {
        message('Function for processing variable: GHG emissions intensity by region')

        luc <- allqueries$'Land Use Change Emission (future)' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "MTC",
                          ghg = "LUC CO2")

        # Aggregate emissions and energy by region/year
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::bind_rows(luc) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)
        ghg <- region_agg(ghg, region, agg_region, add_global)

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)
        final_energy <- region_agg(final_energy, region, agg_region, add_global)

        if ('region' %in% names(final_energy) & 'region' %in% names(ghg)){
        ghg_intensity <- ghg %>%
            dplyr::left_join(final_energy, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
            dplyr::mutate(intensity = value.x / value.y) %>%
            dplyr::select(Units, scenario, region, year, value = intensity)
        } else {
            ghg_intensity <- ghg %>%
                dplyr::left_join(final_energy, by = c('scenario', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(intensity = value.x / value.y) %>%
                dplyr::select(Units, scenario, year, value = intensity)
        }


        ghg_intensity <- filter(ghg_intensity, years, filters)

        if(is.na(ounit))
            return(ghg_intensity)

        iunit <- ghg_intensity$Units[1]
        pat <- '(\\w+) *(\\w+) */ *(\\w+)'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_energy(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(ghg_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ghg_intensity
        }
    }
}

#' GHG Emissions Per Capita Data Module
#'
#' Produce ghg emissions per capita by region, converted to MTCO2e/EJ with AR4 GWPs
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

module.ghg_emissions_per_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                                                  filters, ounit, region, agg_region, add_global){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Population',
          'Land Use Change Emission (future)')
    }
    else {
        message('Function for processing variable: GHG emissions per capita by region')

        luc <- allqueries$'Land Use Change Emission (future)' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "MTC",
                          ghg = "LUC CO2")

        # Aggregate emissions by region/year
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::bind_rows(luc) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)
        ghg <- region_agg(ghg, region, agg_region, add_global)

        pop <- allqueries$'Population' %>%
            aggregate(aggfn, aggkeys)
        pop <- region_agg(pop, region, agg_region, add_global)

        if ('region' %in% names(pop) & 'region' %in% names(ghg)){
            ghg_cap <- ghg %>%
                dplyr::left_join(pop, by = c('scenario', 'region', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, region, year, value)
        } else {
            ghg_cap <- ghg %>%
                dplyr::left_join(final_energy, by = c('scenario', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, year, value)
        }


        ghg_cap <- filter(ghg_cap, years, filters)

        if(is.na(ounit))
            return(ghg_cap)

        iunit <- ghg_cap$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) *(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(ghg_cap, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            ghg_cap
        }
    }
}

#' GHG Emissions Per GDP Data Module
#'
#' Produce ghg emissions per GDP by region, converted to MTCO2e/$ with AR4 GWPs
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

module.ghg_emissions_per_gdp <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit, region, agg_region, add_global){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'GDP(MER)',
          'Land Use Change Emission (future)')
    }
    else {
        message('Function for processing variable: GHG emissions per GDP by region')

        luc <- allqueries$'Land Use Change Emission (future)' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "MTC",
                          ghg = "LUC CO2")

        # Aggregate emissions by region/year
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::bind_rows(luc) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)
        ghg <- region_agg(ghg, region, agg_region, add_global)

        gdp <- allqueries$'GDP(MER)' %>%
            dplyr::mutate(Units = dplyr::if_else(Units == "Million1990US$", "Million 1990US$", Units))

        gdp <- gdp %>%
            dplyr::filter(year %in% unique(ghg$year)) %>%
            aggregate(aggfn, aggkeys)
        gdp <- region_agg(gdp, region, agg_region, add_global)

        if ('region' %in% names(gdp) & 'region' %in% names(ghg)){
            ghg_gdp <- ghg %>%
                dplyr::filter(year %in% unique(gdp$year)) %>%
                dplyr::left_join(gdp, by = c('scenario', 'region', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, region, year, value)
        } else {
            ghg_gdp <- ghg %>%
                dplyr::filter(year %in% unique(gdp$year)) %>%
                dplyr::left_join(gdp, by = c('scenario', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, year, value)
        }


        ghg_gdp <- filter(ghg_gdp, years, filters)

        if(is.na(ounit))
            return(ghg_gdp)

        iunit <- ghg_gdp$Units[1]
        pat <- '(\\w+) *(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE) *
            unitconv_usdollar(mmat[1,5], mmat[2,5], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(ghg_gdp, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            ghg_gdp
        }
    }
}

#' @describeIn Direct emissions worker function for distributing bio emissions
process.direct_bio_emissions <- function(input, output, sequestration){
    gas_sectors <- c('gas processing', 'gas pipeline', 'wholesale gas', 'delivered gas')
    liquids_sectors <- c('refined liquids enduse', 'refined liquids industrial', 'refining')

    Ccoef_calculator <- function(processing_sector, fuel_list, coefficients){
        Ccoef.fuel <- input %>%
            dplyr::left_join(output, by = c("Units", "scenario", "region", "sector", "subsector", "technology", "year")) %>%
            dplyr::filter(sector == processing_sector) %>%
            dplyr::left_join(coefficients, c("region", "input" = "fuel", "year")) %>%
            na.omit() %>%
            dplyr::group_by(Units, scenario, region, sector, year) %>%
            dplyr::summarise(Ccoef = weighted.mean(Ccoef, value.y)) %>%
            dplyr::ungroup() %>%
            dplyr::select(region, fuel = sector, Ccoef, year) %>%
            tidyr::complete(tidyr::nesting(region, Ccoef, year), fuel = fuel_list)

        return(Ccoef.fuel)
    }

    # First, calculate carbon coefficient of gas processing
    Ccoef.gas <- Ccoef_calculator("gas processing", gas_sectors, primary_ccoef)

    # Then, calculate carbon coefficient of refining
    Ccoef.refining <- Ccoef_calculator("refining", liquids_sectors, primary_ccoef)

    # Add to the rest of coefficients
    Ccoef <- dplyr::bind_rows(Ccoef, Ccoef.gas, Ccoef.refining)

    # Calculate carbon from inputs
    input.d <- input %>%
        dplyr::filter(Units == "EJ"|input == "limestone") %>%
        dplyr::left_join(Ccoef, c("region", "input" = "fuel", "year")) %>%
        dplyr:: mutate(C.in = value * Ccoef) %>%
        # Aggregate to techology level
        dplyr::group_by(scenario, region, sector, year) %>%
        dplyr::summarise(C.in = sum(C.in)) %>%
        dplyr::ungroup()

    # Calculate carbon from outputs
    output.d <- output %>%
        # Filter out agriculture
        dplyr::filter(sector == output) %>%
        dplyr::left_join(Ccoef, c("region", "sector" = "fuel", "year")) %>%
        tidyr::replace_na(list(Ccoef = 0)) %>%
        dplyr::mutate(C.out = value * Ccoef) %>%
        # Aggregate to techology level
        dplyr::group_by(scenario, region, sector, year) %>%
        dplyr::summarise(C.out = sum(C.out)) %>%
        dplyr::ungroup()

    # Combine and remove co2 sequestration
    total <- input.d %>%
        dplyr::left_join(output.d, by = c("scenario", "region", "sector", "year")) %>%
        tidyr::replace_na(list(C.out = 0)) %>%
        dplyr::mutate(emissions = C.in - C.out,
               Units = "MTC") %>%
        # join in sequestration and subtract
        dplyr::left_join(sequestration, by = c("Units", "scenario", "region", "sector", "year")) %>%
        tidyr::replace_na(list(value = 0)) %>%
        dplyr::mutate(emissions = round(emissions - value, 1)) %>%
        dplyr::select(scenario, region, sector, year, emissions, Units) %>%
        dplyr::rename(value = emissions)

    remove_sectors <- setdiff(unique(total$sector), unique(ghg_sector$sector))

    total <- dplyr::filter(total, !(sector %in% remove_sectors))

    return(total)

}

#' Direct GHG Emissions Data Module
#'
#' Produce direct sector ghg emissions, including CO2 emissions from biomass, converted to MTCO2e with AR4 GWPs
#' Does not include indirect emissions from electricity consumption.
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
module.direct_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                     filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector')
    }
    else {
        message('Function for processing variable: Direct GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- filter(all_ghg, years, filters) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP)

        all_ghg <- aggregate(all_ghg, aggfn, aggkeys)
        all_ghg <- region_agg(all_ghg, region, agg_region, add_global)

        if(is.na(ounit))
            return(all_ghg)

        iunit <- all_ghg$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(all_ghg, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            all_ghg
        }
    }
}

#' Indirect GHG Emissions Data Module
#'
#' Produce indirect sector ghg emissions by sector - emissions from electricity consumption.
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
module.indirect_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Central electricity demand by demand sector')
    }
    else {
        message('Function for processing variable: Indirect GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Central electricity demand by demand sector' %>%
            dplyr::select(-rundate) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion',
                          end_use = dplyr::if_else(sector == 'elect_td_bld', 'Buildings', ''),
                          end_use = dplyr::if_else(sector == 'elect_td_ind', 'Industry', end_use),
                          end_use = dplyr::if_else(sector == 'elect_td_trn', 'Transportation', end_use))

        # Now calculate all sectoral emissions, then split out electricity emissions
        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- all_ghg %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP)

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- all_ghg %>%
            dplyr::filter(end_use == "Electricity") %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, value = indirect_emissions, end_use) %>%
            filter(years, filters)

        indirect_emissions <- aggregate(indirect_emissions, aggfn, aggkeys)
        indirect_emissions <- region_agg(indirect_emissions, region, agg_region, add_global)

        if(is.na(ounit))
            return(indirect_emissions)

        iunit <- indirect_emissions$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(indirect_emissions, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            indirect_emissions
        }
    }
}

#' Total Sectoral GHG Emissions Data Module
#'
#' Produce total sector ghg emissions - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.total_enduse_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                              filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Central electricity demand by demand sector')
    }
    else {
        message('Function for processing variable: Enduse GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Central electricity demand by demand sector' %>%
            dplyr::select(-rundate) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion',
                          end_use = dplyr::if_else(sector == 'elect_td_bld', 'Buildings', ''),
                          end_use = dplyr::if_else(sector == 'elect_td_ind', 'Industry', end_use),
                          end_use = dplyr::if_else(sector == 'elect_td_trn', 'Transportation', end_use))

        # Now calculate all sectoral emissions, then split out electricity emissions
        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- all_ghg %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- all_ghg %>%
            dplyr::filter(end_use == "Electricity") %>%
            dplyr::select(-end_use) %>%
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, value = indirect_emissions, end_use)

        # Replace electricity emissions with indirect emissions
        total_emissions <- all_ghg %>%
            dplyr::filter(end_use != 'Electricity') %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)


        if(is.na(ounit))
            return(total_emissions)

        iunit <- total_emissions$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(total_emissions, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            total_emissions
        }
    }
}

#' End-Use Sector GHG Emissions Intensity Data Module
#'
#' Produce total sector ghg emissions intensity - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.total_enduse_intensity_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                                  filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Central electricity demand by demand sector', 'Final energy by detailed end-use sector and fuel')
    }
    else {
        message('Function for processing variable: Enduse GHG emissions intensity')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Central electricity demand by demand sector' %>%
            dplyr::select(-rundate) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion',
                          end_use = dplyr::if_else(sector == 'elect_td_bld', 'Buildings', ''),
                          end_use = dplyr::if_else(sector == 'elect_td_ind', 'Industry', end_use),
                          end_use = dplyr::if_else(sector == 'elect_td_trn', 'Transportation', end_use))

        energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::left_join(final_energy_end_use_sector, by = 'sector') %>%
            dplyr::group_by(Units, scenario, region, year, end_use_sector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::rename(end_use = end_use_sector)
        energy <- region_agg(energy, region, agg_region, add_global)

        # Now calculate all sectoral emissions, then split out electricity emissions
        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- all_ghg %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- all_ghg %>%
            dplyr::filter(end_use == "Electricity") %>%
            dplyr::select(-end_use) %>%
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, value = indirect_emissions, end_use)

        # Replace electricity emissions with indirect emissions
        total_emissions <- all_ghg %>%
            dplyr::filter(end_use != 'Electricity') %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)

        # Add energy and calculate intensity
        emissions_intensity <- total_emissions %>%
            dplyr::inner_join(energy, by = c("scenario", "region", "year", "end_use")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x , '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        emissions_intensity <- filter(emissions_intensity, years, filters)
        emissions_intensity <- aggregate(emissions_intensity, aggfn, aggkeys)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_energy(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(emissions_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            emissions_intensity
        }
    }
}

#' GHG Emissions Intensity of Electricity Data Module
#'
#' Produces GHG emissions intensity for electricity, including CO2 emissions from biomass, converted to MTCO2e with AR4 GWPs
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
module.elec_ghg_emissions_intensity_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Electricity')
    }
    else {
        message('Function for processing variable: Electricity GHG emissions intensity by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        electricity <- allqueries$'Electricity' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        electricity <- region_agg(electricity, region, agg_region, add_global)

        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- all_ghg %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP) %>%
            dplyr::filter(end_use == 'Electricity') %>%
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        all_ghg <- region_agg(all_ghg, region, agg_region, add_global)

        # Calculate intensity
        elec_intensity <- all_ghg %>%
            dplyr::inner_join(electricity, by = c("scenario", "region", "year")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        elec_intensity <- filter(elec_intensity, years, filters)
        elec_intensity <- aggregate(elec_intensity, aggfn, aggkeys)

        if(is.na(ounit))
            return(elec_intensity)

        iunit <- elec_intensity$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_energy(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(elec_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            elec_intensity
        }
    }
}

#' GHG Emissions Per Floorspace Data Module
#'
#' Produce floorspace ghg emissions intensity - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.floorspace_ghg_intensity_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                              filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Energy consumption by sector', 'Building floorspace')
    }
    else {
        message('Function for processing variable: Florspace GHG emissions intensity')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        # CO2 emissions by enduse sector
        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        # Electricity splits
        elec_split <- allqueries$'Energy consumption by sector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl('elect_td', input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion') %>%
            dplyr::filter(grepl('comm|resid', sector))

        # Electricity GHG emissions
        elec_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP) %>%
            dplyr::filter(end_use == 'Electricity') %>%
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Separate out building ghg emissions
        building_direct_ghg <- ghg %>%
            dplyr::filter(grepl('comm|resid', sector))

        # Building CO2 emissions
        building_co2 <- co2 %>%
            dplyr::filter(grepl('comm|resid', sector))

        # Now calculate all building emissions, not including electricity emissions
        building_direct_ghg <- building_direct_ghg %>%
            dplyr::bind_rows(building_co2) %>%
            dplyr::select(-rundate) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, sector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- elec_ghg %>%
            dplyr::select(-end_use) %>%
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, sector, value = indirect_emissions)

        # Replace electricity emissions with indirect emissions
        total_building_emissions <- building_direct_ghg %>%
            dplyr::bind_rows(indirect_emissions) %>%
            dplyr::mutate(building = stringr::str_extract(sector, '(\\w+)+(?= )')) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, building, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        total_building_emissions <- region_agg(total_building_emissions, region, agg_region, add_global)

        # Resid and comm floorspace
        flrspace <- allqueries$'Building floorspace' %>%
            dplyr::select(-nodeinput, -`building-node-input`, -rundate)
        flrspace <- region_agg(flrspace, region, agg_region, add_global)

        building_intensity <- total_building_emissions %>%
            dplyr::inner_join(flrspace, by = c("scenario", "region", "building", "year"))

        # Include total floorspace & emissions, then calculate intensity
        building_intensity <- building_intensity %>%
            dplyr::group_by(Units.x, Units.y, scenario, region, year) %>%
            dplyr::summarise(value.x = sum(value.x),
                             value.y = sum(value.y)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(building = 'total') %>%
            dplyr::bind_rows(building_intensity) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(Units, scenario, region, year, value, building)

        building_intensity <- filter(building_intensity, years, filters)

        if(is.na(ounit))
            return(building_intensity)

        iunit <- building_intensity$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+)|(\\d)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(building_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            building_intensity
        }
    }
}

#' GHG Emissions Per Capita by sector Data Module
#'
#' Produce ghg emissions per capita by sector - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.ghg_per_capita_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                                filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Energy consumption by sector', 'Population')
    }
    else {
        message('Function for processing variable: GHG emissions per capita')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        # CO2 emissions by enduse sector
        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        # Electricity splits
        elec_split <- allqueries$'Energy consumption by sector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl('elect_td', input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion',
                          end_use = dplyr::if_else(input == 'elect_td_bld', 'Buildings', ''),
                          end_use = dplyr::if_else(input == 'elect_td_ind', 'Industry', end_use),
                          end_use = dplyr::if_else(input == 'elect_td_trn', 'Transportation', end_use))

        # All GHG Emissions
        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP) %>%
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Electricity GHG emissions
        elec_ghg <- all_ghg %>%
            dplyr::filter(end_use == 'Electricity')

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- elec_ghg %>%
            dplyr::select(-end_use) %>%
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, sector, value = indirect_emissions, end_use) %>%
            dplyr::group_by(Units, scenario, region, end_use, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Replace electricity emissions with indirect emissions
        total_emissions <- all_ghg %>%
            dplyr::filter(end_use != 'Electricity') %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, end_use, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)
        pop <- region_agg(pop, region, agg_region, add_global)

        emissions_per_capita <- total_emissions %>%
            dplyr::left_join(pop, by = c("scenario", "region", "year")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        emissions_per_capita <- filter(emissions_per_capita, years, filters)

        if(is.na(ounit))
            return(emissions_per_capita)

        iunit <- emissions_per_capita$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(emissions_per_capita, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            emissions_per_capita
        }
    }
}

#' GHG Emissions Per veh-km Data Module
#'
#' Produce ghg emissions per veg-km - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.ghg_per_veh_km <- function(mode, allqueries, aggkeys, aggfn, years,
                                      filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Energy consumption by subsector', 'Transportation Service Output')
    }
    else {
        message('Function for processing variable: GHG emissions per vehicle-km')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion') %>%
            dplyr::filter(grepl("trn_", sector))

        # Now calculate all electricity emissions
        elec_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            dplyr::filter(end_use == "Electricity") %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Pull out electricity emissions in order to split between end use sectors
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, sector, subsector, year, value = indirect_emissions)

        # Transportation co2 split
        trn_co2_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::filter(grepl("trn_", sector),
                          !(subsector %in% c("Walk", "Cycle")),
                          input != "elect_td_trn",
                          input != "H2 enduse") %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::group_by(Units, scenario, sector, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "Proportion")

        # Calculate direct emissions
        direct_emissions <- co2 %>%
            dplyr::filter(grepl("trn_", sector)) %>%
            # apportion sector co2 emissions to subsector
            dplyr::inner_join(trn_co2_split, by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y) %>%
            dplyr::bind_rows(dplyr::filter(ghg, grepl("trn_", sector))) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, sector, subsector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Replace electricity emissions with indirect emissions
        total_emissions <- direct_emissions %>%
            dplyr::bind_rows(elec_ghg) %>%
            dplyr::left_join(trn_subsector_map, by = c("sector", "subsector")) %>%
            dplyr::filter(end_use == 'Transportation')

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)

        # Convert Passenger service output to vehicle km
        vkm <- trans_filter_svc(allqueries['Transportation Service Output'], TRUE)$'Transportation Service Output' %>%
            dplyr::mutate(technology = stringr::str_match(technology, ".+(?=,year)")) %>%
            dplyr::inner_join(load_factor, by = c("region", "sector", "subsector", "technology", "year")) %>%
            dplyr::mutate(value = value / loadFactor,
                          Units = 'million vehicle-km') %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        vkm <- filter(vkm, years, filters)
        vkm <- aggregate(vkm, aggfn, aggkeys)
        vkm <- region_agg(vkm, region, agg_region, add_global)

        # Add in service output to emissions, which will ignore H2 forecourt production
        emissions_intensity <- total_emissions %>%
            dplyr::inner_join(vkm, by = c("scenario", "region", "sector", "subsector", "year")) %>%
            dplyr::inner_join(trn_sector_map, by = c("sector", "subsector")) %>%
            dplyr::group_by(Units.x, scenario, region, year, Units.y, group) %>%
            dplyr::summarise(value.x = sum(value.x), value.y = sum(value.y)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y) %>%
            dplyr::rename(sector = group)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(emissions_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            emissions_intensity
        }
    }
}

#' GHG Emissions Per Cement production Data Module
#'
#' Produce ghg emissions per cement production - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.ghg_per_cement <- function(mode, allqueries, aggkeys, aggfn, years,
                                   filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Energy consumption by sector', 'Cement production by region')
    }
    else {
        message('Function for processing variable: Cement GHG emissions intensity')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by sector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(grepl("cement", sector)) %>%
            dplyr::mutate(Units = 'Proportion')

        # Now calculate all sectoral emissions, then split out electricity emissions
        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- all_ghg %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, sector, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        elec_ghg <- all_ghg %>%
            dplyr::filter(end_use == "Electricity") %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- elec_ghg %>%
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, value = indirect_emissions, sector)

        # Replace electricity emissions with indirect emissions
        total_emissions <- all_ghg %>%
            dplyr::filter(grepl("cement", sector)) %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)

        # Passenger service output
        cement_prod <- allqueries$'Cement production by region' %>%
            dplyr::select(-rundate)
        cement_prod <- region_agg(cement_prod, region, agg_region, add_global)

        # Add in service output to emissions, which will ignore H2 forecourt production
        emissions_intensity <- total_emissions %>%
            dplyr::inner_join(cement_prod, by = c("scenario", "region", "year")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        emissions_intensity <- filter(emissions_intensity, years, filters)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_mass(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(emissions_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            emissions_intensity
        }
    }
}

#' Total Cement GHG Emissions Data Module
#'
#' Produce total cement ghg emissions - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.cement_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                                  filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Energy consumption by sector')
    }
    else {
        message('Function for processing variable: Cement GHG emissions')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by sector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(grepl("cement", sector)) %>%
            dplyr::mutate(Units = 'Proportion')

        # Now calculate all sectoral emissions, then split out electricity emissions
        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- all_ghg %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, sector, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        elec_ghg <- all_ghg %>%
            dplyr::filter(end_use == "Electricity") %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- elec_ghg %>%
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, value = indirect_emissions, sector)

        # Replace electricity emissions with indirect emissions
        total_emissions <- all_ghg %>%
            dplyr::filter(grepl("cement", sector)) %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)


        if(is.na(ounit))
            return(total_emissions)

        iunit <- total_emissions$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(total_emissions, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            total_emissions
        }
    }
}

#' Total Transport GHG Emissions Data Module
#'
#' Produce total transport ghg emissions - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.total_trans_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                                  filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Energy consumption by subsector')
    }
    else {
        message('Function for processing variable: Total Transport GHG emissions')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion') %>%
            dplyr::filter(grepl("trn_", sector))

        # Now calculate all electricity emissions
        elec_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            dplyr::filter(end_use == "Electricity") %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Pull out electricity emissions in order to split between end use sectors
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, sector, subsector, year, value = indirect_emissions)

        # Transportation co2 split
        trn_co2_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::filter(grepl("trn_", sector),
                          !(subsector %in% c("Walk", "Cycle")),
                          input != "elect_td_trn",
                          input != "H2 enduse") %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::group_by(Units, scenario, sector, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "Proportion")

        # Calculate direct emissions
        direct_emissions <- co2 %>%
            dplyr::filter(grepl("trn_", sector)) %>%
            # apportion sector co2 emissions to subsector
            dplyr::inner_join(trn_co2_split, by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y) %>%
            dplyr::bind_rows(dplyr::filter(ghg, grepl("trn_", sector))) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, sector, subsector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Replace electricity emissions with indirect emissions
        total_emissions <- direct_emissions %>%
            dplyr::bind_rows(elec_ghg) %>%
            dplyr::left_join(trn_subsector_map, by = c("sector", "subsector")) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)


        if(is.na(ounit))
            return(total_emissions)

        iunit <- total_emissions$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(total_emissions, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            total_emissions
        }
    }
}

#' Direct Transport GHG Emissions Data Module
#'
#' Produce direct transport ghg emissions - includes biomass CO2 accounting.
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
module.direct_trans_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                                 filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Energy consumption by subsector')
    }
    else {
        message('Function for processing variable: Direct Transport GHG emissions')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::filter(ghg != 'CO2',
                          grepl("trn_", sector))

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::filter(grepl("trn_", sector))

        # Transportation co2 split
        trn_co2_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::filter(grepl("trn_", sector),
                          !(subsector %in% c("Walk", "Cycle")),
                          input != "elect_td_trn",
                          input != "H2 enduse") %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::group_by(Units, scenario, sector, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "Proportion")

        # Calculate direct emissions
        direct_emissions <- co2 %>%
            # apportion sector co2 emissions to subsector
            dplyr::right_join(trn_co2_split, by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y) %>%
            dplyr::bind_rows(ghg) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, sector, subsector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::left_join(trn_subsector_map, by = c("sector", "subsector")) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        direct_emissions <- filter(direct_emissions, years, filters)
        direct_emissions <- aggregate(direct_emissions, aggfn, aggkeys)
        direct_emissions <- region_agg(direct_emissions, region, agg_region, add_global)

        if(is.na(ounit))
            return(direct_emissions)

        iunit <- direct_emissions$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(direct_emissions, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            direct_emissions
        }
    }
}

#' Indirect Transport GHG Emissions Data Module
#'
#' Produce indirect transport ghg emissions - includes indirect emissions from electricity and biomass CO2 accounting in electricity.
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
module.indirect_trans_ghg_emissions_ar4 <- function(mode, allqueries, aggkeys, aggfn, years,
                                                 filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector', 'Energy consumption by subsector')
    }
    else {
        message('Function for processing variable: Indirect Transport GHG emissions')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion') %>%
            dplyr::filter(grepl("trn_", sector))

        # Now calculate all electricity emissions
        elec_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            dplyr::filter(end_use == "Electricity") %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Pull out electricity emissions in order to split between end use sectors
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(value = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, sector, subsector, year, value)

        # Replace electricity emissions with indirect emissions
        indirect_emissions <- elec_ghg %>%
            dplyr::left_join(trn_subsector_map, by = c("sector", "subsector")) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        indirect_emissions <- filter(indirect_emissions, years, filters)
        indirect_emissions <- aggregate(indirect_emissions, aggfn, aggkeys)
        indirect_emissions <- region_agg(indirect_emissions, region, agg_region, add_global)

        if(is.na(ounit))
            return(indirect_emissions)

        iunit <- indirect_emissions$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3])
        if(!is.na(cfac)) {
            dplyr::mutate(indirect_emissions, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            indirect_emissions
        }
    }
}

#' Share of Ag GHG Emissions from Enteric Fermentation Data Module
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
module.enteric_fermentation_share <- function(mode, allqueries, aggkeys, aggfn, years,
                                            filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'GHG emissions by sector'
    }
    else {
        message('Function for processing variable: Enteric fermentation GHG share')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            dplyr::filter(end_use == "Agriculture") %>%
            filter(years, filters) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            dplyr::select(-GWP) %>%
            # Aggregate over gases
            dplyr::group_by(Units, scenario, region, sector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        ghg <- aggregate(ghg, aggfn, aggkeys)
        ghg <- region_agg(ghg, region, agg_region, add_global)

        shares <- ghg %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(sector == "Beef") %>%
            dplyr::mutate(Units = "Percentage")

        shares
    }
}

#' GHG Emissions Per ton-km Data Module
#'
#' Produce ghg emissions per ton-km - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.ghg_per_ton_km <- function(mode, allqueries, aggkeys, aggfn, years,
                                  filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Energy consumption by subsector', 'Transportation Service Output')
    }
    else {
        message('Function for processing variable: Freight GHG emissions intensity')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion') %>%
            dplyr::filter(grepl("trn_", sector))

        # Now calculate all electricity emissions
        elec_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            dplyr::filter(end_use == "Electricity") %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Pull out electricity emissions in order to split between end use sectors
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, sector, subsector, year, value = indirect_emissions)

        # Transportation co2 split
        trn_co2_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::filter(grepl("trn_", sector),
                          !(subsector %in% c("Walk", "Cycle")),
                          input != "elect_td_trn",
                          input != "H2 enduse") %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::group_by(Units, scenario, sector, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "Proportion")

        # Calculate direct emissions
        direct_emissions <- co2 %>%
            dplyr::filter(grepl("trn_", sector)) %>%
            # apportion sector co2 emissions to subsector
            dplyr::inner_join(trn_co2_split, by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y) %>%
            dplyr::bind_rows(dplyr::filter(ghg, grepl("trn_", sector))) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, sector, subsector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Replace electricity emissions with indirect emissions
        total_emissions <- direct_emissions %>%
            dplyr::bind_rows(elec_ghg) %>%
            dplyr::left_join(trn_subsector_map, by = c("sector", "subsector")) %>%
            dplyr::filter(end_use == 'Transportation')

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)

        # Convert Passenger service output to vehicle km
        tkm <- trans_filter_svc(allqueries['Transportation Service Output'], FALSE)$'Transportation Service Output' %>%
            dplyr::mutate(technology = stringr::str_match(technology, ".+(?=,year)")) %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        tkm <- filter(tkm, years, filters)
        tkm <- aggregate(tkm, aggfn, aggkeys)
        tkm <- region_agg(tkm, region, agg_region, add_global)

        # Add in service output to emissions, which will ignore H2 forecourt production
        emissions_intensity <- total_emissions %>%
            dplyr::inner_join(tkm, by = c("scenario", "region", "sector", "subsector", "year")) %>%
            dplyr::group_by(Units.x, scenario, region, year, Units.y) %>%
            dplyr::summarise(value.x = sum(value.x), value.y = sum(value.y)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(emissions_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            emissions_intensity
        }
    }
}

#' GHG Emissions Per Transport Energy Data Module
#'
#' Produce ghg emissions per transport energy - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.ghg_trans_en_intensity <- function(mode, allqueries, aggkeys, aggfn, years,
                                  filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Consumption by technology',
          'Service output by technology', 'CO2 sequestration by sector',
          'Energy consumption by subsector')
    }
    else {
        message('Function for processing variable: Transport Energy GHG emissions intensity')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by subsector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- process.direct_bio_emissions(allqueries$`Consumption by technology`,
                                            allqueries$`Service output by technology`,
                                            allqueries$`CO2 sequestration by sector`) %>%
            dplyr::mutate(ghg = 'CO2')

        elec_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::select(-rundate) %>%
            dplyr::filter(grepl("elect_", input)) %>%
            # Turn demand into proportions
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Proportion') %>%
            dplyr::filter(grepl("trn_", sector))

        # Now calculate all electricity emissions
        elec_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector') %>%
            dplyr::filter(end_use == "Electricity") %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Pull out electricity emissions in order to split between end use sectors
            dplyr::inner_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, sector, subsector, year, value = indirect_emissions)

        # Transportation co2 split
        trn_co2_split <- allqueries$'Energy consumption by subsector' %>%
            dplyr::filter(grepl("trn_", sector),
                          !(subsector %in% c("Walk", "Cycle")),
                          input != "elect_td_trn",
                          input != "H2 enduse") %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::group_by(Units, scenario, sector, region, year) %>%
            dplyr::mutate(value = value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = "Proportion")

        # Calculate direct emissions
        direct_emissions <- co2 %>%
            dplyr::filter(grepl("trn_", sector)) %>%
            # apportion sector co2 emissions to subsector
            dplyr::inner_join(trn_co2_split, by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x * value.y,
                          Units = Units.x) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y) %>%
            dplyr::bind_rows(dplyr::filter(ghg, grepl("trn_", sector))) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::inner_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MT CO2e') %>%
            # aggregate across all ghgs
            dplyr::group_by(Units, scenario, region, year, sector, subsector) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Replace electricity emissions with indirect emissions
        total_emissions <- direct_emissions %>%
            dplyr::bind_rows(elec_ghg) %>%
            dplyr::left_join(trn_subsector_map, by = c("sector", "subsector")) %>%
            dplyr::filter(end_use == 'Transportation')

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)
        total_emissions <- region_agg(total_emissions, region, agg_region, add_global)

        # Convert Passenger service output to vehicle km
        en <- allqueries$'Energy consumption by subsector' %>%
            dplyr::filter(grepl("trn_", sector),
                          subsector != "Walk",
                          subsector != "Cycle") %>%
            dplyr::group_by(Units, scenario, region, sector, subsector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        en <- filter(en, years, filters)
        en <- aggregate(en, aggfn, aggkeys)
        en <- region_agg(en, region, agg_region, add_global)

        # Add in service output to emissions, which will ignore H2 forecourt production
        emissions_intensity <- total_emissions %>%
            dplyr::inner_join(en, by = c("scenario", "region", "sector", "subsector", "year")) %>%
            dplyr::group_by(Units.x, scenario, region, year, Units.y) %>%
            dplyr::summarise(value.x = sum(value.x), value.y = sum(value.y)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        pat <- '(\\w+)+ *(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,3], mmat[2,3]) *
            unitconv_energy(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(emissions_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            emissions_intensity
        }
    }
}
