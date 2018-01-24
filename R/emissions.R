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
                                         filters, ounit)
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
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'GHG emissions by subsector'
    }
    else {
        message('Function for processing variable: GHG emissions by subsector')

        ghg <- allqueries$'GHG emissions by subsector' %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP) %>%
            # Add in gas type
            dplyr::left_join(ghg_gas_type, by = 'ghg')

        ghg <- filter(ghg, years, filters)
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
                                     filters, ounit)
{
  if(mode == GETQ) {
    # Return titles of necessary queries
    # For more complex variables, will return multiple query titles in vector
    'GHG emissions by subsector'
  }
  else {
    message('Function for processing variable: GHG emissions by subsector')

    ghg <- allqueries$'GHG emissions by subsector' %>%
      # Add in GWP, and remove gases without GWP
      dplyr::right_join(gwp_ar5, by = c('ghg', 'Units')) %>%
      # Convert to MTCO2e
      dplyr::mutate(value = value * GWP,
                    Units = 'MTCO2e') %>%
      dplyr::select(-GWP) %>%
      # Add in gas type
      dplyr::left_join(ghg_gas_type, by = 'ghg')

    ghg <- filter(ghg, years, filters)
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
                                     filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Final energy by detailed end-use sector and fuel')
    }
    else {
        message('Function for processing variable: GHG emissions intensity by region')

        # Aggregate emissions and energy by region/year
        ghg <- allqueries$'GHG emissions by subsector' %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)

        final_energy <- allqueries$'Final energy by detailed end-use sector and fuel' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)

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
                                                  filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'Population')
    }
    else {
        message('Function for processing variable: GHG emissions per capita by region')

        # Aggregate emissions by region/year
        ghg <- allqueries$'GHG emissions by subsector' %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)

        pop <- allqueries$'Population' %>%
            aggregate(aggfn, aggkeys)

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
        pat <- '(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

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
                                            filters, ounit){
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by subsector', 'GDP(MER)')
    }
    else {
        message('Function for processing variable: GHG emissions per GDP by region')

        # Aggregate emissions by region/year
        ghg <- allqueries$'GHG emissions by subsector' %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            aggregate(aggfn, aggkeys)

        gdp <- allqueries$'GDP(MER)' %>%
            aggregate(aggfn, aggkeys)

        if ('region' %in% names(gdp) & 'region' %in% names(ghg)){
            ghg_gdp <- ghg %>%
                dplyr::left_join(gdp, by = c('scenario', 'region', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, region, year, value)
        } else {
            ghg_gdp <- ghg %>%
                dplyr::left_join(gdp, by = c('scenario', 'year')) %>%
                dplyr::mutate(Units = paste0(Units.x, "/", Units.y)) %>%
                dplyr::mutate(value = value.x / value.y) %>%
                dplyr::select(Units, scenario, year, value)
        }


        ghg_gdp <- filter(ghg_gdp, years, filters)

        if(is.na(ounit))
            return(ghg_gdp)

        iunit <- ghg_gdp$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE) *
            unitconv_usdollar(mmat[1,3], mmat[2,3], inverse=TRUE)

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
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

        all_ghg <- ghg %>%
            dplyr::bind_rows(co2) %>%
            dplyr::select(-rundate) %>%
            # Inner join gets rid of CO2 from the following categories: delivered biomass, delivered gas, refined liquids enduse,
            # refined liquids industrial, and wholesale gas. These are all practically zero.
            dplyr::inner_join(ghg_sector, by = 'sector')

        all_ghg <- filter(all_ghg, years, filters) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP)

        all_ghg <- aggregate(all_ghg, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_counts(all_ghg$Units[1], ounit)
            if(!is.na(cfac)) {
                all_ghg$value <- all_ghg$value *cfac
                all_ghg$Units <- ounit
            }
        }
        all_ghg
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
                                            filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector', 'Central electricity demand by demand sector')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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

        all_ghg <- filter(all_ghg, years, filters) %>%
            # Add in GWP, and remove gases without GWP
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
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
            dplyr::select(Units, scenario, region, year, value = indirect_emissions, end_use)

        indirect_emissions <- aggregate(indirect_emissions, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_counts(indirect_emissions$Units[1], ounit)
            if(!is.na(cfac)) {
                indirect_emissions$value <- indirect_emissions$value *cfac
                indirect_emissions$Units <- ounit
            }
        }
        indirect_emissions
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
                                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector', 'Central electricity demand by demand sector')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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
            dplyr::right_join(gwp_ar4, by = c('ghg', 'Units')) %>%
            # Convert to MTCO2e
            dplyr::mutate(value = value * GWP,
                          Units = 'MTCO2e') %>%
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

        total_emissions <- filter(total_emissions, years, filters)
        total_emissions <- aggregate(total_emissions, aggfn, aggkeys)


        if(!is.na(ounit)) {
            cfac <- unitconv_counts(total_emissions$Units[1], ounit)
            if(!is.na(cfac)) {
                total_emissions$value <- total_emissions$value *cfac
                total_emissions$Units <- ounit
            }
        }
        total_emissions
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
                                                  filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector',
          'Central electricity demand by demand sector', 'Final energy by detailed end-use sector and fuel')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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
                          Units = 'MTCO2e') %>%
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
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+)+ */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_energy(mmat[1,3], mmat[2,3], inverse=TRUE)

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
                                            filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector', 'Electricity')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

        electricity <- allqueries$'Electricity' %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

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
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP) %>%
            dplyr::filter(end_use == 'Electricity') %>%
            dplyr::group_by(Units, scenario, region, year, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

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
        pat <- '(\\w+)+ */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_energy(mmat[1,3], mmat[2,3], inverse=TRUE)

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
                                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector',
          'Energy consumption by sector', 'Building floorspace')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        # CO2 emissions by enduse sector
        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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
                          Units = 'MTCO2e') %>%
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
                          Units = 'MTCO2e') %>%
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

        # Resid and comm floorspace
        flrspace <- allqueries$'Building floorspace' %>%
            dplyr::select(-nodeinput, -`building-node-input`, -rundate)

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
        pat <- '(\\w+)+ */ *(\\w+)|(\\d)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

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
                                                filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector',
          'Energy consumption by sector', 'Population')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        # CO2 emissions by enduse sector
        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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
                          Units = 'MTCO2e') %>%
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

        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)

        emissions_per_capita <- total_emissions %>%
            dplyr::left_join(pop, by = c("scenario", "region", "year")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        emissions_per_capita <- filter(emissions_per_capita, years, filters)

        if(is.na(ounit))
            return(emissions_per_capita)

        iunit <- emissions_per_capita$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

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

#' GHG Emissions Per Pass-km Data Module
#'
#' Produce ghg emissions per pass-km - includes indirect emissions from electricity and biomass CO2 accounting.
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
module.ghg_per_pass_km <- function(mode, allqueries, aggkeys, aggfn, years,
                                      filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector',
          'Energy consumption by sector', 'Transportation Service Output')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        # CO2 emissions by enduse sector
        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP) %>%
            dplyr::group_by(Units, scenario, region, year, sector, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Electricity GHG emissions
        elec_ghg <- all_ghg %>%
            dplyr::filter(end_use == 'Electricity') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(elec = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- elec_ghg %>%
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value * elec,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, sector, value = indirect_emissions, end_use) %>%
            dplyr::group_by(Units, scenario, region, sector, end_use, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(end_use == 'Transportation')

        # Replace electricity emissions with indirect emissions
        total_transport_emissions <- all_ghg %>%
            dplyr::filter(end_use == 'Transportation') %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, sector, end_use, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Passenger service output
        pass_so <- trans_filter_svc(allqueries['Transportation Service Output'], TRUE)$'Transportation Service Output' %>%
            dplyr::group_by(Units, scenario, region, sector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Freight service output
        freight_so <- trans_filter_svc(allqueries['Transportation Service Output'], FALSE)$'Transportation Service Output' %>%
            dplyr::group_by(Units, scenario, region, sector, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Add in service output to emissions, which will ignore H2 forecourt production
        emissions_intensity <- total_transport_emissions %>%
            dplyr::inner_join(dplyr::bind_rows(pass_so, freight_so), by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        emissions_intensity <- filter(emissions_intensity, years, filters)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

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
                                   filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('GHG emissions by sector', 'Net Zero Bio CO2 Emissions by Sector',
          'Energy consumption by sector', 'Cement production by region')
    }
    else {
        message('Function for processing variable: GHG emissions by sector')

        # We need to filter out the CO2 emissions from ghg and add them in from the co2 query to get them attributed to the correct sector
        ghg <- allqueries$'GHG emissions by sector' %>%
            dplyr::filter(ghg != 'CO2')

        # CO2 emissions by enduse sector
        co2 <- allqueries$'Net Zero Bio CO2 Emissions by Sector' %>%
            dplyr::mutate(ghg = 'CO2') %>%
            dplyr::rename(sector = `primary fuel`)

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
                          Units = 'MTCO2e') %>%
            dplyr::select(-GWP) %>%
            dplyr::group_by(Units, scenario, region, year, sector, end_use) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Electricity GHG emissions
        elec_ghg <- all_ghg %>%
            dplyr::filter(end_use == 'Electricity') %>%
            dplyr::group_by(Units, scenario, region, year) %>%
            dplyr::summarise(elec = sum(value)) %>%
            dplyr::ungroup()

        # Pull out electricity emissions in order to split between end use sectors
        indirect_emissions <- elec_ghg %>%
            dplyr::right_join(elec_split, by = c('scenario', 'region', 'year')) %>%
            dplyr::mutate(indirect_emissions = value * elec,
                          Units = Units.x) %>%
            dplyr::select(Units, scenario, region, year, sector, value = indirect_emissions, end_use) %>%
            dplyr::group_by(Units, scenario, region, sector, end_use, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(sector == 'cement')

        # Replace electricity emissions with indirect emissions
        total_cement_emissions <- all_ghg %>%
            dplyr::filter(sector == 'cement') %>%
            dplyr::bind_rows(indirect_emissions) %>%
            # aggregate direct and indirect emissions
            dplyr::group_by(Units, scenario, region, sector, end_use, year) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        # Passenger service output
        cement_prod <- allqueries$'Cement production by region' %>%
            dplyr::select(-rundate)

        # Add in service output to emissions, which will ignore H2 forecourt production
        emissions_intensity <- total_cement_emissions %>%
            dplyr::inner_join(cement_prod, by = c("scenario", "region", "sector", "year")) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        emissions_intensity <- filter(emissions_intensity, years, filters)

        if(is.na(ounit))
            return(emissions_intensity)

        iunit <- emissions_intensity$Units[1]
        ## This doesn't currently allow for converting m^2
        pat <- '(\\w+)+ */ *(\\w+)+ *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_mass(mmat[1,2], mmat[2,2]) *
            unitconv_co2(mmat[1,2], mmat[2,2]) *
            unitconv_mass(mmat[1,3], mmat[2,3], inverse=TRUE)

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
