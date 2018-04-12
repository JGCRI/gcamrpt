#### Data modules for the transprtation group

#' Transportation data modules
#'
#' Produce transportation variables by service, mode, submode, fuel, and
#' vintage.
#'
#' The transportation representation in GCAM is devilishly complicated, due to
#' the pass-through sectors needed to sidestep the model's requirement that
#' sectors have (exactly) three levels of detail.   As a result, GCAM's normal
#' designations of "sector", "subsector", and "technology" aren't really
#' meaningful for transportation.  A quantity represented as a sector could be
#' spliced in underneath another sector, for example.
#'
#' Each transportation data module starts with a call to the
#' \code{trans_standardize} function, which translates all of the GCAM output
#' variables to the standard taxonomy.  Each module also comes in a passenger
#' and a freight variant.  This is necessary because although the procedure for
#' producing the data is the same for each variant, there are some subtle
#' differences, especially where units are concerned that make them
#' incompatible.  The parts of each module that \emph{are} compatible are
#' factored into a single worker function that is called by both variants
#'
#' @inheritParams runModule
#'
#' @name trans_modules
#' @keywords internal
NULL

#' @describeIn trans_modules Passenger transportation service output data module
module.pass_trans_service_output <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries <- trans_filter_svc(allqueries[queries], TRUE)
        process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, ounit) %>%
            region_agg(region, agg_region, add_global)
    }
}

#' @describeIn trans_modules Freight transportation service output data module
module.frgt_trans_service_output <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries <- trans_filter_svc(allqueries[queries], FALSE)
        process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, ounit) %>%
            region_agg(region, agg_region, add_global)
    }
}

#' @describeIn trans_modules Passenger transportation final energy module
module.pass_trans_final_energy <- function(mode, allqueries, aggkeys, aggfn, years,
                                           filters, ounit, region, agg_region, add_global)
{
    queries <- c('Transportation Final Energy', 'Refined Liquids')
    if(mode == GETQ) {
        queries
    }
    else {
        ## The refined liquids table isn't service-specific
        transqueries <-
            trans_filter_svc(allqueries['Transportation Final Energy'], TRUE)
        allqueries <- c(transqueries, allqueries['Refined Liquids'])
        process.tr_fe_output(allqueries, aggkeys, aggfn, years, filters, ounit) %>%
            region_agg(region, agg_region, add_global)
    }
}

#' @describeIn trans_modules Freight transportation final energy module
module.frgt_trans_final_energy <- function(mode, allqueries, aggkeys, aggfn, years,
                                           filters, ounit, region, agg_region, add_global)
{
    queries <- c('Transportation Final Energy', 'Refined Liquids')
    if(mode == GETQ) {
        queries
    }
    else {
        ## The refined liquids table isn't service-specific
        transqueries <-
            trans_filter_svc(allqueries['Transportation Final Energy'], FALSE)
        allqueries <- c(transqueries, allqueries['Refined Liquids'])
        process.tr_fe_output(allqueries, aggkeys, aggfn, years, filters, ounit) %>%
            region_agg(region, agg_region, add_global)
    }
}


#' @describeIn trans_modules Passenger transportation service intensity module
module.pass_trans_service_intensity <- function(mode, allqueries, aggkeys, aggfn, years,
                                                filters, ounit, region, agg_region, add_global)
{
    queries <- c('Transportation Final Energy', 'Transportation Service Output',
                 'Refined Liquids')
    if(mode == GETQ) {
        queries
    }
    else {
        ## silence notes on package check
        value <- NULL

        ## Refined liquids isn't a transportation-specific query, and so can't
        ## be filtered by service
        transqueries <- trans_filter_svc(allqueries[queries[1:2]], TRUE)
        allqueries <- c(transqueries, allqueries['Refined Liquids'])
        allqueries <- lapply(allqueries, region_agg, region, agg_region, add_global)
        svc_intensity <- process.tr_svc_intensity(allqueries, aggkeys, aggfn, years, filters,
                                                  ounit, 'EJ / million pass-km')

        if(is.na(ounit))
            ## no unit conversion to do.
            return(svc_intensity)

        iunit <- svc_intensity$Units[1]
        ## To convert this compound unit, we have to split into energy, count,
        ## and service components (e.g., EJ, million, and pass-km).  This is
        ## complex because the count is optional.
        ##
        ## The pattern to accomplish this is:
        ##   (\\w+) *              # 1. One or more word constituent characters,
        ##                         #    followed by any number of spaces
        ##                         #    (including 0)
        ##   / *                   # 2. A slash followed by any number of spaces
        ##  (\\w+)? +              # 3. An optional group comprising any number of
        ##                         #    word constituents, followed by one or more
        ##                         #    spaces
        ##  (\\w+) *- *            # 4. Another word, followed by any amount of space,
        ##                         #    followed by a hyphen, followed by any
        ##                         #    amount of space
        ##  (\\w+)                 # 5. Another word.
        ## The parentheses caused all of the word groups to be captured in the
        ## matrix returned by the call
        pat <- '(\\w+) */ *(\\w+)? +(\\w+) *- *(\\w+)'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        ## column 3 is the second group, which is optional.  If it's absent,
        ## we'll get a NA there.  If that happens, replace it with an empty
        ## string
        mmat[is.na(mmat[,3]), 3] <- ''
        ## Now use column 2 for energy conversion and column 3 for count conversion
        ## (inverted).  Theoretically we could convert the length unit as well,
        ## but that isn't implemented, so check to see that they're equal and
        ## issue a warning if not.  We don't do anything with 'pass-', since
        ## there is nothing to convert it to (for freight, we will convert the
        ## mass.
        cfac <-
            unitconv_energy(mmat[1,2], mmat[2,2]) *
              unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE)

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
            dplyr::mutate(svc_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            svc_intensity
        }
    }
}

#' @describeIn trans_modules Passenger transportation service intensity module
module.frgt_trans_service_intensity <- function(mode, allqueries, aggkeys, aggfn, years,
                                                filters, ounit, region, agg_region, add_global)
{
    queries <- c('Transportation Final Energy', 'Transportation Service Output',
                 'Refined Liquids')
    if(mode == GETQ) {
        queries
    }
    else {
        ## silence notes on package check
        value <- NULL

        ## Refined liquids isn't a transportation-specific query, and so can't
        ## be filtered by service
        transqueries <- trans_filter_svc(allqueries[queries[1:2]], FALSE)
        allqueries <- c(transqueries, allqueries['Refined Liquids'])
        allqueries <- lapply(allqueries, region_agg, region, agg_region, add_global)
        svc_intensity <- process.tr_svc_intensity(allqueries, aggkeys, aggfn, years, filters,
                                                  ounit, 'EJ / million tonne-km')

        if(is.na(ounit))
            return(svc_intensity)

        iunit <- svc_intensity$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) */ *(\\w+)? +(\\w+) *- *(\\w+)'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_energy(mmat[1,2], mmat[2,2]) *
              unitconv_counts(mmat[1,3], mmat[2,3], inverse=TRUE) *
              unitconv_mass(mmat[1,4], mmat[2,4], inverse=TRUE)

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
            dplyr::mutate(svc_intensity, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            svc_intensity
        }
    }
}

#' @describeIn trans_modules Passenger transportation load factor data module
module.pass_trans_load_factor <- function(mode, allqueries, aggkeys, aggfn, years,
                                           filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Load Factors'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries <- trans_filter_svc(allqueries[queries], TRUE)
        ## GCAM reports the units for both passenger and freight transportation
        ## as "load / veh", but "load" actually has some kind of unit, probably
        ## "passengers"
        allqueries[['Transportation Load Factors']]$Units <- 'pass / veh'
        process.trans_load_factors(allqueries, aggkeys, aggfn, years, filters,
                                   ounit) %>%
            region_agg(region, agg_region, add_global)
        ## no unit conversions for passenger load factors; passengers / vehicle
        ## is the only sensible unit.
    }
}

#' @describeIn trans_modules Freight transportation load factor data module
module.frgt_trans_load_factor <- function(mode, allqueries, aggkeys, aggfn, years,
                                           filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Load Factors'
    if(mode == GETQ) {
        queries
    }
    else {
        ## silence notes on package check
        value <- NULL

        allqueries <- trans_filter_svc(allqueries[queries], FALSE)
        ## GCAM reports the units for both passenger and freight transportation
        ## as "load / veh", but "load" actually has some kind of unit, probably
        ## "tonnes"
        allqueries[['Transportation Load Factors']]$Units <- 'tonnes / veh'
        lf <- process.trans_load_factors(allqueries, aggkeys, aggfn, years, filters,
                                         ounit) %>%
            region_agg(region, agg_region, add_global)
        if(!is.na(ounit)) {
            cf <- unitconv_mass(lf$Units[1], ounit)
            if(!is.na(cf)) {
                lf <- dplyr::mutate(lf, value=value*cf, Units=ounit)
            }
        }
        lf
    }
}


#' @describeIn trans_modules Worker function for transportation service output modules
process.tr_svc_output <- function(allqueries, aggkeys, aggfn, years,
                                  filters, ounit)
{
    ## silence notes on package check
    Units <- scenario <- region <- year <- technology <- vintage <- service <-
        submode <- value <- NULL

    message('Function for processing variable: Transportation service output')
    serviceOutput <- allqueries$'Transportation Service Output'
    serviceOutput <- trans_standardize(serviceOutput) %>%
      dplyr::group_by(Units, scenario, region, year, technology, vintage, service, mode, submode) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup()
    serviceOutput <- filter(serviceOutput, years, filters)
    serviceOutput <- aggregate(serviceOutput, aggfn, aggkeys)

    ## units example: million p-km
    if(!is.na(ounit)) {
        iunit <- serviceOutput$Units[1]
        pat <- '(\\w+) *(\\w+-\\w+) *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_counts(mmat[1,2], mmat[2,2])
        if(!is.na(cfac)) {
            serviceOutput <- dplyr::mutate(serviceOutput, value=value*cfac,
                                           Units=ounit)
        }
    }
    serviceOutput
}

#' @describeIn trans_modules Worker function for service output modules
process.tr_fe_output <- function(allqueries, aggkeys, aggfn, years,
                                 filters, ounit)
{
    ## silence notes on package checks
    Units <- scenario <- region <- year <- technology <- vintage <- fuel <-
        liquid_type <- service <- submode <- value <- NULL

    message('Function for processing variable: Final energy')

    energy <- allqueries$'Transportation Final Energy'
    refining <- allqueries$'Refined Liquids'
    energy <- mapfuel(energy, refining) #replaces input/technology with fuel & liquid_type
    energy <- trans_standardize(energy) %>%
      dplyr::group_by(Units, scenario, region, year, technology, vintage, fuel, liquid_type, service, mode, submode) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup()
    energy <- filter(energy, years, filters)
    energy <- aggregate(energy, aggfn, aggkeys)
    ## units example: EJ/yr
    if(!is.na(ounit)) {
        cf <- unitconv_energy(energy$Units[1], ounit)
        if(!is.na(cf)) {
            energy <- dplyr::mutate(energy, value=value*cf, Units=ounit)
        }
    }
    energy
}


#' @describeIn trans_modules Worker function for load factor modules
process.trans_load_factors <- function(allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    ## silence notes on package check
    Units <- scenario <- region <- year <- technology <- service <- submode <-
        value <- NULL

    message('Function for processing variable: Load factors')

    ldfctr <- allqueries$'Transportation Load Factors'
    ldfctr <- trans_standardize(ldfctr) %>%
      dplyr::group_by(Units, scenario, region, year, technology, service, mode, submode) %>% # average over LHDT
      dplyr::summarise(value=mean(value)) %>%
      dplyr::ungroup()
    ldfctr <- filter(ldfctr, years, filters)
    ldfctr <- aggregate(ldfctr, aggfn, aggkeys)
    ## We don't do any unit conversion here because the allowable conversions
    ## will differ between passenger and freight services.
    ldfctr
}

#' @describeIn trans_modules Worker function for transportation service
#' intensity
process.tr_svc_intensity <- function(allqueries, aggkeys, aggfn, years,
                                     filters, ounit, nativeunit)
{
    ## silence notes on package check
    Units <- scenario <- region <- year <- service <- submode <- value <-
        fuel <- liquid_type <- value.x <- value.y <- svc <- Units.x <-
            Units.y <- intensity <- NULL

    message('Function for processing variable: Transportation service intensity')

    ## filter and aggregate component queries, then calculate final variable.
    ## We can get away with this because these have all the same variables
    serviceOutput <- allqueries$'Transportation Service Output'
    serviceOutput <- trans_standardize(serviceOutput) %>%
      dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup()

    energy <- allqueries$'Transportation Final Energy'
    refining <- allqueries$'Refined Liquids'
    energy <- mapfuel(energy, refining) #replaces input/technology with fuel & liquid_type
    energy <- trans_standardize(energy) %>%
      dplyr::group_by(Units, scenario, region, year, service, mode, submode, fuel, liquid_type) %>%
      dplyr::summarise(value=sum(value)) %>%
      dplyr::ungroup()

    ## We need to filter and aggregate *before* we calculate the quotient.  This
    ## will ensure that we are dividing *total* energy by *total* service
    ## output across the aggregation categories.  However, to do this we have to
    ## make sure that we have all the id columns in a single table, since the
    ## filters may mention them.

    energy %>%
      dplyr::inner_join(serviceOutput,
                        by = c('scenario', 'region', 'year', 'service', 'mode',
                        'submode')) %>%
      filter(years, filters) %>%
      aggregate(aggfn, aggkeys, multiple=TRUE) %>%
      ## Calculate service intensity as energy / service output
      dplyr::rename(energy=value.x, svc=value.y) %>%
      dplyr::mutate(intensity = energy/svc, Units=nativeunit) %>%
      dplyr::select(-svc, -Units.x, -energy, -Units.y)  %>%
      dplyr::rename(value=intensity)
}


#' @describeIn trans_modules Vehicle sales
#'
#' This module appears to be incomplete.
module.ldv_sales <- function(mode, allqueries, aggkeys, aggfn, years,
                         filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        ## silence notes on package check
        value <- year <- vintage <- Units <- scenario <- service <-
            submode <- value.x <- value.y <- pkm <- lf <- Units.x <- Units.y <-
                vkm <- mileage <- NULL

        serviceOutput <- allqueries$'Transportation Service Output' %>%
            tidyr::separate(technology, c('technology', 'vintage'), ',year=') %>%
            dplyr::filter(year==vintage,
                          grepl('LDV', sector)) %>%
            dplyr::select(-vintage, -rundate)

        # load factor needs to be updated for all BEV
        lf <- load_factor

        vkm <- serviceOutput %>%
            # inner join should only get rid of pass-through categories
            dplyr::inner_join(lf, by = c("region", "sector", "subsector", "technology", "year")) %>%
            dplyr::mutate(vkm = value / loadFactor,
                          Units = 'million vehicle-km') %>%
            dplyr::select(-value, -loadFactor)

        sales <- vkm %>%
            dplyr::inner_join(annual_mileage, by = c("region", "subsector", "year")) %>%
            dplyr::mutate(sales = vkm / value,
                          Units = 'million vehicles') %>%
            dplyr::select(Units, scenario, region, sector, subsector, technology, year, value = sales) %>%
            dplyr::mutate(sector = substr(sector, nchar(sector)-1, nchar(sector)),
                          sector = if_else(sector == 'DV', '3W', sector))

        sales <- filter(sales, years, filters)
        sales <- aggregate(sales, aggfn, aggkeys)
        sales <- region_agg(sales, region, agg_region, add_global)

        ## units example: million p-km
        if(!is.na(ounit)) {
            cf <- unitconv_counts(sales$Units[1], ounit)
            if(!is.na(cf)) {
                sales <- dplyr::mutate(sales, value=value*cf, Units=ounit)
            }
        }
        sales
    }
}


#' Fuel
#'
#' Using input col of energy query, create fuel col. Remove input col.
#' Using bio share of total liquids refining, calculate liquids energy from biomass vs traditional.
#' All other fuel sources remain unchanged.
#'
#' @param en Data returned for final energy query
#' @param ref Data returned for refined liquids query
#' @keywords internal
mapfuel <- function(en, ref = NULL) {
    ## silence notes on package checks.
    input <- fuel <- scenario <- region <- year <- value <- total <-
        subsector <- Units <- share <- sector <- technology <- liquid_type <-
            NULL


    # replace input col with fuel col, else add blank fuel col
    if ('input' %in% names(en)) {
        en$input <- tolower(en$input)
        en <- en %>%
            dplyr::mutate(fuel='') %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('coal', input), 'Coal', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('gas', input), 'Natural Gas', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('elec', input), 'Electricity', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('h2', input), 'Hydrogen', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('liquids', input), 'Liquids', fuel))

        ## Refined Liquids
        # aggregate for total
        tot_refining <- ref %>%
            dplyr::group_by(scenario, region, year) %>%
            dplyr::summarise(total = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::select(scenario, region, year, total)
        # subset to biomass -- bioethanol vs biodiesel??
        bio_refining <- ref[ref$subsector == 'biomass liquids', ] %>%
            dplyr::select(-subsector) %>%
            dplyr::inner_join(tot_refining, by=c("scenario", "region", "year")) %>%
            dplyr::mutate(share = value/total) %>%
            dplyr::select(-value, -total, -Units)

        # break out biomass liquids and traditional liquids
        liq <- en[en$fuel =='Liquids', ]
        bioliq <- liq %>%
            dplyr::mutate(liquid_type = 'biomass') %>%
            dplyr::inner_join(bio_refining, by=c("scenario", "region", "year")) %>%
            dplyr::mutate(value=value*share) %>%
            dplyr::select(-share) %>%
            dplyr::select(Units, scenario, region, sector, subsector, technology, input, year, value, fuel, liquid_type)
        nonbioliq <- liq %>%
            dplyr::mutate(liquid_type='traditional') %>%
            dplyr::inner_join(bio_refining, by=c("scenario", "region", "year")) %>%
            dplyr::mutate(value=value*(1-share)) %>% # subtract biomass liquids
            dplyr::select(-share) %>%
            dplyr::select(Units, scenario, region, sector, subsector, technology, input, year, value, fuel, liquid_type)

        # replace rows where fuel==liquids
        liq <- rbind(bioliq, nonbioliq)
        nonliq <- en %>%
            dplyr::filter(fuel != 'Liquids') %>%
            dplyr::mutate(liquid_type='') %>%
            dplyr::select(Units, scenario, region, sector, subsector, technology, input, year, value, fuel, liquid_type)
        en <- rbind(liq, nonliq)
        en
    } else {
        en$fuel <- ''
        en$liquid_type <- ''
        en
        }
}

#' Produce particulate matter emissions from transportation
#'
#' Convert service output in passenger-km or tonne-km to vehicle-km using the
#' load factor table.  Then apply a table of emissions coefficients to get
#' emissions.  Note this doesn't actually use any emissions generated from
#' GCAM.
#'
#' @keywords internal
module.transportation_pm_emissions <- function(mode, allqueries, aggkeys, aggfn, years,
                                               filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Transportation Service Output', 'Transportation Load Factors')
    }
    else {
        ## silence notes on package check
        Units <- scenario <- year <- service <- submode <- value <-
            value.x <- value.y <- pkm <- lf <- Units.x <- Units.y <- pmfac <-
                pm_emissions <- NULL

        warning('Function for processing variable: vehicle PM emissions is not yet implemented.')
        return(NULL)
        ## everything that follows is a work in progress.
        # data prep
        serviceOutput <- allqueries$'Service output'
        serviceOutput <- trans_standardize(serviceOutput) %>%
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        ldfctr <- allqueries$'Load factors'
        ldfctr <- trans_standardize(ldfctr) %>%
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
        pm <- region_agg(pm, region, agg_region, add_global)

        if(!is.na(ounit)) {
            cf <- unitconv_mass(pm$Units[1], ounit)
            if(!is.na(cf)) {
                pm <- dplyr::mutate(pm, value=value*cf, Units=ounit)
            }
        }
        pm
    }
}


### service mapping
### These will be used in a couple of functions below
## Sectors denoting passenger transportation segments:
passengerSectors <- c('trn_aviation_intl', 'trn_pass', 'trn_pass_road',
                      'trn_pass_road_ldv', 'trn_pass_road_ldv_2w', 'trn_pass_road_ldv_4w')
## Sectors denoting freight transportation segments:
freightSectors <- c('trn_freight', 'trn_freight_road', 'trn_shipping_intl')

#' Filter raw queries by passenger services
#'
#' @param qlist List of queries relevant to the module at hand (i.e., pass only
#' the queries that the module will use.)
#' @param passenger If \code{TRUE}, filter to passenger services; if
#' \code{FALSE} filter to freight services.
#' @keywords internal
trans_filter_svc <- function(qlist, passenger=TRUE)
{
    if(passenger) {
        sectors <- passengerSectors
    }
    else {
        sectors <- freightSectors
    }
    lapply(qlist, function(d) {
               mask <- tolower(d$sector) %in% sectors
               d[mask,]
           })
}


#' Standardize transportation query dataframes
#'
#' Convert the GCAM representation using a hodgepodge of sectors, subsectors,
#' and technologies into the standard transportation taxonomy of service, mode,
#' submode, fuel, and vintage.
#'
#' Columns removed (no mapping): rundate, load-factor
#' Columns removed (after mapping) : sector, subsector
#' Technologies removed: adv-electric, adv-liquid, tech-adv-electric, and
#' tech-adv-liquid (only appears in load factors)
#' Columns aggregated over : technology, vintage -- include in dplyr::group_by() if needed later
#'
#' Final, standardized set of columns : Units, scenario, region, year, service,
#' mode, submode, fuel, liquid_type If this set changes, need to adjust call to
#' group_by() below All other cols are either dropped or aggregated over
#'
#' @param queryData Data returned for one variable query
#' @keywords internal
trans_standardize <- function(queryData) {
    ## silence notes on package check
    rundate <- technology <- sector <- subsector <- service <- submode <-
        scenario <- region <- value <- Units <- NULL

    if ('rundate' %in% names(queryData)) {queryData <- dplyr::select(queryData, -rundate)} # remove rundate
    if ('load-factor' %in% names(queryData)) {queryData <- queryData[,!(names(queryData) %in% c("load-factor"))]} # query always includes col of NA's
    if ('sector' %in% names(queryData)) {
        queryData$sector <- tolower(queryData$sector)
        } else {queryData$sector <- ''}
    if ('subsector' %in% names(queryData)) {
        queryData$subsector <- tolower(queryData$subsector)
        queryData$subsector <- gsub('[()]', '', queryData$subsector)
    } else {queryData$subsector <- ''}


    # include tech + vint cols
    if('technology' %in% names(queryData)) {
        queryData <- dplyr::filter(queryData, ! grepl('Adv', technology))# only appears in load factors query
        if(grepl(',year=', queryData$technology[1])) { # split vint/tech
            queryData <- tidyr::separate(queryData, technology, c("technology", "vintage"), ",year=")
        } else {queryData$vintage <- ''} # include vint col if not incl'd in tech
    } else { # if neither, add both
        queryData$technology <- ''
        queryData$vintage <- ''
    }

    ## semi-aggregate if query returns data with missing branches (LDV_2W without LDV )
    #2w
    if ('2w' %in% levels(factor(queryData$subsector)) & 'trn_pass_road_ldv_2w' %in% levels(factor(queryData$sector))) {
        # trn_pass_road_ldv_2w is redundant and can be removed
        queryData <- dplyr::filter(queryData, sector != 'trn_pass_road_ldv_2w')
    } else if (! '2w' %in% levels(factor(queryData$subsector)) & 'trn_pass_road_ldv_2w' %in% levels(factor(queryData$sector))) {
        # trn_pass_road_ldv_2w needs to be mapped to sector=trn_pass_road_ldv & subsector=2w
        queryData <- dplyr::mutate(queryData, subsector = dplyr::if_else(sector == 'trn_pass_road_ldv_2w', '2w', subsector))
        queryData <- dplyr::mutate(queryData, sector = dplyr::if_else(sector == 'trn_pass_road_ldv_2w', 'trn_pass_road_ldv', sector))
    }
    #4w
    if ('4w' %in% levels(factor(queryData$subsector)) & 'trn_pass_road_ldv_4w' %in% levels(factor(queryData$sector))) {
        # trn_pass_road_ldv_4w is redundant and can be removed
        queryData <- dplyr::filter(queryData, sector != 'trn_pass_road_ldv_4w')
    } else if (! '4w' %in% levels(factor(queryData$subsector)) & 'trn_pass_road_ldv_4w' %in% levels(factor(queryData$sector))) {
        # trn_pass_road_ldv_4w needs to be mapped to sector=trn_pass_road_ldv & subsector=4w
        queryData <- dplyr::mutate(queryData, subsector = dplyr::if_else(sector == 'trn_pass_road_ldv_4w', '4w', subsector))
        queryData <- dplyr::mutate(queryData, sector = dplyr::if_else(sector == 'trn_pass_road_ldv_4w', 'trn_pass_road_ldv', sector))
    }

    # remove these rows
    removeSectors <- 'trn_pass_road_bus' # provided in sector trn_pass_road, subsector bus
    removeSubsectors <- c('road', 'cycle', 'walk', 'ldv') # too aggregated
    # mode mapping
    aviation <- tolower(c('International Aviation', 'Domestic Aviation'))
    rail <- tolower(c('Freight Rail', 'Passenger Rail', 'HSR'))
    road <- tolower(c('truck 0-2t', 'truck 2-5t', 'truck 5-9t', 'truck 9-16t', 'truck >15t', 'truck 0-1t', 'truck 6-15t',
                      'truck 1-6t', 'truck 6-30t', 'truck 0-4.5t', 'truck 4.5-15t', 'truck >32t', 'truck 0-3.5t',
                      'truck 16-32t', 'truck 3.5-16t', '3w rural', 'truck >14t', 'truck 0-6t', 'truck 6-14t', 'truck',
                      'truck >12t', 'truck 0-2.7t', 'truck 2.7-4.5t', 'truck 4.5-12t', 'bus', '2W', '4W', 'Three-Wheeler'))
    shipping <- tolower(c('domestic ship', 'international ship'))
    # submode mapping
    lhdt <- c('truck 0-2t', 'truck 2-5t', 'truck 0-1t', 'truck 1-6t', 'truck 0-4.5t', 'truck 0-3.5t', 'truck 0-6t',
              'truck 0-2.7t', 'truck 2.7-4.5t')
    mhdt <- c('truck 5-9t', 'truck 6-15t', 'truck 6-30t', 'truck 4.5-15t', 'truck 3.5-16t', 'truck 6-14t', 'truck', 'truck 4.5-12t')
    hhdt <- c('truck 9-16t', 'truck >15t', 'truck >32t', 'truck 16-32t', 'truck >14t', 'truck >12t')

    queryData <- queryData %>%
        dplyr::filter(! sector %in% removeSectors) %>%
        dplyr::filter(! subsector %in% removeSubsectors) %>%
        dplyr::mutate(service='', mode='', submode='') %>%
        dplyr::mutate(service = dplyr::if_else(sector %in% passengerSectors, 'Passenger', service)) %>%
        dplyr::mutate(service = dplyr::if_else(sector %in% freightSectors, 'Freight', service)) %>%
        dplyr::mutate(mode = dplyr::if_else(subsector %in% aviation, 'Aviation', mode)) %>%
        dplyr::mutate(mode = dplyr::if_else(subsector %in% rail, 'Rail', mode)) %>%
        dplyr::mutate(mode = dplyr::if_else(subsector %in% road, 'Road', mode)) %>%
        dplyr::mutate(mode = dplyr::if_else(subsector %in% shipping, 'Shipping', mode)) %>%
        dplyr::mutate(submode = dplyr::if_else(grepl('domestic', subsector), 'Domestic', submode)) %>% # shiping + aviation
        dplyr::mutate(submode = dplyr::if_else(grepl('international', subsector), 'International', submode)) %>% # shipping + aviation
        dplyr::mutate(submode = dplyr::if_else(service=='Passenger' & mode=='Rail', 'Passenger Rail', submode)) %>% #HSR + passenger rail
        dplyr::mutate(submode = dplyr::if_else(service=='Freight' & mode=='Rail', 'Freight Rail', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='2w', '2W', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='three-wheeler', '3W', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='4w', '4W', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='bus', 'Bus', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector %in% lhdt, 'LHDT', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector %in% mhdt, 'MHDT', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector %in% hhdt, 'HHDT', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector == '3w rural', '3W Freight', submode)) %>%
        dplyr::filter( service != '' & mode != '' & submode != '' ) %>%
        dplyr::arrange(scenario, region, service, mode, submode, value, Units)


    queryData
}

#' @describeIn trans_modules Passenger transportation service output per capita data module
module.pass_trans_service_output_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- c('Transportation Service Output', 'Population')
    if(mode == GETQ) {
        queries
    }
    else {
        population <- allqueries$'Population' %>%
            dplyr::select(-rundate)
        allqueries <- trans_filter_svc(allqueries['Transportation Service Output'], TRUE)
        unitz <- allqueries$`Transportation Service Output`$Units[1]
        so <- process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, unitz)
        so <- region_agg(so, region, agg_region, add_global)

        # sum globally if needed
        if (!('region' %in% names(so))){
            population <- aggregate(population,'sum', 'scenario')
        }
        population <- region_agg(population, region, agg_region, add_global)

        # Columns to join by
        join_cats <- names(population)[!(names(population) %in% c('Units', 'value'))]

        # Units are everything before division sign if aggregation has ocurred
        iunit <- allqueries$`Transportation Service Output`$Units[1]

        so_cap <- so %>%
            dplyr::left_join(population, by = join_cats) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(iunit, '/', Units.y)) %>%
            dplyr::select(-Units.x, -Units.y, -value.x, -value.y)

        if(is.na(ounit))
            return(so_cap)

        iunit <- so_cap$Units[1]
        ## See notes above for unit conversion.  This is largely repeated from
        ## the passenger version, but there are a couple of wrinkles, so it
        ## would take more time than I have right now to refactor it.
        pat <- '(\\w+) *(\\w+-\\w+) */ *(\\w+) *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_counts(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(so_cap, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            so_cap
        }

    }
}

#' @describeIn trans_modules Passenger transportation fleet size data module
module.pass_trans_fleet <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries <- trans_filter_svc(allqueries[queries], TRUE)

        allqueries$`Transportation Service Output` <- allqueries$`Transportation Service Output` %>%
            tidyr::separate(technology, c("technology", "vintage"), ",year=") %>%
            dplyr::left_join(load_factor, by = c("region", "sector", "subsector", "technology", "year")) %>%
            # Remove modes without a load factor - these should all be pass-through sectors or cycle/walk
            dplyr::filter(!is.na(loadFactor)) %>%
            dplyr::mutate(value = value / loadFactor,
                          Units = 'million veh-km') %>%
            dplyr::select(-loadFactor) %>%
            dplyr::left_join(annual_mileage, by = c("region", "year", "subsector")) %>%
            # Remove modes without annual mileage - should only be aviation, rail, and bus
            dplyr::filter(!is.na(value.y)) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = 'million vehicles') %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        fleet <- process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, ounit)
        fleet <- region_agg(fleet, region, agg_region, add_global)


        if(!is.na(ounit)) {
            cfac <- unitconv_counts(fleet$Units[1], ounit)
            if(!is.na(cfac)) {
                fleet$value <- fleet$value *cfac
                fleet$Units <- ounit
            }
        }
        fleet
    }
}

#' @describeIn trans_modules Passenger transportation fleet size per person data module
module.pass_trans_fleet_per_capita <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit, region, agg_region, add_global)
{
    queries <- c('Transportation Service Output', 'Population')
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries['Transportation Service Output'] <- trans_filter_svc(allqueries['Transportation Service Output'], TRUE)

        allqueries$`Transportation Service Output` <- allqueries$`Transportation Service Output` %>%
            tidyr::separate(technology, c("technology", "vintage"), ",year=") %>%
            dplyr::left_join(load_factor, by = c("region", "sector", "subsector", "technology", "year")) %>%
            # Remove modes without a load factor - these should all be pass-through sectors or cycle/walk
            dplyr::filter(!is.na(loadFactor)) %>%
            dplyr::mutate(value = value / loadFactor,
                          Units = 'million veh-km') %>%
            dplyr::select(-loadFactor) %>%
            dplyr::left_join(annual_mileage, by = c("region", "year", "subsector")) %>%
            # Remove modes without annual mileage - should only be aviation, rail, and bus
            dplyr::filter(!is.na(value.y)) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = 'million vehicles') %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        fleet <- process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                                       filters, NA) %>%
            # units are not handled correctly by process.tr_svc_output, so set correctly here
            # are there any exceptions??
            dplyr::mutate(Units = allqueries$`Transportation Service Output`$Units[1])
        fleet <- region_agg(fleet, region, agg_region, add_global)


        pop <- allqueries$'Population' %>%
            dplyr::select(-rundate)

        # Figure out if we need to aggregate population by region
        if (!('region' %in% names(fleet))){
            pop <- aggregate(pop, aggfn, 'year')
        }

        pop <- region_agg(pop, region, agg_region, add_global)

        # Determine columns to join by
        join_cats <- names(pop)[!(names(pop) %in% c('Units', 'value'))]

        fleet_capita <- fleet %>%
            dplyr::left_join(pop, by = join_cats) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = paste0(Units.x,'/',Units.y)) %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        if(is.na(ounit))
            return(fleet_capita)

        iunit <- fleet_capita$Units[1]
        pat <- '(\\w+) *(\\w+)? */ *(\\w+) *(\\w+)? *'
        mmat <- stringr::str_match(c(iunit, ounit), pat)
        mmat[is.na(mmat[,3]), 3] <- ''
        cfac <-
            unitconv_counts(mmat[1,2], mmat[2,2]) *
            unitconv_counts(mmat[1,4], mmat[2,4], inverse=TRUE)

        if(!is.na(cfac)) {
            dplyr::mutate(fleet_capita, value=cfac*value, Units=ounit)
        }
        else {
            ## If any conversions failed, the warning will already have been
            ## issued, so just return the result unconverted.
            fleet_capita
        }
    }
}

#' @describeIn trans_modules Vehicle sales
#'
#' This module appears to be incomplete.
module.ldv_sales_fuel_prop <- function(mode, allqueries, aggkeys, aggfn, years,
                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        ## silence notes on package check
        value <- year <- vintage <- Units <- scenario <- service <-
            submode <- value.x <- value.y <- pkm <- lf <- Units.x <- Units.y <-
            vkm <- mileage <- NULL

        serviceOutput <- allqueries$'Transportation Service Output' %>%
            tidyr::separate(technology, c('technology', 'vintage'), ',year=') %>%
            dplyr::filter(year==vintage,
                          grepl('LDV', sector)) %>%
            dplyr::select(-vintage, -rundate)

        # load factor needs to be updated for all BEV
        lf <- load_factor

        vkm <- serviceOutput %>%
            # inner join should only get rid of pass-through categories
            dplyr::inner_join(lf, by = c("region", "sector", "subsector", "technology", "year")) %>%
            dplyr::mutate(vkm = value / loadFactor,
                          Units = 'million vehicle-km') %>%
            dplyr::select(-value, -loadFactor)

        sales <- vkm %>%
            dplyr::inner_join(annual_mileage, by = c("region", "subsector", "year")) %>%
            dplyr::mutate(sales = vkm / value,
                          Units = 'million vehicles') %>%
            dplyr::select(Units, scenario, region, sector, subsector, technology, year, value = sales) %>%
            dplyr::mutate(sector = substr(sector, nchar(sector)-1, nchar(sector)),
                          sector = dplyr::if_else(sector == 'DV', '3W', sector))

        sales <- aggregate(sales, aggfn, aggkeys)
        sales <- region_agg(sales, region, agg_region, add_global)

        if (!('technology' %in% names(sales))){
            warning('Technology not in LDV Sales')
        }

        group_cats <- names(sales)[!(names(sales) %in% c('value', 'technology'))]

        sales <- sales %>%
            dplyr::group_by_(.dots = group_cats) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage')

        sales <- filter(sales, years, filters)

        ## units example: million p-km
        if(!is.na(ounit)) {
            cf <- unitconv_counts(sales$Units[1], ounit)
            if(!is.na(cf)) {
                sales <- dplyr::mutate(sales, value=value*cf, Units=ounit)
            }
        }
        sales
    }
}

#' @describeIn trans_modules Passenger transportation service output data module
module.pass_vkm <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries[['Transportation Service Output']] <- trans_filter_svc(allqueries[queries], TRUE)$'Transportation Service Output' %>%
            tidyr::separate(technology, c('technology', 'vintage'), ',year=') %>%
            dplyr::inner_join(load_factor, by = c("region", "sector", "subsector", "technology", "year")) %>%
            dplyr::mutate(value = value / loadFactor,
                          Units = 'million vehicle-km') %>%
            dplyr::select(-loadFactor)


        process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, ounit) %>%
            region_agg(region, agg_region, add_global)

    }
}

#' @describeIn trans_modules Freight transportation shares data module
module.frgt_trans_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries <- trans_filter_svc(allqueries[queries], FALSE)
        freight <- process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, ounit)

        # Aggregate over vintages if necessary
        grp_cats <- names(freight)[!(names(freight) %in% c('vintage', 'value'))]

        freight <- freight %>%
            # Sum over vintages
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        freight <- region_agg(freight, region, agg_region, add_global)

        # This will only allow us to see percentage of mode or submode in service, not submode in mode
        grp_cats <- names(freight)[!(names(freight) %in% c('value', 'mode', 'submode', 'technology'))]

        freight_shares <- freight %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage')


    }
}

#' @describeIn trans_modules Passenger transportation shares public vs private (not including aviation) data module
module.pass_trans_public_share <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        # NOTE: this module is very specific, and may not be easily manipulable via control files
        allqueries <- trans_filter_svc(allqueries[queries], TRUE)
        shares <- process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                              filters, ounit) %>%
            dplyr::filter(mode != 'Aviation') %>%
            dplyr::mutate(transit_type = dplyr::if_else(mode == 'Rail' | submode == 'Bus', 'Public', 'Private')) %>%
            dplyr::group_by(Units, scenario, region, year, service, transit_type) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()
        shares <- region_agg(shares, region, agg_region, add_global)

        grp_cats <- names(shares)[!(names(shares) %in% c('value', 'transit_type'))]

        shares <- shares %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::mutate(value = 100 * value / sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage')

        shares
    }
}

#' @describeIn trans_modules Passenger transportation fleet fuel shares data module
module.pass_trans_fleet_shares <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit, region, agg_region, add_global)
{
    queries <- 'Transportation Service Output'
    if(mode == GETQ) {
        queries
    }
    else {
        allqueries <- trans_filter_svc(allqueries[queries], TRUE)

        allqueries$`Transportation Service Output` <- allqueries$`Transportation Service Output` %>%
            tidyr::separate(technology, c("technology", "vintage"), ",year=") %>%
            dplyr::left_join(load_factor, by = c("region", "sector", "subsector", "technology", "year")) %>%
            # Remove modes without a load factor - these should all be pass-through sectors or cycle/walk
            dplyr::filter(!is.na(loadFactor)) %>%
            dplyr::mutate(value = value / loadFactor,
                          Units = 'million veh-km') %>%
            dplyr::select(-loadFactor) %>%
            dplyr::left_join(annual_mileage, by = c("region", "year", "subsector")) %>%
            # Remove modes without annual mileage - should only be aviation, rail, and bus
            dplyr::filter(!is.na(value.y)) %>%
            dplyr::mutate(value = value.x / value.y,
                          Units = 'million vehicles') %>%
            dplyr::select(-value.x, -value.y, -Units.x, -Units.y)

        fleet <- process.tr_svc_output(allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
        fleet <- region_agg(fleet, region, agg_region, add_global)

        grp_cats <- names(fleet)[!(names(fleet) %in% c('technology', 'value'))]

        fleet <- fleet %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::mutate(value = round(100 * value / sum(value), 2)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Units = 'Percentage')

        fleet
    }
}
