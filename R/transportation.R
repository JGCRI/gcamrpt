#### Data modules for the transprtation group

#' Service Output Data Module
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

module.service_output <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'Service output'
    }
    else {
        message('Function for processing variable: Service output')
        serviceOutput <- allqueries$'Service output'
        serviceOutput <- normalize(serviceOutput) %>%
            dplyr::group_by(Units, scenario, region, year, technology, vintage, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        serviceOutput <- filter(serviceOutput, years, filters)
        serviceOutput <- aggregate(serviceOutput, aggfn, aggkeys)
        # units example: million p-km
        serviceOutput <- unitconv_counts(serviceOutput, ounit)
        serviceOutput
    }
}

#' Energy Data Module
#'
#' Produce final energy by technology and vintage
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

module.final_energy <- function(mode, allqueries, aggkeys, aggfn, years,
                                filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Final energy', 'Refined liquids')
    }
    else {
        message('Function for processing variable: Final energy')

        energy <- allqueries$'Final energy'
        refining <- allqueries$'Refined liquids'
        energy <- mapfuel(energy, refining) #replaces input/technology with fuel & liquid_type
        energy <- normalize(energy) %>%
            dplyr::group_by(Units, scenario, region, year, technology, vintage, fuel, liquid_type, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        energy <- unitconv_energy(energy, ounit)
        energy <- filter(energy, years, filters)
        energy <- aggregate(energy, aggfn, aggkeys)
        # units example: EJ/yr
        energy

    }
}
#' Load Factors Data Module
#'
#' Produce load factors by technology and vintage
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

module.load_factors <- function(mode, allqueries, aggkeys, aggfn, years,
                                  filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        'Load factors'
    }
    else {
        message('Function for processing variable: Load factors')

        ldfctr <- allqueries$'Load factors'
        ldfctr <- normalize(ldfctr) %>%
            dplyr::group_by(Units, scenario, region, year, technology, service, mode, submode) %>% # average over LHDT
            dplyr::summarise(value=mean(value)) %>%
            dplyr::ungroup()
        ldfctr <- filter(ldfctr, years, filters)
        ldfctr <- aggregate(ldfctr, aggfn, aggkeys)
        ldfctr$Units <- ounit

        ldfctr
    }
}

#' Service Intensity Data Module
#'
#' Calculate service intensity by submode
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

module.service_intensity <- function(mode, allqueries, aggkeys, aggfn, years,
                                filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Final energy', 'Refined liquids', 'Service output')
    }
    else {
        message('Function for processing variable: Service intensity')

        # run filters and agg on component queries, then calculate final variable
        serviceOutput <- allqueries$'Service output'
        serviceOutput <- normalize(serviceOutput) %>%
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        serviceOutput <- filter(serviceOutput, years, filters)
        serviceOutput <- aggregate(serviceOutput, aggfn, aggkeys) # million pkm

        energy <- allqueries$'Final energy'
        refining <- allqueries$'Refined liquids'
        energy <- mapfuel(energy, refining) #replaces input/technology with fuel & liquid_type
        energy <- normalize(energy) %>%
            dplyr::group_by(Units, scenario, region, year, service, mode, submode) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
        energy <- filter(energy, years, filters)
        energy <- aggregate(energy, aggfn, aggkeys) #EJ

        # calculation
        intensity <- energy %>%
            dplyr::inner_join(serviceOutput,
                              by = c('scenario', 'region', 'year')) %>%
            dplyr::rename(energy=value.x, pkm=value.y) %>%
            dplyr::mutate(intensity = energy/pkm, Units='TJ') %>%
            # relying on native units of 'EJ' (10^18) and million pass-km'/'million ton-km' (10^6)
            dplyr::select(-pkm, -Units.x, -energy, -Units.y)  %>%
            dplyr::rename(value=intensity)
        intensity <- unitconv_energy(intensity, ounit)
        intensity

    }
}

#' Sales Data Module
#'
#' Produce sales
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

module.sales <- function(mode, allqueries, aggkeys, aggfn, years,
                                  filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles in vector
        c('Service output', 'Load factors')
    }
    else {
        message('Sales processing module: work in progress. Data returned')
        return(NULL)

        serviceOutput <- allqueries$'Service output' %>%
            normalize(serviceOutput) %>%
            dplyr::filter(year==vintage) # vintage aggregated over in normalize()
            # need to aggregate over fuel because don't have that info for ldfctr

        ldfctr <- allqueries$'Load factors' %>%
            normalize(ldfctr) %>%
            # average over technology (same for all submodes, so really just collapsing technology column)
            dplyr::group_by(Units, scenario, region, service, mode, submode, year) %>%
            dplyr::summarise(value=mean(value)) %>%
            dplyr::ungroup()

        # calculation
        sales <- serviceOutput %>%
            dplyr::inner_join(ldfctr,
                              by = c('scenario', 'region', 'service', 'mode', 'submode', 'year')) %>%
            dplyr::rename(pkm=value.x, lf=value.y) %>%
            dplyr::mutate(vkm = pkm/lf, Units='million vehicle-km') %>%
            # relying on native units of 'million pass-km'/'million ton-km'
            dplyr::select(-pkm, -Units.x, -lf, -Units.y)  %>%
            dplyr::inner_join(annual_mileage, #sysdata
                              by = c('service', 'mode', 'submode', 'year')) %>%
            dplyr::rename(mileage=value) %>%
            dplyr::mutate(sales = vkm/mileage) %>%
            dplyr::select(-vkm, -Units.x, -mileage, -Units.y) %>%
            dplyr::rename(value=sales) %>%
            dplyr::mutate(Units = 'million vehicles')

        sales <- filter(sales, years, filters)
        sales <- aggregate(sales, aggfn, aggkeys)
        # units example: million p-km
        sales <- unitconv_counts(sales, ounit)
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

#' Normalize query dataframes
#'
#' Columns removed (no mapping): rundate, load-factor
#' Columns removed (after mapping) : sector, subsector
#' Technologies removed: adv-electric, adv-liquid, tech-adv-electric, and tech-adv-liquid (only appears in load factors)
#' Columns aggregated over : technology, vintage -- include in dplyr::group_by() if needed later
#'
#' Final, normalized set of columns : Units, scenario, region, year, service, mode, submode, fuel, liquid_type
#' If this set changes, need to adjust call to group_by() below
#' All other cols are either dropped or aggregated over
#'
#' @param queryData Data returned for one variable query
#' @keywords internal
normalize <- function(queryData) {
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
    # service mapping
    passengerSectors <- c('trn_aviation_intl', 'trn_pass', 'trn_pass_road', 'trn_pass_road_ldv')
    freightSectors <- c('trn_freight', 'trn_freight_road', 'trn_shipping_intl')
    # mode mapping
    aviation <- tolower(c('International Aviation', 'Domestic Aviation'))
    rail <- tolower(c('Freight Rail', 'Passenger Rail', 'HSR'))
    road <- tolower(c('truck 0-2t', 'truck 2-5t', 'truck 5-9t', 'truck 9-16t', 'bus', '2W', '4W', 'Three-Wheeler'))
    shipping <- tolower(c('domestic ship', 'international ship'))
    # submode mapping
    lhdt <- c('truck 0-2t', 'truck 2-5t') # requires aggregation (see below) -- rest are 1:1 from subsector

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
        dplyr::mutate(submode = dplyr::if_else(subsector=='truck 5-9t', 'MHDT', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='truck 9-16t', 'HHDT', submode)) %>%
        dplyr::filter( service != '' & mode != '' & submode != '' ) %>%
        dplyr::arrange(scenario, region, service, mode, submode, value, Units)


    queryData
}
