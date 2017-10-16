context('Transportation modules')
load('test-data/service_outputq.rda')
load('test-data/load_factorsq.rda')
load('test-data/final_energyq.rda')
load('test-data/bio_refining.rda')

queries <- list(service_outputq, load_factorsq, final_energyq)
queries <- stats::setNames(queries, c("Service output", "Load factors", "Final energy"))

# database normalization
normalize.transp <- function(queryData) {
    if ('rundate' %in% names(queryData)) {queryData <- dplyr::select(queryData, -rundate)} # remove rundate
    if ('load-factor' %in% names(queryData)) {queryData <- queryData[,!(names(queryData) %in% c("load-factor"))]}
    # query always includes col of NA's

    queryData$sector <- tolower(queryData$sector)
    queryData$subsector <- tolower(queryData$subsector)
    queryData$subsector <- gsub('[()]', '', queryData$subsector)

    # include tech + vint cols
    if('technology' %in% names(queryData)) {
        queryData$technology <- tolower(queryData$technology)
        if(grepl(',year=', queryData$technology[1])) {
            queryData <- tidyr::separate(queryData, technology, c("technology", "vintage"), ",year=")
            queryData$vintage <- tolower(queryData$vintage)
            # split vint/tech
        }
        else {queryData$vintage <- ''} # include vint col if not incl'd in tech
    } else { # if neither, add both
        queryData$technology <- ''
        queryData$vintage <- ''
    }

    # replace input col with fuel col, else add blank fuel col
    if ('input' %in% names(queryData)) {
        queryData$input <- tolower(queryData$input)
        queryData <- queryData %>%
            dplyr::mutate(fuel='') %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('coal', input), 'Coal', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('gas', input), 'Natural Gas', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('elec', input), 'Electricity', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('h2', input), 'Hydrogen', fuel)) %>%
            dplyr::mutate(fuel = dplyr::if_else(grepl('liquids', input), 'Liquids', fuel))

        # use pre-calculated bio share of refining to break out biomass liquids and traditional liquids
        liq <- queryData[queryData$fuel =='Liquids', ]
        bioliq <- liq %>%
            dplyr::mutate(liquid_type = 'biomass') %>%
            dplyr::inner_join(bio_refining, by=c("scenario", "region", "year")) %>%
            dplyr::mutate(value=value*share) %>%
            dplyr::select(-share)
        nonbioliq <- liq %>%
            dplyr::mutate(liquid_type='traditional') %>%
            dplyr::inner_join(bio_refining, by=c("scenario", "region", "year")) %>%
            dplyr::mutate(value=value*(1-share)) %>% # subtract biomass liquids
            dplyr::select(-share) # drop original liquids and biomass value col
        liq <- rbind(bioliq, nonbioliq)
        nonliq <- queryData %>%
            dplyr::filter(fuel != 'Liquids') %>%
            dplyr::mutate(liquid_type='')
        queryData <- rbind(liq, nonliq) # replace rows where fuel==liquids
    } else {
        queryData$fuel <- ''
        queryData$liquid_type <- ''}

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
        dplyr::mutate(submode = dplyr::if_else(service=='Freight' & mode=='Rail', 'Freight  Rail', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='2w', '2W', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='three-wheeler', '3W', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='4w', '4W', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='bus', 'Bus', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector %in% lhdt, 'LHDT', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='truck 5-9t', 'MHDT', submode)) %>%
        dplyr::mutate(submode = dplyr::if_else(subsector=='truck 9-16t', 'HHDT', submode)) %>%
        dplyr::group_by(Units, scenario, region, year, service, mode, submode, fuel, liquid_type) %>% # aggregate over LHDT
        dplyr::summarise(value=sum(value)) %>% # aggregate over LHDT
        dplyr::ungroup() %>% # aggregate over LHDT
        dplyr::filter( service != '' & mode != '' & submode != '' )

    queryData
}

queries <- lapply(queries, normalize.transp)

test_that('GETQ Mode returns correct query titles', {
    expect_match(module.service_output(iamrpt:::GETQ), 'Service output')
    expect_match(module.load_factors(iamrpt:::GETQ), 'Load factors')
    expect_equal(module.final_energy(iamrpt:::GETQ), c("Final energy", "Refined liquids"))
    expect_equal(module.service_intensity(iamrpt:::GETQ), c("Final energy", "Refined liquids", "Service output"))
})


test_that('Transportation models return correct service/mode/submode factors', {
    serviceopts <- c("Freight", "Passenger")
    expect_equal(levels(factor(queries$`Service output`$service)),
                     serviceopts)
    expect_equal(levels(factor(queries$`Load factors`$service)),
                     serviceopts)
    expect_equal(levels(factor(queries$`Final energy`$service)),
                     serviceopts)
    expect_equal(levels(factor(queries$`Final energy`$service)),
                     serviceopts)

    modeopts <- c("Aviation", "Rail", "Road", "Shipping")
    expect_equal(levels(factor(queries$`Service output`$mode)),
                     modeopts)
    expect_equal(levels(factor(queries$`Load factors`$mode)),
                     modeopts)
    expect_equal(levels(factor(queries$`Final energy`$mode)),
                     modeopts)
    expect_equal(levels(factor(queries$`Final energy`$mode)),
                     modeopts)

    submodeopts <- c("2W", "3W", "4W", "Bus", "Domestic", "Freight Rail", "HHDT", "International", "LHDT", "MHDT", "Passenger Rail")
    expect_equal(levels(factor(queries$`Service output`$submode)),
                     submodeopts)
    expect_equal(levels(factor(queries$`Load factors`$submode)),
                     submodeopts)
    expect_equal(levels(factor(queries$`Final energy`$submode)),
                     submodeopts)
    expect_equal(levels(factor(queries$`Final energy`$submode)),
                     submodeopts)
})

test_that('Transportation models return transportation data', {
    aggkeys <- NA
    aggfn <- NA
    years <- '2000:2050'
    filters <- NA
    ounit <- NA
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service output`, year>=2000, year<=2050))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Load factors`, year>=2000, year<=2050))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Final energy`, year>=2000, year<=2050))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service intensity`, year>=2000, year<=2050))

    years <- NA
    filters <- '(==; region;India), (==; service;Freight), (==;mode;Road), (==;submode;LHDT)'
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service output`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Load factors`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Final energy`, service=='Freight', mode=='Road', submode=='LHDT'))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service intensity`, service=='Freight', mode=='Road', submode=='LHDT'))

    filters <- '(==; region;India), (==;service;Passenger), (==;mode;Road), (==;submode;4W), (==;fuel;Electricity)'
    expect_identical(module.service_output(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service output`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
    expect_identical(module.load_factors(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Load factors`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
    expect_identical(module.final_energy(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Final energy`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))
    expect_identical(module.service_intensity(iamrpt:::RUN, queries , aggkeys, aggfn, years, filters, ounit),
                     dplyr::filter(queries$`Service intensity`, service=='Passenger', mode=='Road', submode=='4W', fuel=='Electricity'))

})














