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

        # sometimes appears in query. useless info
        if ('rundate' %in% names(serviceOutput)) {serviceOutput <- dplyr::select(serviceOutput, -rundate)}

        serviceOutput <- vint_tech_split(serviceOutput)
        serviceOutput <- parse_sector(serviceOutput, hasvintage=TRUE, hasfuel=FALSE, hastechnology=TRUE)

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

        # sometimes appears in query. useless info
        if ('rundate' %in% names(energy)) {
            energy <- dplyr::select(energy, -rundate)}
        if ('rundate' %in% names(refining)) {
            refining <- dplyr::select(refining, -rundate)}

        energy <- vint_tech_split(energy)
        energy <- mapfuel(energy, refining) #removes input and technology, replaces with fuel and liquid_type
        energy <- semiaggregate(energy)
        energy <- parse_sector(energy, hasvintage=TRUE, hasfuel=TRUE, hastechnology=FALSE) # must go after mapfuel() because group_by call in this func'n uses 'fuel' and 'liquid_type'
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

        # sometimes appears in query. useless info
        if ('rundate' %in% names(ldfctr)) {ldfctr <- dplyr::select(ldfctr, -rundate)}
        # query output includes NA column
        ldfctr <- ldfctr[, !(names(ldfctr) %in% c('load-factor'))]

        # data prep
        ldfctr <- semiaggregate(ldfctr)
        ldfctr <- parse_sector(ldfctr, hasvintage=FALSE, hasfuel=FALSE, hastechnology=TRUE)

        # after calculation
        ldfctr <- filter(ldfctr, years, filters)
        ldfctr <- aggregate(ldfctr, aggfn, aggkeys)
        ldfctr <- unitconv_ldfctr(ldfctr, ounit)

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

        serviceOutput <- allqueries$'Service output'
        energy <- allqueries$'Final energy'
        refining <- allqueries$'Refined liquids'

        # sometimes appears in query. useless info
        if ('rundate' %in% names(serviceOutput)) {
            serviceOutput <- dplyr::select(serviceOutput, -rundate)}
        if ('rundate' %in% names(energy)) {
            energy <- dplyr::select(energy, -rundate)}
        if ('rundate' %in% names(refining)) {
            refining <- dplyr::select(refining, -rundate)}

        #data prep
        serviceOutput <- vint_tech_split(serviceOutput)
        serviceOutput <- parse_sector(serviceOutput, hasvintage=TRUE, hasfuel=FALSE, hastechnology=TRUE)
        serviceOutput <- serviceOutput %>% # sum over vintage/technology
            dplyr::group_by(Units, scenario, region, service, mode, submode, year) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()

        energy <- vint_tech_split(energy)
        energy <- mapfuel(energy, refining) #removes input and technology, replaces with fuel and liquid_type
        energy <- semiaggregate(energy)
        energy <- parse_sector(energy, hasvintage=TRUE, hasfuel=TRUE, hastechnology=FALSE) # must go after mapfuel() because group_by call in this func'n uses 'fuel' and 'liquid_type'
        energy <- energy %>% # sum over vintage/technology
            dplyr::group_by(Units, scenario, region, service, mode, submode, year) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()

        # calculation
        intensity <- energy %>%
            dplyr::inner_join(serviceOutput,
                              by = c('scenario', 'region', 'service', 'mode', 'submode', 'year')) %>%
            dplyr::rename(energy=value.x, pkm=value.y) %>%
            dplyr::mutate(intensity = energy/pkm, Units='TJ') %>%
            # relying on native units of 'EJ' (10^18) and million pass-km'/'million ton-km' (10^6)
            dplyr::select(-pkm, -Units.x, -energy, -Units.y)  %>%
            dplyr::rename(value=intensity)

        # after calculation
        intensity <- filter(intensity, years, filters)
        intensity <- aggregate(intensity, aggfn, aggkeys)
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
        message('Function for processing variable: Service output')

        serviceOutput <- allqueries$'Service output'
        ldfctr <- allqueries$'Load factors'
        # sometimes appears in query. useless info
        if ('rundate' %in% names(serviceOutput)) {serviceOutput <- dplyr::select(serviceOutput, -rundate)}
        if ('rundate' %in% names(ldfctr)) {ldfctr <- dplyr::select(ldfctr, -rundate)}
        # query output includes NA column
        ldfctr <- ldfctr[, !(names(ldfctr) %in% c('load-factor'))]

        # data prep
        serviceOutput <- vint_tech_split(serviceOutput)
        serviceOutput <- parse_sector(serviceOutput, hasvintage=TRUE, hasfuel=FALSE, hastechnology=TRUE)
        serviceOutput <- serviceOutput %>% # sum over technology
            dplyr::group_by(Units, scenario, region, service, mode, submode, vintage, year) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(year==vintage) %>%
            dplyr::select(-vintage)

        ldfctr <- semiaggregate(ldfctr)
        ldfctr <- parse_sector(ldfctr, hasvintage=FALSE, hasfuel=FALSE, hastechnology=TRUE)
        ldfctr <- ldfctr %>% # average over technology (same for all submodes, so really just collapsing technology column)
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

#' Split technology into technology and vintage cols
#'
#' Query returns data with Technology column in the format'Technology, year=Vintage'.
#' This function returns the data frame with Technology split into a Technology and
#' Vintage column.
#'
#' @param df Data returned for individual query
#' @keywords internal
vint_tech_split <- function(df) {
    df <- tidyr::separate(df, technology, c("technology", "vintage"), ",year=")
    df
}

#' Parse sector column
#'
#' Service, mode, and submode can be parsed from sector and subsector cols of
#' query data. Some sectors are disaggregated versions of other sectors, making
#' the data redundant. This is handled by filling service, mode, and submode cols
#' only for those observations that meet present demands of reporting templates.
#'
#'
#' @param df Data returned for individual query
#' @param hasvintage Logical indicating if data has vintage col from query file
#' @param hasfuel Logical indicating if data has fuel and liquid_type col after fuel() processing
#' @param hastechnology Logical indication if data has technology col from query file
#' @keywords internal
parse_sector <- function(df, hasvintage, hasfuel, hastechnology) {

    ## Service cond'ns
    freight <- grepl('freight', df$sector)
    pass <- grepl('pass', df$sector)

    ## Mode cond'ns
    ship <- grepl('ship', tolower(df$subsector))
    av <- grepl('aviation', tolower(df$subsector))
    road <- grepl('road', df$sector)
    rail <- grepl('rail', tolower(df$subsector))
    hsrail <- grepl('hsr', tolower(df$subsector)) # also used to indicate submode as Passenger Rail

    ## Service
    df[freight | ship, 'service'] <- 'Freight'
    df[pass, 'service'] <- 'Passenger'
    df[av, 'service'] <- 'Passenger'

    ## Mode
    df[,'mode'] <- NA # use as index later
    df[rail, 'mode'] <- 'Rail'
    df[hsrail, 'mode'] <- 'Rail'
    df[road, 'mode'] <- 'Road'
    df[ship,'mode'] <- 'Shipping'
    df[av, 'mode'] <- 'Aviation'
    df[is.na(df$mode), 'mode'] <- 'Unassigned'


    #Submode cond'ns
    intl <- grepl('international', tolower(df$subsector)) # av and ship
    dom <- grepl('domestic', tolower(df$subsector)) # av and ship

    freightrail <- grepl('freight rail', tolower(df$subsector))
    passrail <- grepl('passenger rail', tolower(df$subsector))

    w2 <- grepl('2w', tolower(df$subsector))
    w3 <- grepl('three-wheeler', tolower(df$subsector))
    w4 <- grepl('4w', tolower(df$subsector))
    bus <- grepl('bus', tolower(df$subsector))

    t2 <- grepl('0-2t', df$subsector)
    t5 <- grepl('2-5t', df$subsector)
    t9 <- grepl('5-9t', df$subsector)
    t16 <- grepl('9-16t', df$subsector)

    #Submode
    df[,'submode'] <- NA #use as index later
    df[intl, 'submode'] <- 'International'
    df[dom, 'submode'] <- 'Domestic'
    df[freightrail, 'submode'] <- 'Freight Rail'
    df[passrail, 'submode'] <- 'Passenger Rail'
    df[hsrail, 'submode'] <- 'Passenger Rail'
    df[w2, 'submode'] <- '2W'
    df[w3, 'submode'] <- '3W'
    df[w4, 'submode'] <- '4W'
    df[bus, 'submode'] <- 'Bus'
    df[t2 | t5, 'submode'] <- 'LHDT' # aggregated over to collapse LHDT submode in data module
    df[t9, 'submode'] <- 'MHDT'
    df[t16, 'submode'] <- 'HHDT'
    df[is.na(df$submode), 'submode'] <- 'Unassigned'

    # Remove t2 t5 subsector redundancy, collapse both subsectors into same row for each region
    if ( hasvintage && hasfuel && hastechnology ) {
        df <- df %>%
            dplyr::select(-sector, -subsector) %>% #sector/subsector info no longer important
            dplyr::group_by(Units, scenario, region, service, mode, submode, fuel, liquid_type, year, vintage, technology) %>%
            # vintage col is probably unnecessary, but its the query saved in the pkg
            # technology is dropped in fuel() because the input and tech cols are replaced with fuel and liquid_type
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
    }

    if ( hasvintage && hasfuel && !hastechnology ) {
        df <- df %>%
            dplyr::select(-sector, -subsector) %>% #sector/subsector info no longer important
            dplyr::group_by(Units, scenario, region, service, mode, submode, fuel, liquid_type, year, vintage) %>%
            # vintage col is probably unnecessary, but its the query saved in the pkg
            # technology is dropped in fuel() because the input and tech cols are replaced with fuel and liquid_type
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
    }

    if ( hasvintage && !hasfuel && hastechnology  ) {
        df <- df %>%
            dplyr::select(-sector, -subsector) %>% #sector/subsector info no longer important
            dplyr::group_by(Units, scenario, region, service, mode, submode, technology, year, vintage) %>%
            # vintage col is probably unnecessary, but its the query saved in the pkg
            # technology column is used in some filters for service output
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
    }

    if ( !hasvintage && !hasfuel && hastechnology ) {
        df <- df %>%
            dplyr::select(-sector, -subsector) %>% #sector/subsector info no longer important
            dplyr::group_by(Units, scenario, region, service, mode, submode, technology, year) %>%
            # vintage col is probably unnecessary, but its the query saved in the pkg
            # technology is dropped in fuel() because the input and tech cols are replaced with fuel and liquid_type
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
    }

    if (!hasvintage && !hasfuel && !hastechnology) {
        df <- df %>%
            dplyr::select(-sector, -subsector) %>% #sector/subsector info no longer important
            dplyr::group_by(Units, scenario, region, service, mode, submode, year) %>%
            # vintage col is probably unnecessary, but its the query saved in the pkg
            # technology is dropped in fuel() because the input and tech cols are replaced with fuel and liquid_type
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()
    }



    df
}


#' Semiaggregate
#'
#' some queries return queries with disaggregated sectors/subsectors, which requires ones like (trn_pass_road_LDV_2W, moped)
#  to be mapped to (trn_pass_road_LDV, 2W) before being passed to parse_sector
#'
#'
#' @param df Data returned for individual query
#' @keywords internal
semiaggregate <- function(df) {
    w2 <- grepl('2w', tolower(df$sector))
    df[w2,'sector'] <- 'trn_pass_road_LDV'
    df[w2, 'subsector'] <- '2W'

    w4 <- grepl('4w', tolower(df$sector))
    df[w4,'sector'] <- 'trn_pass_road_LDV'
    df[w4,'subsector'] <- '4W'

    if('technology' %in% names(df)) {
        adv_tech <- grepl('adv', tolower(df$technology))
        df <- df[!adv_tech, ]
    }

    df
}


#' Fuel
#'
#' Service, mode, and submode can be parsed from sector and subsector cols of
#' query data. Some sectors are disaggregated versions of other sectors, making
#' the data redundant. This is handled by filling service, mode, and submode cols
#' only for those observations that meet present demands of reporting templates.
#'
#'
#' @param en Data returned for final energy query
#' @param en Data returned for refined liquids query

#' @keywords internal
mapfuel <- function(en, ref) {
    # cond'ns
    coal <- grepl('coal', tolower(en$input))
    gas <- grepl('gas', tolower(en$input))
    elec <- grepl('elec', tolower(en$input))
    hyd <- grepl('h2', tolower(en$input))
    liq <- grepl('liquids', tolower(en$input))

    en[coal, 'fuel'] <- 'Coal'
    en[gas, 'fuel'] <- 'Natural Gas'
    en[elec, 'fuel'] <- 'Electricity'
    en[hyd, 'fuel'] <- 'Hydrogen'
    en[liq, 'fuel'] <- 'Liquids'

    en <- dplyr::select(en, -input, -technology) # input replaced with fuel col; tech col not used
    en <- liquids(en, ref)

    en
}

#' Liquid Fuels
#'
#' Calculate biomass liquids energy output from biomass' share of total liquids refining
#'
#'
#' @param en Data returned for final energy query
#' @param ref Data returned for refined liquids query
#' @keywords internal
liquids <- function(en, ref) {

    ## Refined Liquids
    # aggregate for total
    ref_tot <- ref %>%
        dplyr::group_by(scenario, region, year) %>%
        dplyr::summarise(total = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::select(scenario, region, year, total)
    # subset to biomass -- bioethanol vs biodiesel??
    ref_bio <- ref[ref$subsector == 'biomass liquids', ] %>%
        dplyr::select(-subsector) %>%
        dplyr::inner_join(ref_tot, by=c("scenario", "region", "year")) %>%
        dplyr::mutate(share = value/total) %>%
        dplyr::select(-value, -total, -Units)

    ## Energy from Liquids
    # Biomass liquids
    en_bio <- en[en$fuel =='Liquids', ] %>%
        dplyr::mutate(liquid_type = 'biomass') %>%
        dplyr::inner_join(ref_bio, by=c("scenario", "region", "year")) %>% # lose 1k obs
        dplyr::mutate(value=value*share) %>% # scale total energy down to fraction produced by refined biomass liquids
        dplyr::select(-share)


    # Non-biomass liquids
    en_nonbio <- en[en$fuel =='Liquids', ] %>%
        dplyr::mutate(liquid_type = 'traditional') %>% # traditional = all liquids minus bio
        dplyr::inner_join(en_bio[, c("Units", "scenario", "region", "year", "value")],
                          by=c("Units", "scenario", "region", "year")) %>% # gain 2k obs
        dplyr::mutate(value.x=value.x - value.y) %>% # subtract biomass liquids
        dplyr::rename(value = value.x) %>% # keep traditional/non-biomass
        dplyr::select(-value.y) # drop biomass


    # Replace liquids with bio/nonbio liquids
    en_nonliq <- en[en$fuel != 'Liquids',] %>%
        dplyr::mutate(liquid_type = 'Unassigned')
    en <- rbind(en_nonliq, en_bio, en_nonbio)

    en
}

