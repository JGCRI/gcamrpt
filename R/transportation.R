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
        serviceOutput <- vint_tech_split(serviceOutput)
        serviceOutput <- parse_sector(serviceOutput)

        # Need to handle 'LHDT' redundancy differently because not all queries have input col, which changes call to group_by()
        serviceOutput <- serviceOutput %>%
            dplyr::select(-sector, -subsector) %>%
            dplyr::group_by(Units, scenario, region, year, technology, service, mode, submode) %>%
            dplyr::summarise(value = sum(value)) %>%
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
        energy <- allqueries$'Final energy'
        refining <- allqueries$'Refined liquids'
        energy <- vint_tech_split(energy)
        energy <- fuel(energy, refining)
        energy <- parse_sector(energy) # must go after fuel() because 'input' col disrupts aggregating over submodes
        # Need to handle 'LHDT' redundancy differently because not all queries have input col, which changes call to group_by()
        energy <- energy %>%
            dplyr::select(-sector, -subsector, -technology, -input) %>%
            dplyr::group_by(Units, scenario, region, year, service, mode, submode, fuel, liquid_type) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup()

        energy <- filter(energy, years, filters)
        energy <- aggregate(energy, aggfn, aggkeys)
        # units example: EJ/yr
        energy <- unitconv_energy(energy, ounit)
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
        c('Load factors', 'Service output')
    }
    else {
        message('Function for processing variable: Load factors')

        ldfctr <- allqueries$'Load factors'
        ldfctr <- ldfctr[, !(names(ldfctr) %in% c('load-factor', 'technology', 'rundate'))]
        # query output includes NA column
        # ldfctr technology col has diff levels than that of service output
        ldfctr <- parse_sector(ldfctr) %>% dplyr::select(-sector, subsector)
        # parse_sector makes sector/subsector redundant

        serviceOutput <- allqueries$'Service output'
        serviceOutput <- dplyr::select(serviceOutput, -technology, -rundate)
        # no need to split tech and vint bc not matching to ldfctr by technology
        serviceOutput <- parse_sector(serviceOutput) %>% dplyr::select(-sector, -subsector)
        # parse_sector makes sector/subsector redundant

        ldfctr

    }
}


#' Split technology into technology and vintage cols
#'
#' Query returns data with Technology column in the format'Technology, year=Vintage'.
#' This function returns the data frame with Technology split into a Technology and
#' Vintage column.
#'
#' Makes use of split.vt() function below. This function must write Vintage column first
#' before rewriting Technology column.
#'
#' @param df Data returned for individual query
#' @keywords internal
vint_tech_split <- function(df) {
    df$vintage <- unlist(lapply(df$technology, split.vt, col='vint'))
    df$technology <- unlist(lapply(df$technology, split.vt, col='tech'))
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
#' @keywords internal
parse_sector <- function(df) {

    ## Service cond'ns
    freight <- grepl('freight', df$sector)
    pass <- grepl('pass', df$sector)

    ## Mode cond'ns
    ship <- grepl('ship', tolower(df$subsector))
    av <- grepl('aviation', tolower(df$subsector))
    road <- grepl('road', df$sector)
    rail <- grepl('rail', tolower(df$subsector))

    ## Service
    df[,'service'] <- NA # na.omit(df) later
    df[freight | ship, 'service'] <- 'Freight'
    df[pass, 'service'] <- 'Passenger'
    df[av, 'service'] <- 'Passenger'

    ## Mode
    df[,'mode'] <- NA #na.omit(df) later
    df[rail, 'mode'] <- 'Rail'
    df[road, 'mode'] <- 'Road'
    df[ship,'mode'] <- 'Shipping'
    df[av, 'mode'] <- 'Aviation'


    #Submode cond'ns
    df[,'submode'] <- NA #na.omit(df) later
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
    df[intl, 'submode'] <- 'International'
    df[dom, 'submode'] <- 'Domestic'
    df[freightrail, 'submode'] <- 'Freight Rail'
    df[passrail, 'submode'] <- 'Passenger Rail'
    df[w2, 'submode'] <- '2W'
    df[w3, 'submode'] <- '3W'
    df[w4, 'submode'] <- '4W'
    df[bus, 'submode'] <- 'Bus'
    df[t2 | t5, 'submode'] <- 'LHDT' # aggregated over to collapse LHDT submode in data module
    df[t9, 'submode'] <- 'MHDT'
    df[t16, 'submode'] <- 'HHDT'

    df <- na.omit(df) # remove incomplete observations (no service/mode/submode assigned)
    df
}

#' Split: vintage, technology
#'
#' Using text in the format of 'Technology, year=Vintage', splits text at
#' ',' and '=', and returns entry indicated by col
#'
#' @param text Text entry of original technology column
#' @param col Specifies which string from text to return
#' @keywords internal
split.vt <- function(text, col) {
    splt <- strsplit(text, ',')[[1]]
    if (col =='tech') {
        splt[1]
    } else if (col =='vint') {
        strsplit(splt[2],'=')[[1]][2]
    }
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
fuel <- function(en, ref) {
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
        dplyr::select(-value, -total, -Units, -rundate)

    ## Energy from Liquids
    # Biomass liquids
    en_bio <- en[en$fuel =='Liquids', ] %>%
        dplyr::mutate(liquid_type = 'biomass') %>%
        dplyr::inner_join(ref_bio, by=c("scenario", "region", "year")) %>% # lose 1k obs
        dplyr::mutate(value=value*share) %>% # scale total energy down to fraction produced by refined biomass liquids
        dplyr::select(-share, -rundate)


    # Non-biomass liquids
    en_nonbio <- en[en$fuel =='Liquids', ] %>%
        dplyr::mutate(liquid_type = 'traditional') %>% # traditional = all liquids minus bio
        dplyr::inner_join(en_bio[, c("Units", "scenario", "region", "year", "value")],
                          by=c("Units", "scenario", "region", "year")) %>% # gain 2k obs
        dplyr::mutate(value.x=value.x - value.y) %>% # subtract biomass liquids
        dplyr::rename(value = value.x) %>% # keep traditional/non-biomass
        dplyr::select(-value.y, -rundate) # drop biomass


    # Replace liquids with bio/nonbio liquids
    en_nonliq <- en[en$fuel != 'Liquids',] %>%
        dplyr::mutate(liquid_type = NA) %>%
        dplyr::select(-rundate)
    en <- rbind(en_nonliq, en_bio, en_nonbio)

    en
}
