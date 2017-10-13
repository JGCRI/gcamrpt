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
        message('Function for processing variable: CO2 emissions')
        co2 <- allqueries$'CO2 emissions'
        # sometimes appears in query. useless info
        if ('rundate' %in% names(co2)) {co2 <- dplyr::select(co2, -rundate)}

        co2 <- semiaggregate(co2)
        co2 <- parse_sector(co2, hasvintage=FALSE, hasfuel=FALSE, hastechnology=FALSE)
        co2 <- filter(co2, years, filters)
        co2 <- aggregate(co2, aggfn, aggkeys)
        co2 <- unitconv_co2(co2, ounit)
        co2
    }
}

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
        serviceOutput <- serviceOutput %>% # sum over vintage/technology
            dplyr::group_by(Units, scenario, region, service, mode, submode, year) %>%
            dplyr::summarise(value=sum(value)) %>%
            dplyr::ungroup()

        ldfctr <- semiaggregate(ldfctr)
        ldfctr <- parse_sector(ldfctr, hasvintage=FALSE, hasfuel=FALSE, hastechnology=TRUE)
        ldfctr <- ldfctr %>% # average over technology (same for all submodes, so really just collapsing technology column)
            dplyr::group_by(Units, scenario, region, service, mode, submode, year) %>%
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
            dplyr::mutate(pm_emissions = vkm*pmfac, Units='g') %>%
            dplyr::select(-pmfac, -Units.x, -vkm, -Units.y) %>%
            dplyr::rename(value=pm_emissions)

        pm <- filter(pm, years, filters)
        pm <- aggregate(pm, aggfn, aggkeys)
        pm <- unitconv_weight(pm, ounit)
        pm
    }
}
