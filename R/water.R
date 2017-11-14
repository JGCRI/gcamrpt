#### Data modules for the water group

#' Water Withdrawals Module
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{subsector}
#'   \item{technology}
#'   \item{input}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.water_withdrawals <- function(mode, allqueries, aggkeys, aggfn, years,
                                     filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Water Withdrawals'
    }
    else {
        message('Function for processing variable: Water Withdrawals')

        waterWdrwls <- allqueries$'Water Withdrawals' %>%
                            filter(years, filters) %>%
                            dplyr::mutate(sector = sub('nuclear.*', 'uranium', sector),
                                          sector = sub('regional *', '', sector),
                                          sector = sub('unconventional oil.*', 'oil', sector),
                                          subsector = sub('AEZ\\d{2}', '', subsector),
                                          technology = sub('AEZ\\d{2}', '', technology)) %>%
                            dplyr::group_by(Units, scenario, region, year, sector, subsector, technology) %>%
                            dplyr::summarise(value = sum(value)) %>%
                            aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_vol(waterWdrwls$Units[1], ounit)
            waterWdrwls$value <- waterWdrwls$value * cf
            waterWdrwls$Units <- ounit
        }

        waterWdrwls
        }
}


#' Water Consumption Module
#'
#' Produce agricultural production by crop type
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{output}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.water_consumption <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Water Consumption'
    }
    else {
        message('Function for processing variable: Water Consumption')

        waterCnsmptn <- allqueries$'Water Consumption'
        waterCnsmptn <- filter(waterCnsmptn, years, filters)
        waterCnsmptn <- aggregate(waterCnsmptn, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_vol(waterCnsmptn$Units[1], ounit)
            waterCnsmptn$value <- waterCnsmptn$value * cf
            waterCnsmptn$Units <- ounit
        }

        waterCnsmptn$output <- NULL
        waterCnsmptn
        }
}

#' Water Irrigation Module
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{output}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.water_irrigation <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Water Withdrawals'
    }
    else {
        message('Function for processing variable: Water Withdrawals for Irrigation')

        crops <- c('Corn', 'FiberCrop', 'FodderGrass', 'FodderHerb', 'MiscCrop',
                   'OilCrop', 'OtherGrain', 'PalmFruit', 'Rice', 'Root_Tuber',
                   'SugarCrop', 'Wheat', 'biomass', 'biomassOil')

        waterIrr <- allqueries$'Water Withdrawals' %>%
                    dplyr::filter(sector %in% crops) %>%
                    filter(years,  filters) %>%
                    aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_vol(waterIrr$Units[1], ounit)
            waterIrr$value <- waterIrr$value * cf
            waterIrr$Units <- ounit
        }

        waterIrr$output <- NULL
        waterIrr
        }
}

#' Water Electricity Module
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{output}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.water_electricity <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Water Withdrawals'
    }
    else {
        message('Function for processing variable: Water Withdrawals for Electricity')

        waterElct <- allqueries$'Water Withdrawals' %>%
                     dplyr::filter(sector == 'electricity')
        waterElct <- filter(waterElct, years,  filters)
        waterElct <- aggregate(waterElct, aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_vol(waterElct$Units[1], ounit)
            waterElct$value <- waterElct$value * cf
            waterElct$Units <- ounit
        }

        waterElct$output <- NULL
        waterElct
        }
}
