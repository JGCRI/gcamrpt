#### Data modules for the water group

#' Water Withdrawals Module
#'
#' Produce water withdrawals by region, supersector, sector, subsector, or
#' technology. The column supersector is created by further aggregating sectors
#' into one of Biomass, Primary Energy, Irrigation, Manufacturing, Electricity,
#' Municipal, or Livestock.
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
                                          supersector = groupSectors(sector)) %>%
                            dplyr::group_by(Units, scenario, region, year, supersector, sector, subsector, technology) %>%
                            dplyr::summarise(value = sum(value)) %>%
                            dplyr::ungroup() %>%
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

        waterCnsmptn <- allqueries$'Water Consumption' %>%
                        filter(years, filters) %>%
                        dplyr::mutate(sector = sub('nuclear.*', 'uranium', sector),
                                      sector = sub('regional *', '', sector),
                                      sector = sub('unconventional oil.*', 'oil', sector),
                                      subsector = sub('AEZ\\d{2}', '', subsector),
                                      supersector = groupSectors(sector)) %>%
                        dplyr::group_by(Units, scenario, region, year, supersector, sector, subsector, technology) %>%
                        dplyr::summarise(value = sum(value)) %>%
                        dplyr::ungroup() %>%
                        aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_vol(waterCnsmptn$Units[1], ounit)
            waterCnsmptn$value <- waterCnsmptn$value * cf
            waterCnsmptn$Units <- ounit
        }

        waterCnsmptn
        }
}

#' Water Irrigation Module
#'
#' Take all water withdrawals used for crops. Because the subsector column for
#' crops is identical to the technology column for crops in the raw data, remove
#' AEZ specification from the subsector column.
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
                    dplyr::mutate(subsector = sub('AEZ\\d{2}', '', subsector)) %>%
                    filter(years,  filters) %>%
                    aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_vol(waterIrr$Units[1], ounit)
            waterIrr$value <- waterIrr$value * cf
            waterIrr$Units <- ounit
        }

        waterIrr$input <- NULL
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

#' Group water use sectors into categories
#'
#' Convert the mix of energy, agriculture, and other uses of water into a more
#' concise grouping. The six larger categories are Forest, Cropland, Biomass,
#' Other Natural Land, Pasture, and Desert. Any unknown sector passed to this
#' function will be returned as is.
#'
#' @param sector A column (generally the 'sector' column) from a query result
#' @keywords internal
groupSectors <- function(sector) {
    groups <- c(
        'biomass' = 'Biomass',
        'coal' = 'Primary Energy',
        'natural gas' = 'Primary Energy',
        'oil' = 'Primary Energy',
        'uranium' = 'Primary Energy',
        'Corn' = 'Irrigation',
        'FiberCrop' = 'Irrigation',
        'FodderGrass' = 'Irrigation',
        'FodderHerb' = 'Irrigation',
        'MiscCrop' = 'Irrigation',
        'OilCrop' = 'Irrigation',
        'OtherGrain' = 'Irrigation',
        'Rice' = 'Irrigation',
        'Root_Tuber' = 'Irrigation',
        'SugarCrop' = 'Irrigation',
        'Wheat' = 'Irrigation',
        'industry' = 'Manufacturing',
        'electricity' = 'Electricity',
        'domestic water' = 'Municipal',
        'Beef' = 'Livestock',
        'Dairy' = 'Livestock',
        'Pork' = 'Livestock',
        'Poultry' = 'Livestock',
        'SheepGoat' = 'Livestock'
    )
    sapply(sector, function(s) {
        if(s %in% names(groups)) groups[[s]] else s
    })
}
