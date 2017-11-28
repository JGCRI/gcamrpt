#### Data modules for the agriculture, animals, and biomass groups

#' Crop Production Data Module
#'
#' Produce agricultural production (in mass) by crop type. Exclude forest and
#' crops grown for biomass, as they are in different units.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{output}
#'   \item{sector}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.crop_production <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Ag Production'
    }
    else {
        message('Function for processing variable: Crop Production')

        noncrops <- c('biomass', 'NonFoodDemand_Forest', 'Forest', 'UnmanagedLand')

        agproduction <- allqueries$'Ag Production' %>%
                        dplyr::filter(!output %in% noncrops) %>%
                        dplyr::select(-output) %>%
                        filter(years, filters) %>%
                        aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_mass(agproduction$Units[1], ounit)
            agproduction$value <- agproduction$value * cfac
            agproduction$Units <- ounit
        }

        agproduction
    }
}


#' Crop Consumption Data Module
#'
#' Produce crop (food) consumption by food type. Unlike Crop Production, this
#' module also includes meat demand.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{output}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.food_consumption <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Food Consumption'
    }
    else {
        message('Function for processing variable: Food Consumption')

        foodConsumption <- allqueries$'Food Consumption' %>%
                           dplyr::mutate(sector = sub('FoodDemand_', '', sector)) %>%
                           dplyr::select(-output) %>%
                           filter(years, filters) %>%
                           aggregate(aggfn, aggkeys)

        if('technology' %in% names(foodConsumption))
            foodConsumption <- dplyr::rename(foodConsumption,
                                             type = technology)

        if(!is.na(ounit)) {
            # Use energy conversion because default units are Pcal
            cfac <- unitconv_energy(foodConsumption$Units[1], ounit)
            foodConsumption$value <- foodConsumption$value * cfac
            foodConsumption$Units <- ounit
        }

        foodConsumption
    }
}

#' Biomass Production Data Module
#'
#' Produce biomass production by crop type, including forest
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{subsector}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.biomass_production <- function(mode, allqueries, aggkeys, aggfn, years,
                                      filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Biomass Production'
    }
    else {
        message('Function for processing variable: Biomass Production')

        bioProduction <- allqueries$'Biomass Production' %>%
                         filter(years, filters) %>%
                         aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_energy(bioProduction$Units[1], ounit)
            bioProduction$value <- bioProduction$value * cfac
            bioProduction$Units <- ounit
        }

        bioProduction
    }
}

#' Biomass Consumption Data Module
#'
#' Produce biomass consumption by crop type
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{output}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.biomass_consumption <- function(mode, allqueries, aggkeys, aggfn, years,
                                       filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Biomass Consumption'
    }
    else {
        message('Function for processing variable: Biomass Consumption')

        bioConsumption <- allqueries$'Biomass Consumption' %>%
                          filter(years, filters) %>%
                          aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cfac <- unitconv_energy(bioConsumption$Units[1], ounit)
            bioConsumption$value <- bioConsumption$value * cfac
            bioConsumption$Units <- ounit
        }

        bioConsumption
    }
}
