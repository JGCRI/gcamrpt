#### Data modules for the agriculture and animals group

#' Agriculture Production Data Module
#'
#' Produce agricultural production by crop type
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
                        filter(years, filters) %>%
                        aggregate(aggfn, aggkeys) %>%
                        dplyr::select(-output)

        if(!is.na(ounit)) {
            cfac <- unitconv_mass(agproduction$Units[1], ounit)
            agproduction$value <- agproduction$value * cfac
            agproduction$Units <- ounit
        }

        agproduction
    }
}


#' Produce food consumption by type
#'
#' Unlike Crop Consumption, this module also includes meat demand
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

module.crop_consumption <- function(mode, allqueries, aggkeys, aggfn, years,
                                    filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Crop Consumption'
    }
    else {
        message('Function for processing variable: Crop Consumption')

        cropConsumption <- allqueries$'Crop Consumption'
        cropConsumption <- filter(cropConsumption, years, filters)
        cropConsumption <- aggregate(cropConsumption, aggfn, aggkeys)

        if(!is.na(ounit)) {
            warning("Unit conversion is not currently supported for this
                    variable.")
        }
        cropConsumption$output <- NULL
        cropConsumption
    }
}

#' Produce biomass production by crop type
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
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

#' Produce biomass production by crop type
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
        'Crop Consumption'
    }
    else {
        message('Function for processing variable: Biomass Consumption')

        bioConsumption <- allqueries$'Crop Consumption' %>%
                          dplyr::filter(output == 'biomass') %>%
                          filter(years, filters) %>%
                          aggregate(aggfn, aggkeys) %>%
                          dplyr::select(-output)

        if(!is.na(ounit)) {
            cfac <- unitconv_energy(bioConsumption$Units[1], ounit)
            bioConsumption$value <- bioConsumption$value * cfac
            bioConsumption$Units <- ounit
        }

        bioConsumption
    }
}
