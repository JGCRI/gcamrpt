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
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.agriculture <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Ag Production'
    }
    else {
        message('Function for processing variable: Agricultural Production')

        agproduction <- allqueries$'Ag Production'
        agproduction <- filter(agproduction, years, filters)
        agproduction <- aggregate(agproduction, aggfn, aggkeys)

        if(!is.na(ounit)) {
            # Only try to convert units if the input units are all the same
            if(length(unique(agproduction$Units)) != 1) {
                if("Mt" %in% agproduction$Units)
                    ounit <- "MT"
                else
                    ounit <- agproduction$Units[1]
                warning(paste("Input units are not all the same, removing values
                              not in", ounit))
                agproduction <- dplyr::filter(agproduction, Units == ounit)
            }

            cfac <- unitconv_mass(agproduction$Units[1], ounit)
            agproduction$value <- agproduction$value * cfac
            agproduction$Units <- ounit
        }

        agproduction
    }
}


#' Agriculture Production Data Module
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

