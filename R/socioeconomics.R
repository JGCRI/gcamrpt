#### Data modules for the socioeconomics group

#' Population Data Module
#'
#' Produce population by region.
#'
#' Columns:
#' \itemize{
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{units}
#' }
#'
#' @keywords internal

module.population <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Population'
    }
    else {
        message('Function for processing variable: Population')

        population <- allqueries$'Population'
        population <- aggregate(population, aggfn, aggkeys)
        population <- filter(population, startyr, endyr, filters)
        population <- unitconv_counts(population, ounit)
        population
    }
}


#' Unit conversion: counts
#'
#' This function converts data reported in counts, e.g. thousands, millions, etc..
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv <- function(module_data, ounit)
{
    #Needs work
    message("Counts unit conversion function not yet implemented. Data returned unmodified")
    module_data
}
