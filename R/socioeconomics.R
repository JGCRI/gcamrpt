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
        # As a procedure, should return title from query file or manual return?
        # For more complex variables, will return multiple query titles.
        'Population'
    }
    else {
        print(paste('Function for processing variable', var))
        population <- allqueries$'Population'; print(paste('Subset queries to ', var))
        population$value <- aggfn(population$value); print('Aggregated')
        population <- subset(population, Year >= strtyr & Year < endyr); print('Apply start and end years')
        population <- filterfn(population, filters); print('Apply filters')
        population <- unitconv(population, ounit); print('Convert units')
        population
    }
}


#' Filtering function
#'
#' This function filters query output according to filters parameter
#'
#' @param filters Character string giving a list of additional filters to be
#' applied, in s-exp format.
#' @keywords internal
filterfn <- function(module_data, filters)
{
    #Needs work
    module_data
}


#' Unit conversion
#'
#' This function converts module data into output units.
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv <- function(module_data, ounit)
{
    #Needs work
    module_data
}
