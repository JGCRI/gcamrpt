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
        population <- filter(population, strtyr, endyr, filters)
        population <- unitconv_counts(population, ounit)
        population
    }
}
