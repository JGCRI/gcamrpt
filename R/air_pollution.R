#### Data modules for the air pollution queries group

#' Air Pollution Data Module
#'
#' Produce air pollution emissions by sector and region.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{subsector}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.air_pollution <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, filter_operator, ounit)
{
  if(mode == GETQ) {
    # Return titles of necessary queries
    # For more complex variables, will return multiple query titles.
    'BC emissions by subsector'
  }
  else {
    message('Function for processing variable: Air pollution precursor emissions')
    air_pollution <- allqueries$'BC emissions by subsector'
    air_pollution <- filter(air_pollution, years, filters, filter_operator)
    air_pollution <- aggregate(air_pollution, aggfn, aggkeys)

    air_pollution
  }
}
