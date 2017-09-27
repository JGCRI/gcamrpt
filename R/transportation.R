#### Data modules for the transprtation group

#' Service Output Data Module
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

module.service_output <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Service output'
    }
    else {
        message('Function for processing variable: Service output')
        message('Not implemented yet')
    }
}

#' Load Factors Data Module
#'
#' Produce load factors by technology and vintage
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

module.load_factors <- function(mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                                  filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Load factors'
    }
    else {
        message('Function for processing variable: Load factors')
        message('Not implemented yet')
    }
}
