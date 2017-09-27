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
        serviceOutput <- allqueries$'Service output'
        serviceOutput <- tech_vint_split(serviceOutput)
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


#' Split technology into technology and vintage cols
#'
#' Query returns data with Technology column in the format'Technology, year=Vintage'.
#' This function returns the data frame with Technology split into a Technology and
#' Vintage column.
#'
#' Makes use of general split() function below. This function must write Vintage column first
#' before rewriting Technology column.
#'
#' @param data Data returned for individual query
#' @keywords internal
tech_vint_split <- function(data) {
    data$vintage <- lapply(data$technology, split, col='vint')
    data$technology <- lapply(data$technology, split, col='tech')
}

#' Text split function
#'
#' Using text in the format of 'Technology, year=Vintage', splits text at
#' ',' and '=', and returns entry indicated by col
#'
#' @param text Text entry of original technology column
#' @param col Specifies which string from text to return
#' @keywords internal
split <- function(text, col) {
    splt <- strsplit(text, ',')[[1]]
    if (col =='tech') {
        splt[1]
    } else if (col =='vint') {
        strsplit(splt[2],'=')[[1]][2]
    }
}

