#### This file is for functions supporting the runModule generic method.  The
#### actual module functions will be grouped into other files by functional
#### area.

#' Run a data module
#'
#' Run the module associated with object \code{var}.  If mode is \code{\link{GETQ}},
#' return a character vector of the names of all GCAM queries needed as input by
#' the module.  If mode is \code{\link{run}}, return a data frame containing the
#' processed variable.
#'
#' @param var Name of the variable to produce.  This must be one of the varibles
#' known to the system.  The \code{\link{listModules}} function lists the known
#' variables.
#' @param mode Either \code{GETQ} or \code{RUN}.  The former returns
#' the names of the GCAM queries needed for the calculation; the latter runs the
#' calculation.  In \code{GETQ} mode all of the remaining arguments are
#' ignored.
#' @param allqueries List of all the queries pulled by the system.
#' @param aggkeys Character string listing the aggregation columns desired.
#' All of these columns must be present in the table computed by the main body
#' of the function.  If empty or NULL, no aggregation will be performed.
#' @param aggfn Function to use for aggregating.  If none specified,
#' \code{\link[base]{sum}} will be used.
#' @param strtyr Start year.  All years prior to this will be dropped.
#' @param endyr End year.  All years after this will be dropped.
#' @param filters Character string giving a list of additional filters to be
#' applied, in s-exp format.
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @export
runModule <- function(var, mode, allqueries=NULL, aggkeys=NULL, aggfn=NULL,
                      strtyr=NULL, endyr=NULL, filters=NULL, ounit=NULL)
{
    if(is.null(aggfn) || is.na(aggfn)) {
        aggfn <- base::sum
    }
    else {
        getaggfn(aggfn)
    }

    fun <- tryCatch(
        ## Find the corresponding function, limited to the package environment
        get(canonicalForm(var), envir=environment(runModule), mode='function',
            inherits=FALSE),
        error = function(e) {
            ## Maybe should make this a warning and return NULL?
            stop('Unknown variable request:  ', var)
        },
        finally = NULL
        )
    fun(var, mode, allqueries, aggkeys, aggfn, strtyr, endyr, filters, ounit)
}


#' Convert a variable name to canonical form
#'
#' Canonical form means all non-alphanumerics are converted to \code{_}, and
#' "module." is prepended to the name.
#'
#' @param var Variable name to convert
#' @importFrom magrittr %>%
#' @keywords internal
canonicalForm <- function(var)
{
    . <- NULL
    tolower(var) %>%
      gsub('[^a-z0-9]', '_', . ) %>%
      paste0('module.',  . )
}


#' Test function for runModule
#'
#' This function prints a diagnostic and returns an empty data frame.
#'
#' @keywords internal
module.test_fun <- function(var, mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                    filters, ounit)
{
    if(mode == GETQ) {
        character()
    }
    else {
        print(paste('Function for processing variable', var))
        data.frame()
    }
}

#' List the variables that the system knows how to generate
#'
#' Returns a character vector listing the variables that currently have recipes
#' in the system.
#'
#' @importFrom magrittr %>%
#' @export
listModules <- function()
{
    . <- NULL                           # suppress package warnings
    ls(environment(listModules), pattern='^module\\.') %>%
      gsub('^module\\.', '', . )
}

#' Population Data Module
#'
#' In mode = GETQ, returns query title. In mode = RUN, returns requested data.
#'
#' @keywords internal

module.population <- function(var, mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                            filters, ounit)
{
    if(mode == GETQ) {
        # As a procedure, should return title from query file or manual return?
        # For more complex variables, will return multiple query titles.
        'Population'
    }
    else {
        print(paste('Function for processing variable', var))
        data.frame()
    }
}
#' Table of allowable aggregation functions
#'
#' @keywords internal
AGGFNTBL <- list(
    sum = base::sum,
    mean = base::mean,
    max = base::max,
    min = base::min,
    median = stats::median
    )


#' Look up an aggregation function by name.
#'
#' Look up an aggregation function by name, restricting it to a whitelisted set
#' of known functions.
#'
#' @param fname Name of the requested function.
#' @keywords internal
getaggfn <- function(fname)
{
    if(fname %in% names(AGGFNTBL)) {
        AGGFNTBL[[fname]]
    }
    else {
        warning('Function ', fname,
                ' not found in allowed aggregation function table.\n',
                'No aggregation will be performed.')
        NULL
    }
}


#' Token indicating get-queries mode for \code{\link{runModule}} family of
#' functions.
#' @keywords internal
GETQ <- 'GETQUERIES'

#' Token indicating run mode for \code{\link{runModule}} family of functions.
#' @keywords internal
RUN <- 'RUN'
