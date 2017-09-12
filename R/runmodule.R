#### This file is for functions supporting the runModule generic method.  The
#### actual module functions will be grouped into other files by functional
#### area.

#' Run a data module
#'
#' Run the module associated with object \code{var}.  If mode is \code{\link{getq}},
#' return a character vector of the names of all GCAM queries needed as input by
#' the module.  If mode is \code{\link{run}}, return a data frame containing the
#' processed variable.
#'
#' @param var Name of the variable to produce.  This must be one of the varibles
#' known to the system.
#' @param mode Either \code{getq} or \code{run}.  The former returns
#' the names of the GCAM queries needed for the calculation; the latter runs the
#' calculation.  In \code{getq} mode all of the remaining arguments are
#' ignored.
#' @param allqueries List of all the queries pulled by the system.
#' @param aggkeys Character string listing the aggregation columns desired.
#' All of these columns must be present in the table computed by the main body
#' of the function.  If empty or NULL, no aggregation will be performed.
#' @param aggfn Function to use for aggregating.  If none specified,
#' \code{\link{base::sum}} will be used.
#' @param strtyr Start year.  All years prior to this will be dropped.
#' @param endyr End year.  All years after this will be dropped.
#' @param filters Character string giving a list of additional filters to be
#' applied, in s-exp format.
#' @export
runModule <- function(var, mode, allqueries=NULL, aggkeys=NULL, aggfn=NULL,
                      strtyr=NULL, endyr=NULL, filters=NULL)
{
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
    fun(var, mode, allqueries, aggkeys, aggfn, strtyr, endyr, filters)
}


#' Convert a variable name to canonical form
#'
#' Canonical form means all non-alphanumerics are converted to \code{_}, and
#' "module." is prepended to the name.
#'
#' @param var Variable name to convert
#' @keywords internal
canonicalForm <- function(var)
{
    ## Should we do case-folding as well?
    paste0('module.', gsub('[^a-zA-Z0-9]', '_', var) )
}


#' Test function for runModule
#'
#' This function prints a diagnostic and returns an empty data frame.
#'
#' @keywords internal
module.test_fun <- function(var, mode, allqueries, aggkeys, aggfn, strtyr, endyr,
                    filters)
{
    if(mode == getq) {
        character()
    }
    else {
        print(paste('Function for processing variable', var))
        data.frame()
    }
}


#' Token indicating get-queries mode for \code{\link{runModule}} family of
#' functions.
#' @keywords internal
getq <- 'GETQUERIES'

#' Token indicating run mode for \code{\link{runModule}} family of functions.
#' @keywords internal
run <- 'RUN'
