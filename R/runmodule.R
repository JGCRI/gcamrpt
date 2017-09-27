#### This file is for functions supporting the runModule generic method.  The
#### actual module functions will be grouped into other files by functional
#### area.

#' Run a data module
#'
#' Run the module associated with object \code{var}.  If mode is \code{\link{GETQ}},
#' return a character vector of the names of all GCAM queries needed as input by
#' the module.  If mode is \code{\link{RUN}}, return a data frame containing the
#' processed variable.
#'
#' @param var Name of the variable to produce.  This must be one of the varibles
#' known to the system.  The \code{\link{listVariables}} function lists the known
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
#' @param years Years to include in the output
#' @param filters Character string giving a list of additional filters to be
#' applied, in s-exp format.
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
runModule <- function(var, mode, allqueries=NULL, aggkeys=NULL, aggfn=NULL,
                      years=NULL, filters=NULL, ounit=NULL)
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
    fun(mode, allqueries, aggkeys, aggfn, years, filters, ounit)
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
module.test_fun <- function(mode, allqueries, aggkeys, aggfn, years,
                            filters, ounit)
{
    if(mode == GETQ) {
        "Test Query"
    }
    else {
        message('Test Module')
        data.frame()
    }
}

#' List the variables that the system knows how to generate
#'
#' Returns a character vector listing the variables that currently have recipes
#' in the system.  The variable names will be printed in canonical form, which
#' means that all letters will be converted to lowercase, and all
#' non-alphanumeric characters will be replaced with \code{_}s.  Names of
#' variables requested for output by users will similarly be converted to
#' canonical form.  Users can take advantage of this to make the variable lists
#' in their input more readable.  For example, the variable with canonical name
#' \code{gdp_mer_} can be represented as \code{GDP(MER)} in input files.
#'
#' Processing the data for a variable is done in a function called
#' module.varname, for example, the variable \code{gdp_mer_} is processed in
#' \code{\link{module.gdp_mer_}}.  The documentation for these functions
#' contains a list of the columns in the raw table produced by the function,
#' prior to aggregation or filtering.  This information is potentially useful
#' for planning filtering or aggregation operations.  The module documentation
#' will also have any notes pertinent to the computation of the data.
#' @importFrom magrittr %>%
#' @export
listVariables <- function()
{
    . <- NULL                           # suppress package warnings
    ls(environment(listVariables), pattern='^module\\.') %>%
      gsub('^module\\.', '', . )
}


#' Token indicating get-queries mode for \code{\link{runModule}} family of
#' functions.
#' @keywords internal
GETQ <- 'GETQUERIES'

#' Token indicating run mode for \code{\link{runModule}} family of functions.
#' @keywords internal
RUN <- 'RUN'
