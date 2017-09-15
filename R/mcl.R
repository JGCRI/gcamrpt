#### Main control loop and supporting functions


#' Generate a report for a GCAM experiment
#'
#' Read the user input files (see details below) and run the report generation.
#' For each requested scenario the system will pull the queries needed to
#' compute the variables requested by the user.  The necessary calculations will
#' be run, and the result will be written in the requested format.
#'
#' @section Control files:
#'
#' The report generator requires two control files.  The first lists the scenarios
#' to be run.  It should be a CSV file with the following columns:
#' \describe{
#'     \item{GCAM scenario}{The name that GCAM used for the scenario.}
#'     \item{output scenario}{The name that will be used for the scenario in the
#' final report.}
#'     \item{scenario db}{The name of the GCAM database the scenario was recorded in.}
#' }
#' Each row in this table will cause a scenario to be generated in the report.
#'
#' The second control file lists the variables that should be written into the
#' report.  It should have the following columns:
#' \describe{
#'     \item{GCAM variable}{The canonical GCAM name for the variable.  The
#' \code{\link{listModules}} function lists the variables known to the report
#' generation system.}
#'     \item{output variable}{The name that will be used for the variable in the
#' output.}
#'     \item{aggregation keys}{A comma-separated list of columns to group by
#' when aggregating the raw GCAM output.  This column can be left blank if no
#' aggregation is desired for this variable.}
#'     \item{aggregation function}{The function to use in the
#' aggregation. Supported functions are \code{sum}, \code{mean}, \code{max},
#' \code{min}, and \code{median}.  If none is specified, \code{sum} will be
#' used.}
#'     \item{start year}{First year to include in the report (inclusive).}
#'     \item{end year}{Last year to include in the report (inclusive).}
#'     \item{filters}{Arbitrary filters to apply to the table, \emph{before}
#' aggregating.  These should be in the modified s-expression format described
#' below.  If no filters are to be applied this column can be left blank.}
#'     \item{output units}{The desired output units for the variable.  The
#' report generation system will attempt to convert the units and will throw an
#' error if it fails.  If output in GCAM native units is desired, this column
#' can be left blank.}
#' }
#'
#' @section Output:
#'
#' The system has several options for formatting output.  These can be passed as
#' arugments to the system, or set as R options.  The names of the options and
#' their functions are:
#' \describe{
#'     \item{iamrpt.fileformat}{File format for output.  Options are
#' \code{"CSV"} and \code{"XLSX"}}
#'     \item{iamrpt.scenmerge}{If \code{TRUE}, for each variable merge the
#' results for all scenarios into a single table (distinguished by the value of
#' the scenario column).  Otherwise, create a separate table for each
#' combination of scenario and variable.}
#'     \item{iamrpt.tabs}{If \code{TRUE}, write each table to a separate tab (if
#' outputting to an xlsx file) or file (if outputting to csv files).  In the
#' former case each tab/file will be named with the output variable name and
#' scenario (if applicable).  In the latter case all of the tables will be
#' written into a single tab or file, with the name of the scenario and variable
#' before each table.}
#' }
#'
#' Output filenames will be chosen automatically.  For an XLSX file the filename
#' will be 'iamrpt.xlsx'.  For CSV output with \code{tabs == FALSE} the result
#' will be 'iamrpt.csv'.  For CSV output with \code{tabs == TRUE} the output
#' files will be named with the scenario (if scenario tables are not merged) and
#' variable output names, for example 'Reference-GDP.csv'.  In any of these
#' cases, if a file already exists with the automatically chosen filename, the
#' first unused number will be appended, e.g., iamrpt001.xlsx, iamrpt002.xlsx,
#' etc.
#'
#' The system produces a variety of diagnostic messages as it runs.  These can
#' be suppressed with \code{\link[base]{suppressMessages}}.  More serious
#' problems will be indicated with warnings.
#'
#' @section Filters:
#'
#' Filters are specified using three-element modified s-expressions.  For
#' example:
#'
#' (notmatches, technology, CCS)
#'
#' This would describe a filter that would select only those rows for which the
#' technology column does not match the regular expression "CCS".  Multiple
#' filters can be applied by putting them in a comma-separated list.
#'
#' (!=; sector; beef), (!=; sector; sheepgoat)
#'
#' The s-expressions are "modified" by separating the elements with semicolons
#' instead of whitespace.  This allows us to have operands with internal
#' whitespace.  However, leading and trailing whitespace will always be
#' trimmed.
#' @param scenctl Name of the scenario control file.
#' @param varctl Name of the variable control file.
#' @param dbloc Directory holding the GCAM databases
#' @param fileformat Desired format for output files.
#' @param scenmerge Flag: if true, merge scenarios; otherwise, leave scenarios
#' as separate tables.
#' @param tabs Flag: if true, put each table into a separate tab or file.
#' Otherwise, put them all into a single long tab/file.
#' @return NULL; the report will be written to output files as described in the
#' Output section.
#' @importFrom magrittr %>%
#' @export
generate <- function(scenctl,
                     varctl,
                     dbloc,
                     fileformat = getOption('iamrpt.fileformat', 'CSV'),
                     scenmerge = getOption('iamrpt.scenmerge', TRUE),
                     tabs = getOption('iamrpt.tabs', TRUE))
{
    scenctl <- readr::read_csv(scenctl)
    varctl <- readr::read_csv(varctl)

    validatectl(scenctl, varctl)

    gcvars <- varctl[['GCAM variable']]

    ## Collect the queries that we will need to run.
    q2run <-
        sapply(gcvars, function(v) {runModule(v, GETQ)}) %>%
          unique

    ## process the scenarios, one by one
    rslts <- Map(function(scen, dbname) {
                     process_scenario(scen, dbloc, dbname, q2run, varctl)
                 },
                 scenctl[['GCAM scenario']],
                 scenctl[['scenario db']])
    ## rename scenarios
    names(rslts) <- scenctl[['output scenario']]


    if(scenmerge)
        merge_scenarios(rslts)


    if(fileformat == 'XLSX') {
        output_xlsx(rslts, tabs)
    }
    else if(fileformat == 'CSV') {
        output_csv(rslts, tabs)
    }
    else {
        warning('Unknown file format ', fileformat, ' requested. ',
                'Writing as CSV.')
        output_csv(rslts, tabs)
    }

    message('FIN.')
    invisible(NULL)
}


#' Run queries and process results for a single scenario
#'
#' This is the main work function for \code{\link{generate}} and should only be
#' called from there.
#'
#' @param scen Scenario name
#' @param dbloc Directory containing the database
#' @param dbname Name of the database
#' @param q2run Character vector of titles of queries to run.
#' @param varctl Table read in from the variable control file.
#' @keywords internal
process_scenario <- function(scen, dbloc, dbname, q2run, varctl)
{
    message('Processing scenario: ', scen)

    ## Run the required queries
    queries <- runQueries(q2run, dbloc, dbname, scen)

    ## Process each requested variable
    rslts <-
        Map(function(var, aggkeys, aggfn, strtyr, endyr, filters, ounit) {
                runModule(var, RUN, queries, aggkeys, aggfn, strtyr, endyr,
                          filters, ounit)
            },
            varctl[['GCAM variable']],
            varctl[['aggregation keys']],
            varctl[['aggregation function']],
            varctl[['start year']],
            varctl[['end year']],
            varctl[['filters']],
            varctl[['output units']])

    ## Rename variables to their output values
    names(rslts) <- varctl[['output variable']]

    rslts
}


#' Merge tables for multiple scenarios into a single scenario
#'
#' For each variable collect the tables for each of the scenarios and fuse them
#' into a single table.
#'
#' @param rawrslts Results by scenario and variable passed in from main control
#' loop
#' @return List of data frames, one for each variable, with all scenarios
#' included.
#' @keywords internal
merge_scenarios <- function(rawrslts)
{
    ## set of variables should be the same for all scenarios, so we can just
    ## grab the list from the first scenario.
    vars <- names(rawrslts[[1]])
    scenarios <- names(rawrslts)

    lapply(vars,
           function(var) {
               ## pull the tables for all scenarios
               vtbls <- lapply(scenarios,
                               function(scen) {
                                   tbl <- rawrslts[[scen]][[var]]
                                   if(!('scenario' %in% names(tbl)))
                                       tbl$scenario <- scen
                                   tbl
                               })
               dplyr::bind_rows(vtbls)
           })
}


#' Validate the structures read in from the control files
#'
#' Check for the following error conditions:
#' \itemize{
#'   \item{All expected columns are present.}
#'   \item{No empty or missing values in columns where missing values are not
#' permitted.}
#' }
#' Issue warnings for the following conditions:
#' \itemize{
#'   \item{Extraneous columns present}
#' }
#'
#' @param scenctl Scenario control file structure
#' @param varctl Variables control file structure
#' @return NULL; Warnings or errors will be thrown as required.
#' @keywords internal
validatectl <- function(scenctl, varctl)
{
    scencols <- c('GCAM scenario', 'output scenario', 'scenario db')
    scenrqd <- scencols

    varcols <- c('GCAM variable', 'output variable', 'aggregation keys',
                 'aggregation function', 'start year', 'end year', 'filters',
                 'output units')
    varrqd <- varcols[1:2]

    validate1(scenctl, 'scenario control', scencols, scenrqd)
    validate1(varctl, 'variable control', varcols, varrqd)
    invisible(NULL)
}

#' Work function for validatectl
#'
#' @param ctl Control file structure
#' @param ctlname Name of the control file we're testing (used in error messages)
#' @param expectcols Expected columns for this control file
#' @param rqdcols Columns for which data is required (no missing allowed)
#' @keywords internal
validate1 <- function(ctl, ctlname, expectcols, rqdcols) {
    extraneous <- !(names(ctl) %in% expectcols)
    if(any(extraneous)) {
        extstr <- paste(names(ctl)[extraneous], collapse=', ')
        warning('Unrecognized columns in ', ctlname, ' : ', extstr)
    }

    missingcols <- !(expectcols %in% names(ctl))
    if(any(missingcols)) {
        missingstr <- paste(expectcols[missingcols], collapse=', ')
        stop('Columns missing in ', ctlname, ' :  ', missingstr)
    }

    missingdat <- sapply(rqdcols,
                         function(coln) {
                             col <- ctl[[coln]]
                             any(is.na(col) | col == '')
                         })
    if(any(missingdat)) {
        missingstr <- paste(rqdcols[missingdat], collapse=', ')
        stop('Missing data prohibited in these ', ctlname, ' columns: ', missingstr)
    }
}
