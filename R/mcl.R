#### Main control loop and supporting functions


#' Generate a report for a GCAM experiment
#'
#' Read the user control files (see details below) and run the report generation.
#' For each requested scenario the system will pull the queries needed to
#' compute the variables requested by the user.  The necessary calculations will
#' be run, and the result will be written in the requested format.
#'
#' @section Control files:
#'
#' The report generator requires two \emph{control files}.  The first lists the scenarios
#' to be run.  It should be a CSV file with the following columns:
#' \describe{
#'     \item{\strong{GCAM scenario}}{The name that GCAM used for the scenario.}
#'     \item{\strong{output scenario}}{The name that will be used for the scenario in the
#' final report.}
#'     \item{\strong{scenario db}}{The name of the GCAM database the scenario was recorded in.}
#' }
#' Each row in this table will cause a scenario to be generated in the report.
#'
#' The second control file lists the variables that should be written into the
#' report.  It should have the following columns:
#' \describe{
#'     \item{\strong{GCAM variable}}{The canonical GCAM name for the variable.  The
#' \code{\link{listVariables}} function lists the variables known to the report
#' generation system.}
#'     \item{\strong{output variable}}{The name that will be used for the variable in the
#' output.}
#'     \item{\strong{aggregation keys}}{A comma-separated list of columns to group by
#' when aggregating the raw GCAM output.  This column can be left blank if no
#' aggregation is desired for this variable.}
#'     \item{\strong{aggregation function}}{The function to use in the
#' aggregation. Supported functions are \code{sum}, \code{mean}, \code{max},
#' \code{min}, and \code{median}.  If none is specified, \code{sum} will be
#' used.}
#'     \item{\strong{years}}{List of years to include in the output.  You can
#' list individual years, ranges in the form start:end, or stepped ranges in the
#' form start:end:step.  Ranges are inclusive, so 2000:2010:5 is the same as
#' 2000, 2005, 2010.  If the year list is omitted, all years in the data will be
#' included.}
#'     \item{\strong{filters}}{Arbitrary filters to apply to the table, \emph{before}
#' aggregating.  These should be in the modified s-expression format described
#' below.  If no filters are to be applied this column can be left blank.}
#'     \item{\strong{output units}}{The desired output units for the variable.  The
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
#'     \item{\code{iamrpt.fileformat}}{File format for output.  Options are
#' \code{"CSV"} and \code{"XLSX"}}
#'     \item{\code{iamrpt.scenmerge}}{If \code{TRUE}, for each variable merge the
#' results for all scenarios into a single table (distinguished by the value of
#' the scenario column).  Otherwise, create a separate table for each
#' combination of scenario and variable.}
#'     \item{\code{iamrpt.dataformat}}{Specify the data format; that is, how
#' the data is organized in the output files.  Three options are available:
#'       \describe{
#'         \item{\code{"tabs"}}{Each table generated goes into a separate tab (if
#' XLS output is selected) or file (if CSV output is selected).  The tab or file
#' will be named with the output of the table.}
#'         \item{\code{"merged"}}{The tables will be output sequentially into a
#' single tab or file.  Each table will be preceded by its name.  This is
#' similar to the format used by GCAM to output batch queries.}
#'         \item{\code{"IIASA"}}{The database format used by IIASA.  In this
#' format each table is spread into a row in a merged table, with a column to
#' identify the variable that each row comes from.}
#'    }
#'  }
#'    \item{\code{iamrpt.wideformat}}{If \code{TRUE}, reshape the tables into
#' wide format (years as columns) before output.  Otherwise, leave them in long
#' format.  If the IIASA data format is selected, then this option is ignored,
#' since the IIASA format requires wide data.}
#' }
#'
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
#' (notmatches; technology; CCS)
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
#'
#' The filter functions currently recognized by the system are
#' \describe{
#'   \item{\code{==}}{String equality}
#'   \item{\code{!=}}{String inequality}
#'   \item{\code{<}}{Numeric less-than}
#'   \item{\code{>}}{Numeric greather-than}
#'   \item{\code{<=}}{Numeric less-than-or-equals}
#'   \item{\code{>=}}{Numeric greater-than-or-equals}
#'   \item{\code{matches}}{Regular expression match.  Note that because of the
#' way we parse these strings you can't have a \code{','}, \code{';'},
#' \code{'('}, or \code{')'} in your
#' regular expressions for this function or any of the ones below.}
#'   \item{\code{matchesi}}{Case-insensitive regular expression match.}
#'   \item{\code{notmatches}}{Regular expression inverted match.  That is,
#' select the rows that do \emph{not} match the given regular expression.}
#'   \item{\code{notmatchesi}}{Case-insensitive regular expression inverted
#' match.}
#' }
#'
#' @param scenctl Name of the scenario control file.
#' @param varctl Name of the variable control file.
#' @param dbloc Directory holding the GCAM databases
#' @param outputdir Directory to write output to.  Default is the current
#' working directory.
#' @param model Name of the model (e.g., \code{'GCAM'}).  This is required for
#' the IIASA data format.  It is ignored for all other formats.
#' @param template Name of a csv file containing an IIASA template.  This is
#' only useful in the IIASA format.  See \code{\link{complete_iiasa_template}}
#' for instructions on how to use a template
#' @param fileformat Desired format for output files.
#' @param scenmerge Flag: if true, merge scenarios; otherwise, leave scenarios
#' as separate tables.
#' @param dataformat Indicates desired data format.  Supported formats are
#' \code{'tabs'}, \code{'merged'}, or \code{'IIASA'}
#' @param wideformat Flag: if true, convert data to wide format before output;
#' otherwise, leave in long format.
#' @return NULL; the report will be written to output files as described in the
#' Output section.
#' @importFrom magrittr %>%
#' @export
generate <- function(scenctl,
                     varctl,
                     dbloc,
                     outputdir = getwd(),
                     model = 'GCAM',
                     template = NULL,
                     fileformat = getOption('iamrpt.fileformat', 'CSV'),
                     scenmerge = getOption('iamrpt.scenmerge', TRUE),
                     dataformat = getOption('iamrpt.dataformat', 'tabs'),
                     wideformat = getOption('iamrpt.wideformat', TRUE)
                     )
{
    year <- value <- NULL               # silence package check notes.
    suppressMessages({scenctl <- readr::read_csv(scenctl)})
    suppressMessages({varctl <- readr::read_csv(varctl)})

    validatectl(scenctl, varctl)

    ## special condition:  If using the IIASA format, all variables must be
    ## aggregated to region.  If all left blank, then replace them silently.
    ## Otherwise issue a warning and replace.
    if(dataformat == 'IIASA') {
        if(any(varctl$`aggregation keys` != 'scenario, region') &&
           !(all(is.na(varctl$`aggregation keys`) | varctl$`aggregation keys` == ''))) {
            warning('Variables must be aggregated to region for IIASA output format. ',
                    'Aggregation keys will be replaced with "scenario, region".')
        }
        varctl[['aggregation keys']] <- 'scenario, region'
    }

    gcvars <- varctl[['GCAM variable']]

    ## Collect the queries that we will need to run.
    q2run <-
        sapply(gcvars, function(v) {runModule(v, GETQ)}) %>%
            unlist() %>%
            as.vector() %>%
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
        rslts <- merge_scenarios(rslts)

    if(dataformat == 'IIASA') {
        ## convert results to IIASA format.  If we didn't merge scenarios, write
        ## each one to a separate file named for the scenario; otherwise write a
        ## single file.
        . <- NULL    # suppress notes

        tdata <- read_iiasa_template(template)

        if(scenmerge) {
            # replace empty dfs with null
            vars <- names(rslts)
            for(i in seq(1,length(vars))) {
                var <- vars[i]
                vardf <- rslts[[var]]
                if(nrow(vardf) == 0 )
                    rslts[var] <- NULL
            }

            rslts <- iiasafy(rslts) %>%
                dplyr::mutate(Model=model) %>%
                complete_iiasa_template(tdata) %>%
                iiasa_sortcols() %>%
                list(allscen=.)
            dataformat <- 'merged'
        }

        else {
            # replace empty df's with null
            scens <- names(rslts)
            for(i in seq(1,length(scens))) {
                scen <- scens[i]
                vars <- names(rslts[[scen]])
                for(j in seq(1,length(vars))) {
                    var <- vars[j]
                    vardf <- rslts[[scen]][[var]]
                    if(nrow(vardf) == 0 ) {
                        warning('Scenario ', scen, ' , Variable ', var, ' returned empty')
                        rslts[[scen]][[var]] <- NULL
                    }
                }
            }

            rslts <- lapply(rslts, iiasafy) %>%
              lapply(function(df) {
                  dplyr::mutate(df, Model=model) %>%
                    complete_iiasa_template(tdata) %>%
                    iiasa_sortcols()
              })
            dataformat <- 'tabs'
        }
    }

    # need to replace handling of empty dfs with method used in IIASA
    else if (wideformat) {
            if (scenmerge) {

                # year col must be char
                # handle emtpty df by filling with empty row first
                rslts <- lapply(rslts, function(df) {
                    if (nrow(df) ==0) {
                        df[1,] <- rep(0, ncol(df))

                    }
                    df$year <- unlist(lapply(df$year, toString))
                    df
                })

                rslts <- lapply(rslts, function(df) {tidyr::spread(df, year, value)})
            }
            else {
                rslts<- lapply(rslts, function(df) {lapply(df, tidyr::spread, year,value)})
            }
        }

    output(rslts, dataformat, fileformat, outputdir)

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
        Map(function(var, aggkeys, aggfn, years, filters, ounit) {
                runModule(var, RUN, queries, aggkeys, aggfn, years,
                          filters, ounit)
            },
            varctl[['GCAM variable']],
            varctl[['aggregation keys']],
            varctl[['aggregation function']],
            varctl[['years']],
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
    names(vars) <- vars                 # names attribute will be propagated to
                                        # the result.
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
                 'aggregation function', 'years', 'filters',
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

