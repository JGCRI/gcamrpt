## Functions that format the report output

#' Output function for CSV format
#'
#' @param rslts Results tables from \code{\link{generate}}.  This must be either
#' a list of data frames or a list of lists of data frames.
#' @param dataformat Indicator of data format:  If 'tabs', write to separate files; if 'merged'
#' write merged results to a single file.
#' @param fileformat File format for the output:  'CSV' or 'XLSX'
#' @param dirname Directory to write output file(s) into.
#' @importFrom assertthat assert_that
#' @keywords internal
output <- function(rslts, dataformat, fileformat, dirname)
{
    ## results should be a list of data frames or a list of lists of data
    ## frames.
    assert_that(is.list(rslts), !is.data.frame(rslts))

    ## First flatten the list, if the scenarios haven't already been combined.
    isdf <- sapply(rslts, is.data.frame)
    if(any(isdf)) {
        assert_that(all(isdf),
                    msg='output_csv:  Invalid results structure, unbalanced tree.')
        ## This list is ready to go in the next step
    }
    else {
        ## Two-level tree, with scenarios at the top level
        rslts <- unlist(rslts, recursive=FALSE)
        isdf <- sapply(rslts, is.data.frame)
        assert_that(all(isdf),
                    msg='output_csv: Invalid results structure, lists nested > 2 deep.')
    }

    if(!fileformat %in% c('CSV', 'XLSX')) {
        warning('Unknown fileformat requested: ', fileformat, '.  Using CSV.')
        fileformat = 'CSV'
    }

    ## Now we should have a list of data frames.  Output them to file(s) one
    ## by one.
    if(dataformat=='tabs') {
        ## One file or tab for each table
        if(fileformat == 'XLSX')
            wb <- openxlsx::createWorkbook()

        for(tblname in names(rslts)) {
            if(fileformat == 'CSV') {
                filename <-
                    alternate_filename(file.path(dirname,
                                                 paste0(tblname, '.csv')))
                message('Writing file ', filename)
                readr::write_csv(rslts[[tblname]], filename)
            }
            else {
                openxlsx::addWorksheet(wb, tblname)
                openxlsx::writeData(wb, tblname, rslts[[tblname]])
            }
        }

        if(fileformat == 'XLSX') {
            filename <- alternate_filename(file.path(dirname, 'iamrpt.xlsx'))
            message('Writing file ', filename)
            openxlsx::saveWorkbook(wb, filename)
        }
    }
    else {
        ## Single file in PITA format.
        if(fileformat == 'CSV') {
            filename <- alternate_filename(file.path(dirname, 'iamrpt.csv'))
            fcon <- file(filename, 'w')
        }
        else {
            filename <- alternate_filename(file.path(dirname, 'iamrpt.xlsx'))
            wb <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb, 'iamrpt')
            row <- 1                    # current write row
        }
        message('Writing file ', filename)
        line1 <- TRUE
        for(tblname in names(rslts)) {
            if(line1) {
                ## Don't write an extra newline at the start of the line
                line1 <- FALSE
            }
            else {
                if(fileformat == 'CSV') {
                    cat('\n', file=fcon)
                }
                else {
                    openxlsx::writeData(wb, 'iamrpt' , '', startRow=row)
                    row <- row + 1
                }
            }

            if(!('Variable' %in% names(rslts[[tblname]]))) {
                if(fileformat == 'CSV') {
                    cat(tblname, '\n', file=fcon, sep='')
                }
                else {
                    openxlsx::writeData(wb, 'iamrpt', tblname, startRow=row)
                    row <- row + 1
                }
            }

            if(fileformat == 'CSV') {
                readr::write_csv(rslts[[tblname]], fcon)
            }
            else {
                openxlsx::writeData(wb, 'iamrpt', rslts[[tblname]],
                                    startRow=row)
                row <- row + nrow(rslts[[tblname]]) + 1
            }
        }
        if(fileformat == 'CSV') {
            close(fcon)
        }
        else {
            openxlsx::saveWorkbook(wb, filename)
        }
    }
    invisible(NULL)
}


#' Generate an alternate file name, if input name is already in use.
#'
#' Generate the alternate name by appending NNN to the base name, where NNN is
#' the smallest integer for which the resulting name isn't already in use.
#'
#' @param name The intended file name.
#' @keywords internal
alternate_filename <- function(name)
{
    name <- name
    dir <- dirname(name)
    filename <- basename(name)
    np <- nameparse(filename)
    stem <- np[1]
    ext <- np[2]

    i <- 0
    while(file.exists(name)) {
        i <- i + 1
        if(is.na(ext)) {
            filename <- sprintf('%s%03d', stem, i)
        }
        else {
            filename <- sprintf('%s%03d.%s', stem, i, ext)
        }
        name <- file.path(dir, filename)
    }

    normalizePath(name, mustWork=FALSE)
}


#' Separate a filename into a stem and extension.
#'
#' The stem is everything up to the last '.'.  The extension is everything after
#' that.  If the filename has no '.'s in it, the stem is the whole name, and the
#' extension is NA.
#'
#' @param name Name to parse
#' @keywords internal
nameparse <- function(name)
{
    splt <- unlist(stringr::str_split(name, stringr::coll('.')))
    len <- length(splt)
    if(len == 1) {             # no extension
        c(splt, NA)
    }
    else {
        c(stringr::str_c(splt[1:(len-1)], collapse='.'), splt[len])
    }
}


#' Convert a list of tables to a single table in IIASA format
#'
#' The result of this transformation will be a single table with the following
#' columns:
#'
#' \itemize{
#'   \item{Model}
#'   \item{Scenario}
#'   \item{Region}
#'   \item{Variable (taken from the output name of the input)}
#'   \item{Unit}
#'   \item{NNNN - one for each year}
#' }
#'
#' @param datalist List of data frames, one for each variable.
#' @keywords internal
iiasafy <- function(datalist)
{
    varlist <- lapply(datalist, proc_var_iiasa)

    varlist <- lapply(names(varlist),   # Add variable name (need access to names(varlist) for this.)
                      function(var) {
                          dplyr::mutate(varlist[[var]], Variable=var)
                      }) %>%
      dplyr::bind_rows()              # Combine into a single table
}


#' Select the columns needed for the IIASA format
#'
#' Starting with data in long format, keep only the columns needed to form the
#' IIASA format, namely, scenario, region, year, value, and Units.  Then rename
#' variables according to the IIASA conventions, and spread to wide format.  We don't
#' add the model or variable names at this point, however.
#' @keywords internal
proc_var_iiasa <- function(df)
{
    scenario <- region <- year <- value <- Units <- NULL # silence
                                        # check notes
    df <- df %>%
        dplyr::select(scenario, region, year, value, Units) %>%
        dplyr::rename(Scenario=scenario, Region=region, Unit=Units) %>%
        tidyr::spread(year, value)
}

#' Put columns in canonical order for IIASA data format
#'
#' @param df Data frame
#' @keywords internal
iiasa_sortcols <- function(df)
{
    cols <- unique(c('Model', 'Scenario', 'Region', 'Variable', 'Unit', names(df)))
    dplyr::select(df, dplyr::one_of(cols))
}
