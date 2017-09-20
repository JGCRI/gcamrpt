## Functions that format the report output

#' Output function for CSV format
#'
#' @param rslts Results tables from \code{\link{generate}}.  This must be either
#' a list of data frames or a list of lists of data frames.
#' @param tabs Flag indicating whether variables should be written to separate
#' tabs/files.
#' @param dirname Directory to write output file(s) into.
#' @importFrom assertthat assert_that
#' @keywords internal
output_csv <- function(rslts, tabs, dirname)
{
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

    ## Now we should have a list of data frames.  Output them to file(s) one
    ## by one.
    if(tabs) {
        ## One file for each table
        for(tblname in names(rslts)) {
            filename <- alternate_filename(file.path(dirname, paste0(tblname,
                                                                     '.csv')))
            message('Writing file ', filename)
            readr::write_csv(rslts[[tblname]], filename)
        }
    }
    else {
        ## Single file in PITA format.
        filename <- alternate_filename(file.path(dirname, 'iamrpt.csv'))
        message('Writing file ', filename)
        fcon <- file(filename, 'w')
        line1 <- TRUE
        for(tblname in names(rslts)) {
            if(line1) {
                ## Don't write an extra newline at the start of the line
                line1 <- FALSE
            }
            else {
                cat('\n', file=fcon)
            }

            cat(tblname, '\n', file=fcon, sep='')
            readr::write_csv(rslts[[tblname]], fcon)
        }
        close(fcon)
    }
    invisible(NULL)
}


#' @describeIn output_csv Output function for XLSX format
#' @importFrom assertthat assert_that
#' @importFrom xlsx write.xlsx2

#' @keywords internal
#'
output_xlsx <- function(rslts, tabs, dirname)
{
  assert_that(is.list(rslts), !is.data.frame(rslts))

  ## First flatten the list, if the scenarios haven't already been combined.
  isdf <- sapply(rslts, is.data.frame)
  if(any(isdf)) {
    assert_that(all(isdf),
                msg='output_xlsx:  Invalid results structure, unbalanced tree.')
    ## This list is ready to go in the next step
  }
  else {
    ## Two-level tree, with scenarios at the top level
    rslts <- unlist(rslts, recursive=FALSE)
    isdf <- sapply(rslts, is.data.frame)
    assert_that(all(isdf),
                msg='output_xlsx: Invalid results structure, lists nested > 2 deep.')
  }

  ## Now we should have a list of data frames.  Output them to file(s) one
  ## by one.
  if(tabs) {
    ## One file for each table
    for(tblname in names(rslts)) {

      # xlsx workbook function instead
      filename <- alternate_filename(file.path(dirname, paste0('iamrpt', '.xlsx')))
      xlsx::write.xlsx2(rslts[[tblname]], filename, sheetName="tblname")
    }
  }
  else {
    ## Single file in PITA format.
    filename <- alternate_filename(file.path(dirname, 'iamrpt.xlsx'))
    line1 <- TRUE
    for(tblname in names(rslts)) {
      if(line1) {
        ## Don't write an extra newline at the start of the line
        line1 <- FALSE
      }
      else {
          xlsx::write.xlsx2('', filename, append=TRUE)
      }
        xlsx::write.xlsx2(tblname, filename, append=TRUE)
        xlsx::write.xlsx2(rslts[[tblname]], filename, append=TRUE)
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
