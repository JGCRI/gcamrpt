## Functions related to filtering tables


#' Filter an output variable
#'
#' Filter an output variable using startyear and endyear values, as well as the
#' filter string.  The return value is the filtered data set.
#'
#' @param tbl Table to filter
#' @param startyr Start year.  Years less than this will be dropped.
#' @param endyr End year.  Years greater than this will be dropped.
#' @param filterstr Filter string.  A list of filter s-expressions.  These will
#' be parsed and applied to the input table.
#' @return Filtered variable table.
#' @keywords internal
filter <- function(tbl, startyr, endyr, filterstr)
{
    mask <- rep(TRUE, nrow(tbl))
    if((is.na(startyr) || startyr == '')) {
        mask <- mask & (tbl$year >= startyr)
    }

    if(!(is.na(endyr) || endyr == '')) {
        mask <- mask & (tbl$year <= endyr)
    }

    if(!(is.na(filterstr) || filterstr == '')) {
        fsplit <-
            stringr::str_split(filterstr, ',') %>% unlist

        ## This produces a matrix of string matches. Each row is the result of
        ## parsing one filter.  Each column is one of the match groups.
        filters <- stringr::str_match(fsplit, filterpattern)

        ## Check for any failed matches.  These correspond to malformed
        ## filters.  They will be skipped in dofilter()
        fail <- is.na(filters[,1])
        if(any(fail)) {
            ff <- paste(fsplit[fail], collapse='\n')
            warning('The following filters were malformed and will be skipped: ', ff)
        }

        ## This produces a matrix of logical values.  Each *column* is the
        ## result of applying one filter to the table.
        fmasks <- apply(filters, 1, function(x) {dofilter(x, tbl)})

        ## This applies & across every row; the result is true for rows that
        ## passed all the filters, false for those that failed any of them.
        mask <- mask & apply(fmasks, 1, function(x) {all(x)})
    }

    ## select the rows for which all filters returned TRUE.
    tbl[mask,]
}

#' List of available filter functions
#' @keywords internal
filterfns <- list(
    ## == and != are assumed to be string comparisons
    `==` = function(col, x) {col == x},
    `!=` = function(col, x) {col != x},
    ## Order relations assumed to be numeric
    `<` = function(col, x) {col < as.numeric(x)},
    `>` = function(col, x) {col > as.numeric(x)},
    `<=` = function(col, x) {col <= as.numeric(x)},
    `>=` = function(col, x) {col >= as.numeric(x)},
    ## regular expressions
    matches = function(col, x) {grepl(x, col)},
    matchesi = function(col, x) {grepl(x, col, ignore.case=TRUE)},
    notmatches = function(col, x) {!grepl(x, col)},
    notmatchesi = function(col, x) {!grepl(x, col, ignore.case=TRUE)}
    )

#' Regexp for matching '(arg1; arg2; arg3)'
#' @keywords internal
filterpattern <- '\\(([^;]+);([^;]+);([^)]+)\\)'

#' Apply a filter string to a table
#'
#' Parse the filter string and compute a logical vector indicating for each row
#' whether the row is in the result or not.  This logical vector is what is
#' returned, so that we don't attempt to modify the data set until all of the
#' filters have been calculated (allowing us to change the data frame just
#' once).
#'
#' @param fvec Vector of str_match outputs for a single filter
#' @param tbl Table to filter
#' @return logical vector
#' @keywords internal
dofilter <- function(fvec, tbl)
{
    if(!is.na(fvec[1])) {
        fvec <- stringr::str_trim(fvec)
        fstr <- fvec[2]
        colstr <- fvec[3]
        val <- fvec[4]
        if(!(fstr %in% filterfns)) {
            warning('Unknown filter function: ', fstr, '  Skipping.')
            rslt <- rep(TRUE, nrow(tbl))
        }
        else if(!(colstr %in% names(tbl))) {
            warning('Filter function ', fstr, ' column ', colstr,
                    ' is not in table.  Skipping filter.')
            rslt <- rep(TRUE, nrow(tbl))
        }
        else {
            f <- filterfns[[fstr]]
            col <- tbl[[colstr]]
            rslt <- f(col, val)
        }
    }
    else {
        ## This is a filter that didn't parse for some reason.  A warning was
        ## issued above, so just return all true.
        rslt <- rep(TRUE, nrow(tbl))
    }
    rslt
}
