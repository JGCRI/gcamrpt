## Functions related to filtering tables


#' Filter an output variable
#'
#' Filter an output variable using startyear and endyear values, as well as the
#' filter string.  The return value is the filtered data set.
#'
#' @param tbl Table to filter
#' @param years String listing the years to include in the output
#' @param filterstr Filter string.  A list of filter s-expressions.  These will
#' be parsed and applied to the input table.
#' @return Filtered variable table.
#' @keywords internal
filter <- function(tbl, years, filterstr, filter_operator)
{
    mask <- rep(TRUE, nrow(tbl))

    if(!(is.na(years) || years == '')) {
        years <- parse_yrlist(years)
        mask <- mask & (tbl$year %in% years)
    }

    if(!(is.na(filter_operator) || filter_operator == '')) {
        fsplit_o <-
            stringr::str_split(filter_operator, ',') %>% unlist
    }
    else {
        fsplit_o <- NA
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

        if((length(fsplit_o)>1)&(fsplit_o[1]=="AND")&(fsplit_o[2]=="OR")) {

            fmasks <- cbind(fmasks[ , 1], (rowSums(fmasks[ ,c(-1)])==1))
            mask <- mask & apply(fmasks, 1, function(x) {all(x)})
        }
        else if((length(fsplit_o)==1)&(is.na(filter_operator))&(nrow(filters)==1)) {
            mask <- mask & apply(fmasks, 1, function(x) {all(x)})
        }
        else if((length(fsplit_o)==1)&(!is.na(filter_operator))&(filter_operator=="OR")) {
            mask <- (rowSums(fmasks)==1)
        }
        else if((length(fsplit_o)==1)&(!is.na(filter_operator))&(filter_operator=="AND")) {
            mask <- mask & apply(fmasks, 1, function(x) {all(x)})
        }
        else if((length(fsplit_o)==1)&(is.na(filter_operator))&(nrow(filters)>1)) {
            warning('Filter operator not provided.  Applying "AND".')
            mask <- mask & apply(fmasks, 1, function(x) {all(x)})
        }
        else {
            warning('Unrecognized filter operator, "', filter_operator, '".  Applying "AND".')
            mask <- mask & apply(fmasks, 1, function(x) {all(x)})
        }

        ## This applies & across every row; the result is true for rows that
        ## passed all the filters, false for those that failed any of them.
        #mask <- mask & apply(fmasks, 1, function(x) {all(x)})
    }

    ## select the rows for which all filters returned TRUE.
    tbl[mask,]
}

#' List of available filter functions
#' @keywords internal
filterfns <- list(
    ## == and != are assumed to be string comparisons
    '==' = function(col, x) {col == x},
    '!=' = function(col, x) {col != x},
    ## Order relations assumed to be numeric
    '<' = function(col, x) {col < as.numeric(x)},
    '>' = function(col, x) {col > as.numeric(x)},
    '<=' = function(col, x) {col <= as.numeric(x)},
    '>=' = function(col, x) {col >= as.numeric(x)},
    ## regular expressions
    matches = function(col, x) {grepl(x, col)},
    matchesi = function(col, x) {grepl(x, col, ignore.case=TRUE)},
    notmatches = function(col, x) {!grepl(x, col)},
    notmatchesi = function(col, x) {!grepl(x, col, ignore.case=TRUE)}
    )

#' Regexp for matching '(arg1; arg2; arg3)'
#' @keywords internal
filterpattern <- '\\(([^();]+);([^;]+);([^)]+)\\)'

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
        if(!(fstr %in% names(filterfns))) {
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


#' Parse a string describing the years to include in the output
#'
#' Parse the input string and return an integer vector of years to include.
#'
#' The years are specified as a comma-separated list, where each element is one
#' of:
#' \itemize{
#'   \item{A single integer, which includes that individual year}
#'   \item{A range in the form start:end, which includes all years from start to
#' end, inclusive.}
#'   \item{A stepped range in the form start:end:step, which includes all years
#' from start to end in steps of step.}
#' }
#' @param yrlist String containing the year list
#' @keywords internal
parse_yrlist <- function(yrlist)
{
    yrsep <- stringr::str_split(yrlist,',') %>%
      unlist() %>%  stringr::str_trim()

    yrsep <- sapply(yrsep, function(x) {stringr::str_split(x, ':')})

    lapply(yrsep, yr2list) %>%
      unlist(use.names=FALSE) %>%
      unique() %>%
      sort()

}

#' Helper function for parse_yrlist
#'
#' Process each year specification
#'
#' @param y Year spec
#' @keywords internal
yr2list <- function(y)
{
    if(length(y) == 0) {
        ## empty string
        return(NULL)
    }

    y <- as.integer(y)
    if(length(y) > 3) {
        ## Got something like x:y:z:w; ignore everything after the 'z'
        warning('Badly formatted year spec: ', stringr::str_c(y, collapse=':'))
    }

    if(length(y)==1) {
        y
    }
    else if(length(y)==2) {
        seq(y[1], y[2])
    }
    else {
        seq(y[1], y[2], y[3])
    }
}
