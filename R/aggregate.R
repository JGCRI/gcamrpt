#### Aggregation functions

#' Aggregate a table by specified keys
#'
#' Aggregate the input table using the specified aggregation function,
#' using the specified columns as keys.
#'
#' The \code{multiple} argument is used for cases where we need to perform the
#' same aggregation on multiple values.  Usually this occurs when we have joined
#' two tables, so for now the aggregation operates only on columns matching the
#' regular expression \code{'value'}, which would catch the \code{'value.x'} and
#' \code{'value.y'} created by a join.  In future versions we may allow
#' specifying arbitrary columns, but until we are fully upgraded to dplyr 0.7
#' that is too much of a hassle.
#'
#' @param tbl Table to aggregate
#' @param aggfn String giving the aggregation function.  If missing, use
#' \code{\link[base]{sum}}.
#' @param aggkeys String giving a list of aggregation keys.  If missing, return
#' the table unmodified.
#' @param multiple If \code{TRUE}, aggregate multiple columns.  For now, all of
#' these columns must match the regex 'value' (e.g., 'value.x' and 'value.y')
#' @keywords internal
aggregate <- function(tbl, aggfn, aggkeys, multiple=FALSE)
{
    if(is.null(aggkeys) || is.na(aggkeys)) {
        return(tbl)
    }
    else {
        aggkeys <- unlist(strsplit(aggkeys, split=",")) %>%
            stringr::str_trim(side="both")
    }
    value <- NULL                       # silence NSE note in pkg check.

    if(is.null(aggfn) || is.na(aggfn)) {
        aggfn <- base::sum
    }
    else {
        aggfn <- getaggfn(aggfn)
        if(is.null(aggfn))
            return(tbl)
    }

    if (sum(!(aggkeys %in% names(tbl))) > 0 ) {
        warning("Agg keys ",
                paste0(aggkeys[!(aggkeys %in% names(tbl))], collapse=", ") ,
                       " not found in variable data.")
        aggkeys <- aggkeys[aggkeys %in% names(tbl)]
    }

    dots <- c('Units', 'scenario', 'year', aggkeys)

    if(multiple) {
        dplyr::group_by_(tbl, .dots=dots) %>%
          dplyr::summarise_at(dplyr::vars(dplyr::matches('value')), aggfn) %>%
          dplyr::ungroup()
    }
    else {
        dplyr::group_by_(tbl, .dots=dots) %>%
          dplyr::summarise(value=aggfn(value)) %>%
          dplyr::ungroup()
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
