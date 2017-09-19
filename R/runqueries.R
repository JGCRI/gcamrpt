#### Functions for running GCAM queries needed by modules.

### For the time being, we will use local databases.  For performance reasons we
### will probably eventually want to switch to a baseX server, but that's a bit
### more work to set up.

#' Run the input list of queries against a GCAM output database.
#'
#' Run all of the queries in the input list against the specified database.  The
#' result will be returned as a list of data frames.
#'
#' The \code{dbloc}, and \code{dbfile} arguments give the location and name of
#' the database.  The \code{scenario} argument is passed directly to
#' \code{\link[rgcam]{runQuery}}, and so has the same options as the
#' corresponding argument of that function.  Notably, passing \code{NULL} will
#' cause the last scenario in the database, irrespective of its name, to be
#' retrieved.
#'
#' The input query list is a list of query \emph{titles}.  These will be looked
#' up in the list of query strings parsed by \code{\link{parseQueries}}.
#' Therefore, it is an error to call this function without first calling that
#' one.  It is similarly an error to include in the query list a query that
#' wasn't parsed in the call to \code{parseQueries}.
#'
#' @param qlist Character vector of titles of queries to run.
#' @param dbloc Name of the directory containing the database.
#' @param dbfile Name of the database
#' @param scenario Name of the scenario to query
#'
#' @export
runQueries <- function(qlist, dbloc, dbfile, scenario=NULL)
{
    con <- rgcam::localDBConn(dbloc, dbfile)

    pq <- get('parsed_queries', envir=private)

    qchk <- qlist %in% names(pq)
    if(!all(qchk)) {
        badq <- paste(qlist[!qchk], collapse=', ')
        stop('The following queries were not found in the global query list:',
             badq)
    }

    ## convert the list of query titles into a list of query structures.
    qlist <- pq[unique(qlist)]

    lapply(qlist,
           function(q) {
               rgcam::runQuery(con, q$query, scenario)
           })
}


#' Parse queries from an XML query file
#'
#' This function parses queries from the input file and stores them in the
#' package namespace, where \code{\link{runQueries}} can use them to run any
#' queries requested.
#'
#' This function has the side effect of storing the queries retrieved from the
#' file.  The stored list will persist until the R session is restarted and the
#' package is reloaded.  Subsequent calls will accumulate in the list.  If the
#' titles of any new queries duplicate existing queries, the new ones will
#' overwrite the old ones.
#'
#' @param qfile File containing the XML definitions of the queries.
#' @return \code{invisible(NULL)}
#' @keywords internal
parseQueries <- function(qfile)
{
    if(!file.exists(qfile)) {
        stop('Cannot find query file ', qfile)
    }

    pq <- get('parsed_queries', envir=private)
    qq <- c(rgcam::parse_batch_query(qfile), pq)
    qt <- sapply(qq, function(x) x[['title']])
    ## In case of duplicate titles, this will take the first one it comes to:
    assign('parsed_queries', qq[unique(qt)], envir=private)

    invisible(NULL)
}


#' Storage space for package-private variables
#'
#' @keywords internal
private <- new.env(parent=emptyenv())
assign('parsed_queries', list(), envir=private)
