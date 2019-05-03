## Startup code to be run when the package is first loaded

.onLoad <- function(libname, pkgname)
{
    ## Load the default set of GCAM queries.
    parseQueries(system.file('extdata/default-queries.xml', package=pkgname))
}
