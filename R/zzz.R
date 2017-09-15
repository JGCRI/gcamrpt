## Startup code to be run when the package is first loaded

.onLoad <- function(libname, pkgname)
{
    qfile <- system.file('extdata/default-queries.xml', package=pkgname)
    parseQueries(qfile)
}
