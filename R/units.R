#### Unit conversion Functions


#' Counts
#'
#' This function converts data reported in counts, e.g. thousands, millions, etc..
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_counts <- function(module_data, ounit)
{
    #Needs work
    message("Counts unit conversion function not yet implemented. Data returned unmodified")
    module_data
}


#' Energy
#'
#' This function converts data reported as energy, e.g. exaJoules, MegaWatt-hours, etc..
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_energy <- function(module_data, ounit)
{
    if(is.null(ounit) || is.na(ounit)) {
        return(module_data)
    }
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))

    if (length(unique(module_data$Units)) > 1) {
        ## We'll accommodate this case as best we can, but there is something
        ## squirrelly about this data.
        warning("Avast! This variable be reported in multiple units.  Arrr!")
    } else {
        iunit <- module_data$Units[1]
    }

    cfacs <- system.file("extdata", "energyconv.csv", package="iamrpt")
    cfacs <- utils::read.csv(cfacs, header=TRUE, row.names=1, sep=",")
    cfac <- cfacs[iunit, ounit]

    module_data[['value']] <- module_data[['value']] * cfac

    ## Update the units
    module_data[['Units']] <- ounit

    module_data
}
