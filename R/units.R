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
    #Needs work
    message("Energy unit conversion function not yet implemented. Data returned unmodified")
    module_data
}

#' Dollar Years
#'
#' This function converts dollar values from different years
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_usdollar <- function(module_data, ounit)
{
    #Needs work
    message("Dollar value conversion function not yet implemented. Data returned unmodified")
    module_data

    #average over each year
    gdpdef <- system.file("extdata", "GDPDEF.csv", package="iamrpt")
    gdpdef <- read.table(gdpdef, header=TRUE, sep=",", as.is=TRUE)
    gdpdef$DATE = as.POSIXlt(gdpdef$DATE, format = "%Y-%m-%d") #convert to POSIXct class
    gdpdef$year = year(gdpdef$DATE) # add year column
    gdpdef.grp.year = gdpdef[c("GDPDEF", "year")] %>%
        group_by(year) %>%
        summarize(avg=mean(GDPDEF))
    gdpdef.grp.year = data.frame(gdpdef.grp.year) #return to data.frame (tibble after pipe operations)

    # Check current units
    iunits <- module_data$units
    # dollar value conversions
    val.conversion = overnight[c("cost.year", "year")]
    rows = match(val.conversion$cost.year, gdpdef.grp.year$year)
    # positions in gdpdef.grp.year that give the GDPDEF for the cost.year
    val.conversion = val.conversion %>%
        mutate( deflator= gdpdef.grp.year[rows,"avg"], #populate column w/ GDPDEF values
                sevenfive.value.factor = gdpdef.grp.year[gdpdef.grp.year$year==1975, "avg"] / deflator ) #multiply by cost to get in 1975$
}
