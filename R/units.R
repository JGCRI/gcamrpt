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
#' @importFrom lubridate year
#' @keywords internal
unitconv_usdollar <- function(module_data, ounit)
{
    if (length(unique(module_data$Units)) > 1) {
        warning(paste("Variable reported in multiple units"))
    } else if (module_data$Units[1] != ounit) {
        iunit <- module_data$Units[1]
        year_pattern <- "[0-9]{2}"

        # extract input year
        m <- regexpr(year_pattern, iunit)
        input_year <- paste("19", regmatches(iunit, m), sep="")

        # regexp extract output year
        m <- regexpr(year_pattern, ounit)
        output_year <- paste("19", regmatches(ounit, m), sep="")

        # take GDPDEF data. average over each year
        gdpdef <- system.file("extdata", "GDPDEF.csv", package="iamrpt")
        gdpdef <- read.table(gdpdef, header=TRUE, sep=",", as.is=TRUE)
        gdpdef$DATE <- as.POSIXlt(gdpdef$DATE, format = "%Y-%m-%d") #convert to POSIXct class
        gdpdef$year <- year(gdpdef$DATE) # add year column
        gdpdef.grp.year <- gdpdef[c("GDPDEF", "year")] %>%
            group_by(year) %>%
            summarize(avg = mean(GDPDEF))
        gdpdef.grp.year<- data.frame(gdpdef.grp.year) #return to data.frame (tibble after pipe operations)

        # calculate conversion factor
        conv_factr <- gdpdef.grp.year[which(gdpdef.grp.year$year == output_year), 'avg'] / gdpdef.grp.year[which(gdpdef.grp.year$year == input_year), 'avg']

        # unit conversion
        module_data[['value']] <- module_data[['value']] * conv_factr

        # rename units column
        module_data[['Units']] <- rep(ounit, length(module_data[['Units']]))

    }
}
