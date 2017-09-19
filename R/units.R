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
#' This function converts data reported as energy, e.g. exaJoules,
#' MegaWatt-hours, etc.
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
        warning("Variable reported in multiple units. ", module_data$Units[1],
                " will be used for all values.")
    }
    iunit <- module_data$Units[1]


    if(!iunit %in% row.names(energyconv)) {
        warning("Input unit ", iunit,
                " not recognized as an energy unit.  Unit conversion will be skipped.")
        return(module_data)
    }
    if(!ounit %in% colnames(energyconv)) {
        warning("Output unit ", ounit,
                " not recognized as an energy unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    cfac <- energyconv[iunit, ounit]

    module_data[['value']] <- module_data[['value']] * cfac

    ## Update the units
    module_data[['Units']] <- ounit

    module_data
}

#' Unit conversion for dollar years
#'
#' Convert US dollar units from one dollar year to another using the GDP
#' deflator obtained from the Federal Reserve's FRED database.
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @importFrom assertthat assert_that
#' @keywords internal
unitconv_usdollar <- function(module_data, ounit)
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
    }

    conv.tbl <- data.frame(inyr=yrparse(module_data$Units),
                           outyr=yrparse(ounit))

    minyr <- min(gdpdef$year)
    maxyr <- max(gdpdef$year)
    inyr.outofrng <-
        (conv.tbl$inyr < minyr) | (conv.tbl$inyr > maxyr)
    outyr.outofrng <-
        (conv.tbl$outyr < minyr) | (conv.tbl$outyr > maxyr)
    if(any(inyr.outofrng) || any(outyr.outofrng)) {
        warning('GDP deflators are defined only between ', minyr,
                ' and ', maxyr,
                ' .\nYears outside this range will use the nearest in-range year.')
        conv.tbl$inyear <- clamp(conv.tbl$inyear, minyr, maxyr)
        conv.tbl$outyear <- clamp(conv.tbl$outyear, minyr, maxyr)
    }

    ## calculate conversion factor
    conv.tbl <-
        dplyr::left_join(conv.tbl, gdpdef, by=c(inyr='year')) %>%
          dplyr::rename(indef=gdpdef) %>%
          dplyr::left_join(gdpdef, by=c(outyr='year')) %>%
          dplyr::rename(outdef=gdpdef)

    cfac <- conv.tbl$outdef / conv.tbl$indef
    module_data[['value']] <- module_data[['value']] * cfac

    ## Update the units
    module_data[['Units']] <- ounit

    module_data
}

#' Parse year values out of a string
#'
#' We don't know whether we will be getting two or 4 digit years, so we try to
#' figure it out as best we can.
#'
#' @param yrstr Character vector of strings to be parsed.
#' @return Integer vector of years.
#' @keywords internal
yrparse <- function(yrstr)
{
    yrpat <- '[0-9]+'
    years <- as.integer(as.vector(stringr::str_match(yrstr, yrpat)))
    if(any(is.na(years))) {
        ## Oh, dear, some strings didn't match the pattern
        badstr <- !grepl(yrpat, yrstr)
        badstr <- paste(yrstr[badstr], collapse=', ')
        warning('The following strings did not contain valid years: ',
                badstr)
    }

    ## For two-digit years, anything less than 47 is assumed to be 20XX;
    ## anything >= 47 is assumed to be 19XX (why 47?  1947 is where the Fed's
    ## GDP deflator starts.
    y20xx <- years < 47
    years[y20xx] <- years[y20xx] + 2000

    ## For any year value <1000, we assume that this is a year offset from 1900;
    ## thus, 50==1950 and 103==2003.
    ylt1k <- years < 1000
    years[ylt1k] <- years[ylt1k] + 1900

    ## Year values > 1000 are assumed to be correct as written.
    years
}


#' Clamp a vector of values to a specified range.
#'
#' How is this not a built-in function already?
#'
#' @param x Vector of values to clamp
#' @param xlo Lower end of the range
#' @param xhi Upper end of the range
#' @keywords internal
clamp <- function(x, xlo, xhi)
{
    pmax(x,xlo) %>% pmin(xhi)
}
