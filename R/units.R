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
    if(is.null(ounit) || is.na(ounit)) {
        return(module_data)
    }
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))
    iunit <- module_data$Units[1]

    compIunit <- compsplt(iunit, countconv)
    # check if compsplt() found match in countconv
    if (length(compIunit) == 0) {
        warning("Input unit ", iunit,
                " not recognized as a count unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    compOunit <- compsplt(ounit, countconv)
    # check if compsplt() found match in countconv
    if (length(compOunit) == 0) {
        warning("Output unit ", ounit,
                " not recognized as a count unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    cfac <- countconv[compIunit, compOunit]

    module_data[['value']] <- module_data[['value']] * cfac

    ## Update the units
    module_data[['Units']] <- ounit

    module_data
}

#' Load factor
#'
#' GCAM outputs this variable as load/veh, whether Passenger or Freight.
#' Passenger is usually reported as 'persons/vehicle' and Freight as 'tonnes/vehicle
#' Unit conversion functions are called after filtering and aggregation, meaning module_data
#' is a simple time series stripped of information identifying the type of variable reported (ie Passenger vs Freight)
#' Ounit must be specified correctly in the variable control file.
#' iunit is replaced with ounit silently
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_ldfctr <- function(module_data, ounit)
{
    if(is.null(ounit) || is.na(ounit)) {
        return(module_data)
    }
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))

    ## Update the units -- no numerical conversion
    module_data[['Units']] <- ounit

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

    compIunit <-compsplt(iunit, energyconv)
    # check if compsplt() found a match in energyconv
    if(length(compIunit) == 0) {
        warning("Input unit ", iunit,
                " not recognized as an energy unit.  Unit conversion will be skipped.")
        return(module_data)
    }


    compOunit <- compsplt(ounit, energyconv)
    if (length(compOunit) == 0) {
        warning("Output unit ", ounit,
                " not recognized as an energy unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    cfac <- energyconv[compIunit, compOunit]

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

#' Unit conversion for USD into Rupee
#'
#' Convert to 2010 Rupees from USD1990
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @importFrom assertthat assert_that
#' @keywords internal

unitconv_rupee <- function(module_data, ounit) {
    if(is.null(ounit) || is.na(ounit)) {
        return(module_data)
    }
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))

    cfac <- 1.67*45 / 1000
    # native gcam output ~ Million1990US$
    # this cfac true only for ounit ~ billion 2010 Rupee/yr
    # would rather call unitconv_counts() to convert million to billion
    # this is temporary solution

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

#' Split composite units
#'
#' Split composite units (EJ/yr, billion pass-km, etc.), and find canonical version of unit stored
#' in the corresponding conversion matrix.
#' All units in these matrices needed to be lower case.
#' These matrices need to be symmetric for this function to work for both input and output units
#'
#' @param unit Either iunit or ounit (see modules above)
#' @param convmat Conversion matrices stored in sysdata
compsplt <- function(unit, convmat) {
    unitsplt <- unlist(strsplit(unit, '[ -/]')) %>% tolower()

    for(i in seq(1, length(unitsplt))) { # for each component in unit
        # as long as row.names() = colnames(conv), this function can be used for input & output
        compare <- grepl(unitsplt[i], row.names(convmat))
        # is there a matching entry in convmat?\
        if (any(compare)) {
            compunit <- row.names(convmat)[compare]
            break
            # pick out the matching entry in convmat
        }
        else {compunit <- character()} # return empty string
    }

    compunit
}

#' co2 emissions weight conversion
#'
#' This function converts weight
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_co2 <- function(module_data, ounit)
{
    if(is.null(ounit) || is.na(ounit)) {
        return(module_data)
    }
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))
    iunit <- module_data$Units[1]

    # is iunit always MTC? include this section just in case
    compIunit <- tolower(iunit)
    if(!compIunit %in% row.names(emissionsconv)) {
        warning("Input unit ", iunit,
                " not recognized as an emissions unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    compOunit <- compsplt(ounit, emissionsconv)
    # check if compsplt() found match in emissionsconv
    if(length(compOunit) == 0) {
        warning("Output unit ", ounit,
                " not recognized as an emissions unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    cfac <- emissionsconv[compIunit, compOunit]
    module_data[['value']] <- module_data[['value']] * cfac
    ## Update the units
    module_data[['Units']] <- ounit

    module_data
}

#' weight conversion
#'
#' This function converts weight
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_weight <- function(module_data, ounit)
{
    if(is.null(ounit) || is.na(ounit)) {
        return(module_data)
    }
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))
    iunit <- module_data$Units[1]

    if(!iunit %in% row.names(weightconv)) {
        warning("Input unit ", iunit,
                " not recognized as a weight unit.  Unit conversion will be skipped.")
        return(module_data)
    }


    compOunit <- compsplt(ounit, weightconv)
    # check if compsplt() found match in weightconv
    if(length(compOunit) == 0) {
        warning("Output unit ", ounit,
                " not recognized as a weight unit.  Unit conversion will be skipped.")
        return(module_data)
    }

    cfac <- weightconv[iunit, compOunit]

    module_data[['value']] <- module_data[['value']] * cfac

    ## Update the units
    module_data[['Units']] <- ounit

    module_data
}

