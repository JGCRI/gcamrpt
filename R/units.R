#### Unit conversion Functions

#' Calculate unit conversion factors
#'
#' Return a factor for converting the input unit to the output unit.  This
#' factor should be applied as a multiplier:  out = in * fac.  In the event the
#' conversion could not be performed (e.g., because one of the units couldn't be
#' identified), \code{NA} will be returned (we return NA instead of NULL so that
#' you can multiply several conversion factors and check once at the end whether
#' they all succeeded).
#'
#' @param iunit Units for the input data
#' @param ounit Desired output unit.
#' @param inverse If \code{TRUE} invert the conversion factor.  Use this in
#' cases where the unit being converted is in the denominator; \emph{e.g.}
#' cost/EJ -> cost/MWh.
#'
#' @name unitconv
NULL

#' @describeIn unitconv Convert counting units (thousands, millions, etc.)
#'
#' Supported units are described in \code{\link{parsecounts}}.
unitconv_counts <- function(iunit, ounit, inverse=FALSE)
{

    unitasserts(iunit, ounit)

    iufac <- parsecounts(iunit)
    oufac <- parsecounts(ounit)

    if(is.null(iufac) || is.null(oufac)) {
        ## warning will have been issued in parsecounts, so no need to issue
        ## another one here.
        as.numeric(NA)
    }
    else {
        ## see notes in parsecounts()
        if(inverse)
            iufac / oufac
        else
            oufac / iufac
    }
}


#' Parse counting units (thousands, millions, etc) from strings
#'
#' Counting units tend to get treated as words rather than symbols, and thus
#' have a lot of variant forms.  Here, we parse these with regular expressions
#' that should catch the most common ones.  When we find a unit, we return the
#' conversion factor to convert it to ones.  For example, "thousands" -> 0.001.
#' This allows us to construct the conversion from unit 'a' to unit 'b' by
#' dividing val(b)/val(a).  Note that an empty input string is treated as ones
#' (i.e., if you don't specify a count, it's assumed to be ones).
#'
#' If we can't identify a unit, then we issue a warning and return NA.  This
#' will cause \code{\link{unitconv_counts}} to abort the unit conversion.
#'
#' Right now we cover all of the units I've seen in the GCAM output, plus a few
#' other obvious extensions, but there are many more possibilities.  The units
#' are matched by case-insensitive regular expression; the matches currently
#' implemented are:
#'
#' \describe{
#'   \item{(blank string)}{Ones}
#'   \item{\code{hundred[ -]?thous}}{Hundred thousands}
#'   \item{\code{hundred}}{Hundreds (the compound 'hundred thousands' is checked
#'   first so that this match will not capture it.)}
#'   \item{\code{thous}}{Thousands (including abbreviations like 'thous' or
#'   'thousand')}
#'   \item{\code{mil}}{Millions, including abbreviations}
#'   \item{\code{bil}}{Billions, including abbreviations}
#'   \item{\code{tril}}{Trillions, including abbreviations.  This is the largest
#'   unit we expect to see written out (instead of denoted with a metric prefix).}
#' }
#'
#' @param unit The counting unit to convert.
parsecounts <- function(unit)
{
    unit <- stringr::str_trim(unit)

    if(unit == '' | grepl('person|vehicle|m2|km', unit)) {
        1
    }
    else if(grepl('hundred[ -]?thous', unit, ignore.case=TRUE)) {
        ## compound units have to come first, to ensure they don't get grabbed
        ## by one of their constituent parts
        1e-5
    }
    else if(grepl('hundred', unit, ignore.case=TRUE)) {
        0.01
    }
    else if(grepl('thous', unit, ignore.case=TRUE)) {
        0.001
    }
    else if(grepl('mil', unit, ignore.case=TRUE)) {
        1e-6
    }
    else if(grepl('bil', unit, ignore.case=TRUE)) {
        1e-9
    }
    else if(grepl('tril', unit, ignore.case=TRUE)) {
        1e-12
    }
    else {
        warning('String ', unit, ' was not recognized as a counting unit.')
        NA
    }
}


#' @describeIn unitconv Convert energy units
#'
#' Supported units: EJ, TJ, MJ, MWh, TWh
unitconv_energy <- function(iunit, ounit, inverse=FALSE)
{
    unitasserts(iunit, ounit)


    iunit <- extractunit(rownames(energyconv), iunit, 'energy')
    ounit <- extractunit(colnames(energyconv), ounit, 'energy')

    if(is.null(iunit) || is.null(ounit)) {
        ## one of the units was unrecognized, so skip it.
        as.numeric(NA)
    }
    else {
        if(inverse)
            ## swap the units in the lookup to get the inverse factor.
            energyconv[ounit, iunit]
        else
            energyconv[iunit, ounit]
    }
}

#' @describeIn unitconv Adjust US Dollar units for inflation.
#'
#' Convert US dollar units from one dollar year to another using the GDP
#' deflator obtained from the Federal Reserve's FRED database.
unitconv_usdollar <- function(iunit, ounit, inverse=FALSE)
{
    ## Unlike many of the other conversion functions, this one actually handles
    ## multiple input units.  It's not clear this is a capability we need, but
    ## since we've got it, we'll leave it in place.
    assert_that(is.character(iunit))
    assert_that(is.character(ounit))

    if (length(unique(iunit)) > 1) {
        ## We'll accommodate this case as best we can, but there is something
        ## squirrelly about this data.
        warning("Avast! This variable be reported in multiple units.  Arrr!")
    }

    conv.tbl <- data.frame(inyr=yrparse(iunit),
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

    if(inverse) {
        conv.tbl$indef / conv.tbl$outdef
    }
    else {
        conv.tbl$outdef / conv.tbl$indef
    }
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
    ## anything >= 47 is assumed to be 19XX (Why 47?  1947 is where the Fed's
    ## GDP deflator starts.)
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
    assert_that(length(compunit) ==1)
    compunit
}

#' co2 emissions weight conversion
#'
#' This function converts weight
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_co2 <- function(iunit, ounit)
{
    unitasserts(iunit, ounit)

    ## Units are either C (mass-carbon) or CO2 (mass-carbon dioxide).  There may
    ## be other units attached (e.g. MT, or the like), so we have to look to see
    ## what we have.  If we don't see an explicit CO2 marker, we assume that C
    ## was intended.  Don't use extractunit for this because 'C' is a subset of
    ## 'CO2'

    if(grepl('CO2', iunit, ignore.case=TRUE)) {
        iunit <- 'CO2'
    }
    else {
        iunit <- 'C'
    }
    if(grepl('CO2', ounit, ignore.case=TRUE)) {
        ounit <- 'CO2'
    }
    else {
        ounit <- 'C'
    }

    emissionsconv[iunit, ounit]
}

#' weight conversion
#'
#' This function converts weight
#'
#' @param ounit Desired output unit.  If omitted, results will be returned with
#' no unit conversion.
#' @keywords internal
unitconv_mass <- function(iunit, ounit, inverse=FALSE)
{
    unitasserts(iunit, ounit)

    iunit <- extractunit(rownames(massconv), iunit, 'mass')
    ounit <- extractunit(colnames(massconv), ounit, 'mass')

    # In case either unit has different case, change here
    iunit <- colnames(massconv)[grep(iunit, colnames(massconv), ignore.case = TRUE)]
    ounit <- colnames(massconv)[grep(ounit, colnames(massconv), ignore.case = TRUE)]
    if(inverse)
        massconv[ounit, iunit]
    else
        massconv[iunit, ounit]

}

#' Common assertions on unit string inputs
#'
#' All of the unit conversion functions are looking for a length-1 character
#' vector.  This function checks that these conditions are met.
#'
#' The conditions are applied as assertions and will therefore cause errors
#' rather than warnings if they fail.  This a departure from our usual
#' practice.  We do it because having the wrong length or wrong type is a
#' programming error, not a data error.  Therefore, we expect these to trigger
#' only in during testing new modules, in which case an error and immediate halt
#' is appropriate.
#'
#' @importFrom assertthat assert_that
#' @param iunit Input unit
#' @param ounit Output unit
unitasserts <- function(iunit, ounit)
{
    assert_that(length(iunit) == 1)
    assert_that(is.character(iunit))
    assert_that(length(ounit) == 1)
    assert_that(is.character(ounit))
}


#' Determine which unit in a list a string matches
#'
#' Match a string one by one against a list of allowed values.  Return the one
#' that matched, or NULL (with warning) if none of them was matched.
#'
#' @param unitlist List of allowed units (e.g., the column names of a conversion
#' matrix).
#' @param unitstr The string to match against the allowed units
#' @param type The type of unit we were trying to match (used to create warning
#' messages).
#' @keywords internal
extractunit <- function(unitlist, unitstr, type)
{
    ## If we get a match, this should return a single '1'.  The names
    unit <- lapply(unitlist, function(p){stringr::str_match(unitstr, stringr::regex(p, ignore_case = TRUE))}) %>%
      unlist(use.names=FALSE)

    unit <- unit[!is.na(unit)]

    if(length(unit) == 0) {
        warning('String ', unitstr, ' is not recognized as a unit for ', type)
        NULL
    }
    else if(length(unit) > 1) {
        warning('String ', unitstr, ' matches multiple units: ', unit)
        NULL
    }
    else {
        unit
    }
}
