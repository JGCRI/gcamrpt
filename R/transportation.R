#### Data modules for the transprtation group

#' Service Output Data Module
#'
#' Produce service output by technology and vintage
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.service_output <- function(mode, allqueries, aggkeys, aggfn, years,
                              filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Service output'
    }
    else {
        message('Function for processing variable: Service output')
        serviceOutput <- allqueries$'Service output'
        serviceOutput <- vint_tech_split(serviceOutput)
        serviceOutput <- parse_sector(serviceOutput)
        serviceOutput <- filter(serviceOutput, years, filters)
        serviceOutput <- aggregate(serviceOutput, aggfn, aggkeys)
        # units example: EJ/yr
        serviceOutput <- unitconv_energy(serviceOutput, ounit)
        serviceOutput
    }
}
#' Energy Data Module
#'
#' Produce final energy by technology and vintage
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.final_energy <- function(mode, allqueries, aggkeys, aggfn, years,
                                filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Final energy'
    }
    else {
        energy <- allqueries$'Final energy'
        energy <- vint_tech_split(energy)
        energy <- parse_sector(energy)
        energy <- filter(energy, years, filters)
        energy <- aggregate(energy, aggfn, aggkeys)
        # units example: million p-km
        energy <- unitconv_counts(energy, ounit)
        energy

    }
}
#' Load Factors Data Module
#'
#' Produce load factors by technology and vintage
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.load_factors <- function(mode, allqueries, aggkeys, aggfn, years,
                                  filters, ounit)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Load factors'
    }
    else {
        message('Function for processing variable: Load factors')
        message('Not implemented yet')
    }
}


#' Split technology into technology and vintage cols
#'
#' Query returns data with Technology column in the format'Technology, year=Vintage'.
#' This function returns the data frame with Technology split into a Technology and
#' Vintage column.
#'
#' Makes use of split.vt() function below. This function must write Vintage column first
#' before rewriting Technology column.
#'
#' @param df Data returned for individual query
#' @keywords internal
vint_tech_split <- function(df) {
    df$vintage <- lapply(df$technology, split.vt, col='vint')
    df$technology <- lapply(df$technology, split.vt, col='tech')
    df
}

#' Parse sector column
#'
#'Query returns data with Sector column in the format 'trn_service_mode_submode'
#'This function returns the data frame with new Service, Mode and Submode cols
#'
#'Makes use of split.sm() function below.
#'
#' @param df Data returned for individual query
#' @keywords internal
parse_sector <- function(df) {

    ## Service cond'ns
    freight <- grepl('freight', df$sector)
    pass <- grepl('pass', df$sector)

    ## Mode cond'ns
    ship <- grepl('ship', tolower(df$subsector))
    av <- grepl('aviation', tolower(df$subsector))
    road <- grepl('road', df$sector)
    rail <- grepl('rail', tolower(df$subsector))

    ## Service
    df[,'service'] <- NA # na.omit(df) later
    df[freight | ship, 'service'] <- 'Freight'
    df[pass, 'service'] <- 'Passenger'

    ## Mode
    df[,'mode'] <- NA #na.omit(df) later
    df[rail, 'mode'] <- 'Rail'
    df[road, 'mode'] <- 'Road'
    df[ship,'mode'] <- 'Shipping'
    df[av, 'mode'] <- 'Aviation'
  
    
    #Submode cond'ns
    df[,'submode'] <- NA #na.omit(df) later
    intl <- grepl('international', tolower(df$subsector)) # av and ship
    dom <- grepl('domestic', tolower(df$subsector)) # av and ship
    
    freightrail <- grepl('freight rail', tolower(df$subsector))
    passrail <- grepl('passenger rail', tolower(df$subsector))
    
    w2 <- grepl('2w', tolower(df$subsector))
    w3 <- grepl('three-wheeler', tolower(df$subsector))
    w4 <- grepl('4w', tolower(df$subsector))
    bus <- grepl('bus', tolower(df$subsector))
    
    t2 <- grepl('0-2t', df$subsector)
    t5 <- grepl('2-5t', df$subsector)
    t9 <- grepl('5-9t', df$subsector)
    t16 <- grepl('9-16t', df$subsector)
    
    #Submode
    df[intl, 'submode'] <- 'International'
    df[dom, 'submode'] <- 'Domestic'
    df[freightrail, 'submode'] <- 'Freight Rail'
    df[passrail, 'submode'] <- 'Passenger Rail'
    df[w2, 'submode'] <- '2W'
    df[w3, 'submode'] <- '3W'
    df[w4, 'submode'] <- '4W'
    df[bus, 'submode'] <- 'Bus'
    df[t2 | t5, 'submode'] <- 'LHDT'
    df[t9, 'submode'] <- 'MHDT'
    df[t16, 'submode'] <- 'HHDT'

    na.omit(df) # remove incomplete observations (no service/mode/submode assigned)
}

#' Split: vintage, technology
#'
#' Using text in the format of 'Technology, year=Vintage', splits text at
#' ',' and '=', and returns entry indicated by col
#'
#' @param text Text entry of original technology column
#' @param col Specifies which string from text to return
#' @keywords internal
split.vt <- function(text, col) {
    splt <- strsplit(text, ',')[[1]]
    if (col =='tech') {
        splt[1]
    } else if (col =='vint') {
        strsplit(splt[2],'=')[[1]][2]
    }
}
