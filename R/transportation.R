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
        serviceOutput <- tech_vint_split(serviceOutput)
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
#' @param data Data returned for individual query
#' @keywords internal
vint_tech_split <- function(data) {
    data$vintage <- lapply(data$technology, split, col='vint')
    data$technology <- lapply(data$technology, split, col='tech')
}

#' Parse sector column
#'
#'Query returns data with Sector column in the format 'trn_service_mode_submode'
#'This function returns the data frame with new Service, Mode and Submode cols
#'
#'Makes use of split.sm() function below.
#'
#' @param data Data returned for individual query
#' @keywords internal
parse_sector <- function(data) {
    data$service <- lapply(data$sector, split.sm, col='service')
    data$mode <- lapply(data$sector, split.sm, col='mode')
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

#' Split: Service, Mode, Submode
#'
#' Using text in the approximate format of 'trn_service_mode', splits text at '_' and returns
#' new cols
#'
#' @param text Text entry of original technology column
#' @param col Specifies which string from text to return
#' @keywords internal
split.sm <- function(text, col) {
    splt <- strsplit(text, '_')[[1]]
    if(col=='service') {

        if('freight' %in% splt | 'shipping' %in% splt) {'Freight'}

        if('pass' %in% splt && 'road' %in% splt) {'Passenger'} # unsure about trn_pass

        else if ('pass' %in% splt) {
            Message('trn_pass mapped to any Service/Mode/Submode')
        }
    }

    if(col=='mode') {

        if('freight' %in% splt) {
            if('road' %in% splt) {'Road'}
            else ('Rail')
        }

        if('road' %in% splt) {'Road'}

        if('shipping' %in% splt) {'Shipping'}
    }

    if(col=='submode') {

        if('road' %in% splt) {

            if('pass' %in% splt) {  # pass && road
                if('LDV' %in% splt) { # pass && road && LDV
                    if('2W' %in% splt) {'2W'}
                    else if('4W' %in% splt) {'LDV'}
                    else {'3W'}
                } else {'Bus'} # pass && road
            }


            # freight && road
            if('freight' %in% splt) {
                message('sector: trn_freight_road')
                message('Need to look at subsector info to determine Submode')
            }


        else if('freight' %in% splt) {'Freight Rail'} # !('road') && 'freight'

        else if('shipping' %in% splt) {'Shipping'} # !('road') && 'shipping'
        }
    }
}
