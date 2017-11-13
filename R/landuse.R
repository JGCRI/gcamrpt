#### Data modules for the electricity queries group

#' Electricity Data Module
#'
#' Produce land use by region.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{sector}
#'   \item{subsector}
#'   \item{technology}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.land_cover <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, ounit)
{

    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'Land Cover'
    }
    else {
        ## silence notes on package check
        value <- NULL

        message('Function for processing variable: Land Use')

        # Note that filter and aggregate are defined in filtering.R and
        # aggregate.R respectively
        land_cover <- allqueries$'Land Cover' %>%
                      filter(years, filters) %>%
                      dplyr::mutate(land_allocation = sub('AEZ\\d{2}', '', land_allocation),
                                    land_allocation = sub('Protected', '', land_allocation),
                                    land_allocation = sub('Unmanaged', '', land_allocation),
                                    Units = ounit) %>%
                      dplyr::group_by(Units, scenario, region, year, land_allocation) %>%
                      dplyr::summarise(value = sum(value)) %>%
                      aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_area(land_cover$Units[1], ounit)
            land_cover <- dplyr::mutate(land_cover, value=value*cf)
        }

        land_cover
    }
}
