#### Data modules for the land queries group

#' Land Use Change Apportion Data Module
#'
#' Apportion change in land-use by region.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{landleaf}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.land_use_change_apportion <- function(mode, allqueries, aggkeys, aggfn, years,
                               filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'detailed land allocation'
    }
    else {
        message('Function for processing variable: LUC apportion')

        land_alloc <- allqueries$'detailed land allocation' %>%
            # fix land names separated by underscore
            dplyr::mutate(landleaf = sub("Root_Tuber","RootTuber",landleaf),
                          landleaf = sub("biomass_grass","biomassgrass",landleaf),
                          landleaf = sub("biomass_tree","biomasstree",landleaf)) %>%
            # extract land type and location
            dplyr::mutate(landtype = stringr::str_split_fixed(landleaf, "_", 4)[,1],
                          location = stringr::str_split_fixed(landleaf, "_", 4)[,2]) %>%
            # Sum over different water and efficiency types
            dplyr::group_by(scenario, region, landtype, location, year, Units) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Join in aggregate land type
            dplyr::left_join(land_aggregation, by = "landtype")

        # Calculate changes in land type each time period
        land_change <- land_alloc %>%
            # Group by landtype and location to get net change from previous year
            dplyr::group_by(scenario, region, landtype, location) %>%
            dplyr::mutate(change = round(value - dplyr::lag(value),3)) %>%
             # Calculate total land change in each location/year:
            # since we have positive and negative values, sum absolute value, divide by 2
            dplyr::group_by(scenario, region, location, year) %>%
            dplyr::mutate(total_change = sum(abs(change))/2) %>%
            dplyr::ungroup()

        # p <- dplyr::filter(land_change, location == "AdrBlkSea", region == "EU-12", year ==2015)

        # Need a full list of to and from landtypes in each location
        a <- land_change %>%
            dplyr::select(scenario, region, landtype, location, agg_land, year) %>%
            dplyr::distinct()
        b <- land_change %>%
            dplyr::select(scenario, region, landtype, location, agg_land) %>%
            dplyr::distinct()

        land_change_apportion <- dplyr::left_join(a, b, by = c("scenario", "region", "location")) %>%
            dplyr::filter(landtype.x != landtype.y) %>%
            # Join in change to
            dplyr::left_join(land_change, by = c("scenario", "region", "landtype.x" = "landtype", "location", "year")) %>%
            # Join in change from
            dplyr::left_join(land_change, by = c("scenario", "region", "landtype.y" = "landtype", "location", "year")) %>%
            # Apportion change
            dplyr::mutate(change_apportion = 0,
                          change_apportion = dplyr::if_else((change.x > 0 & change.y < 0),
                                                            -1*change.x * change.y / total_change.x,
                                                            change_apportion),
                          change_apportion = dplyr::if_else((change.x < 0 & change.y > 0),
                                                            change.x * change.y / total_change.x,
                                                            change_apportion)) %>%
            dplyr::select(scenario, region, location, landtype.from = landtype.y, landtype.to = landtype.x,
                          agg_land.from = agg_land.y, agg_land.to = agg_land.x,
                          year, value = change_apportion, Units = Units.x)

            #x <- dplyr::filter(land_change_apportion, location == "AdrBlkSea", region == "EU-12", year ==2015)

        # # Function to get all permutations of two tibbles
        # repeat_add_columns <- function (x, y)
        # {
        #     UNIQUE_JOIN_FIELD <- NULL
        #     assert_that(tibble::is_tibble(x))
        #     assert_that(tibble::is_tibble(y))
        #     x %>% dplyr::mutate(UNIQUE_JOIN_FIELD = 1) %>%
        #         dplyr::full_join(dplyr::mutate(y, UNIQUE_JOIN_FIELD = 1), by = "UNIQUE_JOIN_FIELD") %>%
        #         dplyr::select(-UNIQUE_JOIN_FIELD)
        # }

        # # Join data to list of all landtypes/locations
        # land_change_apportion_join <- repeat_add_columns(a,b)
        #
        # land_change_apportion <- land_change_apportion_join %>%
        #     dplyr::filter(location.x == location.y,
        #                   landtype.x != landtype.y) %>%
        #     dplyr::select(-location.y) %>%
        #     dplyr::rename(location = location.x) %>%
        #     # Join in change to
        #     dplyr::right_join(land_change, by = c("scenario", "region", "landtype.x" = "landtype", "location")) %>%
        #     # Join in change from
        #     dplyr::left_join(land_change, by = c("scenario", "region", "landtype.y" = "landtype", "location", "year")) %>%
        #     # Remove if both positive or both negative
        #     dplyr::filter(!(change.x < 0 & change.y < 0),
        #                   !(change.x > 0 & change.y > 0)) %>%
        #     # Apportion change
        #     dplyr::mutate(change_apportion = dplyr::if_else(change.x > 0,
        #                                                     -1*change.x * change.y / total_change.x,
        #                                                     change.x * change.y / total_change.x)) %>%
        #     dplyr::select(scenario, region, location, landtype.from = landtype.y, landtype.to = landtype.x,
        #                   agg_land.from = agg_land.y, agg_land.to = agg_land.x,
        #                   year, value = change_apportion, Units = Units.x)

        # x <- dplyr::filter(land_change_apportion, location == "AdrBlkSea", region == "EU-12", year ==2015)



        # # Need to join land_change > 0 with land_change < 0 to apportion changes
        # land_change_above_0 <- land_change %>%
        #     dplyr::filter(change > 0)
        # land_change_below_0 <- land_change %>%
        #     dplyr::filter(change < 0) %>%
        #     # Add in total change in each location
        #     dplyr::group_by(scenario, region, location, year, Units) %>%
        #     dplyr::mutate(total_change = sum(change)) %>%
        #     dplyr::ungroup()
        #
        # # Want to have two diff landtypes and changes per row
        # land_change_apportion <- dplyr::left_join(land_change_above_0, land_change_below_0,
        #                                    by = c("scenario", "region", "location", "year", "Units")) %>%
        #     dplyr::mutate(change_apportion = change.x * change.y / total_change) %>%
        #     dplyr::select(scenario, region, location, landtype.from = landtype.y, landtype.to = landtype.x,
        #                   agg_land.from = agg_land.y, agg_land.to = agg_land.x,
        #                   year, value = change_apportion, Units)

        land_change_apportion <- filter(land_change_apportion, years, filters)
        land_change_apportion <- aggregate(land_change_apportion, aggfn, aggkeys)
        land_change_apportion <- region_agg(land_change_apportion, region, agg_region, add_global)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_counts(land_change_apportion$Units[1], ounit)
            if(!is.na(cfac)) {
                land_change_apportion$value <- land_change_apportion$value * cfac
                land_change_apportion$Units <- ounit
            }
        }
        land_change_apportion
    }
}

#' LUC Emissions by Land Use Type Data Module
#'
#' Sum LUC emissions by land use type
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{landleaf}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.luc_emissions_detailed <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'LUC emissions by LUT'
    }
    else {
        message('Function for processing variable: LUC emissions detailed')

        luc <- allqueries$'LUC emissions by LUT' %>%
            # fix land names separated by underscore
            dplyr::mutate(landleaf = sub("Root_Tuber","RootTuber",landleaf),
                          landleaf = sub("biomass_grass","biomassgrass",landleaf),
                          landleaf = sub("biomass_tree","biomasstree",landleaf)) %>%
            # extract land type and location
            dplyr::mutate(landtype = stringr::str_split_fixed(landleaf, "_", 4)[,1],
                          location = stringr::str_split_fixed(landleaf, "_", 4)[,2]) %>%
            # Sum over different water and efficiency types
            dplyr::group_by(scenario, region, landtype, location, year, Units) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Join in aggregate land type
            dplyr::left_join(land_aggregation, by = "landtype")


        luc <- filter(luc, years, filters)
        luc <- aggregate(luc, aggfn, aggkeys)
        luc <- region_agg(luc, region, agg_region, add_global)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_co2(luc$Units[1], ounit)
            if(!is.na(cfac)) {
                luc$value <- luc$value * cfac
                luc$Units <- ounit
            }
        }
        luc
    }
}

#' Land Use Change Apportion Data Module
#'
#' Apportion change in land-use by region.
#'
#' The raw table used by this module has columns:
#' \itemize{
#'   \item{scenario}
#'   \item{region}
#'   \item{landleaf}
#'   \item{year}
#'   \item{value}
#'   \item{Units}
#' }
#'
#' @keywords internal

module.land_use_allocation <- function(mode, allqueries, aggkeys, aggfn, years,
                                             filters, ounit, region, agg_region, add_global)
{
    if(mode == GETQ) {
        # Return titles of necessary queries
        # For more complex variables, will return multiple query titles.
        'detailed land allocation'
    }
    else {
        message('Function for processing variable: land use allocation')

        land_alloc <- allqueries$'detailed land allocation' %>%
            # fix land names separated by underscore
            dplyr::mutate(landleaf = sub("Root_Tuber","RootTuber",landleaf),
                          landleaf = sub("biomass_grass","biomassgrass",landleaf),
                          landleaf = sub("biomass_tree","biomasstree",landleaf)) %>%
            # extract land type and location
            dplyr::mutate(landtype = stringr::str_split_fixed(landleaf, "_", 4)[,1],
                          location = stringr::str_split_fixed(landleaf, "_", 4)[,2]) %>%
            # Sum over different water and efficiency types
            dplyr::group_by(scenario, region, landtype, location, year, Units) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            # Join in aggregate land type
            dplyr::left_join(land_aggregation, by = "landtype")

        land_alloc <- filter(land_alloc, years, filters)
        land_alloc <- aggregate(land_alloc, aggfn, aggkeys)
        land_alloc <- region_agg(land_alloc, region, agg_region, add_global)

        if(!is.na(ounit)) {
            ## skip unit conversion if output unit not specified.
            cfac <- unitconv_counts(land_alloc$Units[1], ounit)
            if(!is.na(cfac)) {
                land_alloc$value <- land_alloc$value * cfac
                land_alloc$Units <- ounit
            }
        }
        land_alloc
    }
}
