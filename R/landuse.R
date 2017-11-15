#### Data modules for non-agriculture land use queries

#' Land Allocation Data Module
#'
#' Produce land use by region. Creates columns named 'aez land allocation',
#' 'land allocation', and ' land type' representing different levels of
#' aggregation of the category of land use (the broadest categories being
#' Biomass, Built-up Area, Cropland Forest, Other Natural Land, Pasture). Note
#' that the the aggkeys and filters arguments must use these new column names.
#'
#' The raw table used by this module has columns: \itemize{ \item{scenario}
#' \item{region} \item{landnode} \item{land_allocation} \item{year} \item{value}
#' \item{Units} }
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

        # Process the results of the Land Cover query.
        # 1. Create a column called 'aez land allocation' containing the AEZ
        #    disaggregation of land use
        # 2. Create a column called 'land allocation' that is an aggregation of
        #    the AEZs and combines categories that are split into protected or
        #    managed
        # 3. Create a column called 'land type', which represents a broad
        #    categorization of land use
        # 4. Apply user filters and aggretations
        land_cover <- allqueries$'Land Cover' %>%
                      dplyr::mutate(`aez land allocation` = land_allocation,
                                    land_allocation = removeAEZ(land_allocation),
                                    land_allocation = sub('Protected', '', land_allocation),
                                    `land allocation` = sub('Unmanaged', '', land_allocation),
                                    `land type` = groupLand(`land allocation`),
                                    Units = ounit) %>%
                      dplyr::group_by(Units, scenario, region, year, `land type`, `land allocation`, `aez land allocation`) %>%
                      dplyr::summarise(value = sum(value)) %>%
                      dplyr::ungroup() %>%
                      filter(years, filters) %>%
                      aggregate(aggfn, aggkeys)

        if(!is.na(ounit)) {
            cf <- unitconv_area(land_cover$Units[1], ounit)
            land_cover <- dplyr::mutate(land_cover, value=value*cf)
        }

        land_cover
    }
}

removeAEZ <- function(landuse) {
    sub('AEZ\\d{2}', '', landuse)
}

groupLand <- function(landtype) {
    categories <- c(
        'UnmanagedForest' = 'Forest',
        'Corn' = 'Forest',
        'FiberCrop' = 'Cropland',
        'FodderGrass' = 'Cropland',
        'FodderHerb' = 'Cropland',
        'MiscCrop' = 'Cropland',
        'OilCrop' = 'Cropland',
        'OtherArableLand' = 'Cropland',
        'OtherGrain' = 'Other Natural Land',
        'PalmFruit' = 'Cropland',
        'Rice' = 'Cropland',
        'Root_Tuber' = 'Cropland',
        'SugarCrop' = 'Cropland',
        'Wheat' = 'Cropland',
        'biomass' = 'Biomass',
        'Jatropha' = 'Biomass',
        'Grassland' = 'Biomass',
        'Shrubland' = 'Other Natural Land',
        'Pasture' = 'Other Natural Land',
        'UnmanagedPasture' = 'Pasture',
        'miscanthus' = 'Pasture',
        'eucalyptus' = 'Biomass',
        'willow' = 'Biomass',
        'ProtectedGrassland' = 'Biomass',
        'ProtectedShrubland' = 'Other Natural Land',
        'ProtectedUnmanagedForest' = 'Other Natural Land',
        'ProtectedUnmanagedPasture' = 'Forest',
        'RockIceDesert' = 'Desert',
        'Tundra' = 'Desert',
        'UrbanLand' = 'Built-up Area',
        'UrbanLandAE' = 'Built-up Area'
    )
    sapply(landtype, function(lt) {
        if(lt %in% names(categories)) categories[[lt]] else lt
    })
}
