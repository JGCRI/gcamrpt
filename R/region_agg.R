#### Region aggregation functions

#' Convert a table to aggregate regions
#'
#' Aggregate the input table using the specified aggregation regions
#'
#'
#' @param tbl Table to aggregate
#' @param region List of regions in tbl
#' @param agg_region List of regions to aggregate tbl to
#' @keywords internal
region_agg <- function(tbl, region, agg_region, add_global)
{
    if(('region' %in% names(tbl)) & any(!is.null(region)) & any(!is.null(agg_region))){
        data <- tbl %>%
            dplyr::left_join(tibble::tibble(region, agg_region), by = "region")

        grp_cats <- setdiff(names(data), c("region", "value"))

        data <- data %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::rename(region = agg_region)
    } else {
        data <- tbl
    }

    if (add_global){
        grp_cats <- setdiff(names(data), c("region", "value"))

        global <- data %>%
            dplyr::group_by_(.dots = grp_cats) %>%
            dplyr::summarise(value = sum(value)) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(region = "Global")

        data <- dplyr::bind_rows(data, global)
    }
    return(data)
}
