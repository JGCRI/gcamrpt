## Process GDP deflator data from FRED into a table with one value per year.


calc.gdpdef <- function(filename) {
### take GDPDEF data. average over each year
    gdpdef <- readr::read_csv(filename)
    ## add year column
    gdpdef$year <- lubridate::ymd(gdpdef$DATE) %>% lubridate::year()

    dplyr::select(gdpdef, year, GDPDEF) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(gdpdef = mean(GDPDEF))
}
