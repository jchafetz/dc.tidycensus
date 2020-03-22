#' Pulling Census ACS Data for DC
#'
#' @param geography Census geography, e.g. tract, blockgroup, state.
#' @param vars ACS variable names
#' @param year Year of 5-Year ACS, default = 2018
#'
#' @return sf projected in Maryland State Plane - espg 2248
#' @export
#'
#' @examples census_pull("blockgroup", c("B01003_001E", "B19013_001E"))
#'
census_pull <- function(geography, vars, year = 2018){
  df <-
  tidycensus::get_acs(geography = geography,
                      variables = vars,
                      state = "DC",
                      county = "District of Columbia",
                      year = year,
                      geometry = TRUE) %>%
    dplyr::select(GEOID, variable, estimate) %>%
    as.data.frame() %>%
    tidyr::spread(variable, estimate) %>%
    sf::st_as_sf() %>%
    sf::st_transform(2248)

  return(df)

}
