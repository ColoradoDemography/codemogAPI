#' API Wrapper to call Colorado county population estimates
#'
#' This API wrapper returns a dataframe of county populations for the years
#'  and counties selected.
#'
#'  Note: Selecting all of the counties can be done by providing 300 as the fips_list
#'
#' @param fips_list Numeric FIPS code(s) for the county (0 for the state) (no leading 0's)
#' @param year_list Numeric list of years between 2010 and 2014
#' @importFrom jsonlite read_json
#' @import dplyr
#'
#' @export


county_est = function(fips_list, year_list, vars = "totalpopulation") {
    # subject to change, but this is the server URL
    url_stub = "https://gis.dola.colorado.gov/lookups/munipophousing?"

    # Type checks for each list so that we don't get any text sent to the API for a SQL Injection.
    if (!is.numeric(fips_list))
        stop("FIPS should be numeric.")
    if (!is.numeric(year_list))
        stop("Years should be numeric.")

    if (any(year_list < 2010) || any(year_list > 2016))
        stop("One or more year is out of range. Years should be between 2010 and 2016")

    # Checks for two special call types 300 is for all counties, 0 is for state total, puts proper list
    # together for call
    suppressWarnings(if (fips_list == 300 | fips_list == 0) {
        fips = c(1, 3, 5, 7, 9, 11, 13, 14, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43,
            45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89,
            91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115, 117, 119, 121, 123, 125)
    } else {

        fips = fips_list
    })
    # Creates the URL for the API call
    call = paste0(url_stub, "year=", paste(year_list, collapse = ","), "&countyfips=", paste(fips, collapse = ","),
        "&compressed=yes")

    # Makes the API call and converts the JSON to a data frame
    data = jsonlite::read_json(call, simplifyVector = TRUE) %>% inner_join(county_names, by = "countyfips")

    # Checks if the State Total call (0) is used and if so creates the total, otherwise fixes the type and
    # naming for totalPopulation
    suppressWarnings(if (fips_list == 0) {
        data = group_by(data, year) %>% summarize(totalPopulation = sum(as.numeric(totalpopulation))) %>%
            mutate(countyfips = 0) %>% select(countyfips, county, year, totalPopulation)
    } else {
        data = mutate(data, totalpopulation = as.numeric(totalpopulation)) %>% rename(totalPopulation = totalpopulation) %>%
            select(countyfips, county, year, totalPopulation)
    })
    # tells the function what to return and changes it from a dplyr tbl object back to a generic data
    # frame
    return(as.data.frame(data))

}
