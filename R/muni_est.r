#' API Wrapper to call Colorado municipal population estimates
#'
#' This API wrapper returns a dataframe of municipal populations and related variables for the years
#'  and municipalities selected.
#'
#'  Note: Selecting all of the counties can be done by providing 300 as the fips_list
#'
#' @param fips_list Numeric FIPS code(s) for the municipality(ies). Leave blank and specify county to get all places in a county Defaults to blank
#' @param year_list Numeric list of years between 2010 and 2016
#' @param county county FIPS number used to pull either parts of a muni or all munis in a county if fips_list is blank Defaults to Blank
#' @param vars list of variables to pull includes: totalpopulation, householdpopulation, groupquarterspopulation, householdsize totalhousingunits, occupiedhousingunits, and vacanthousingunits Defaults to totalpopulation
#' @param totals controls whether API returns totals or parts of municipalities Defaults to 'yes' to give totals not parts.
#' @importFrom jsonlite read_json
#' @import dplyr
#'
#' @export


muni_est = function(fips_list = "", year_list, county = "", vars = "totalpopulation", totals = "yes") {
    # subject to change, but this is the server URL
    url_stub = "https://gis.dola.colorado.gov/lookups/munipophousing?"

    # Type checks for each list so that we don't get any text sent to the API for a SQL Injection.
    if (!is.numeric(fips_list) && fips_list != "")
        stop("FIPS should be numeric or blank.")

    if (!is.numeric(year_list))
        stop("Years should be numeric.")

    if (county != "")
        totals = "no"

    if (any(year_list < 2010) || any(year_list > 2020))
        stop("One or more year is out of range. Years should be between 2010 and 2020")

    # Creates the URL for the API call
    call = paste0(url_stub, "year=", paste(year_list, collapse = ","), "&placefips=", paste(fips_list,
        collapse = ","), "&countyfips=", county, "&stats=", paste(vars, collapse = ","), "&compressed=",
        totals)

    # Makes the API call and converts the JSON to a data frame
    data = jsonlite::read_json(call, simplifyVector = TRUE)
    if (totals == "yes") {
        data = mutate(data, totalpopulation = as.numeric(totalpopulation)) %>% rename(municipality = municipalityname)
    } else {
        data = mutate(data, totalpopulation = as.numeric(totalpopulation)) %>% rename(municipality = municipalityname)
    }
    # tells the function what to return and changes it from a dplyr tbl object back to a generic data
    # frame
    return(as.data.frame(data))

}
