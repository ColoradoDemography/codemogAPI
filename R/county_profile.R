#' API Wrapper to call Colorado County Demographic Profile Variables
#'
#' This API wrapper returns a dataframe of county-level demographic variables.
#'
#' These variables include:births, deaths, naturalincrease, netmigration, censusbuildingpermits, groupquarterspopulation,
#' householdpopulation, households, householdsize, totalhousingunits, vacancyrate, vacanthousingunits
#'
#'  Note: Selecting all of the counties can be done by providing 300 as the fips_list
#'
#' @param fips_list Numeric FIPS code(s) for the county (0 for the state) (no leading 0's)
#' @param year_list Numeric list of years between 2010 and 2014
#' @param vars list of variables (noted in description) for API to pull Defaults to All
#' @importFrom jsonlite fromJSON
#' @import tidyr
#' @import dplyr
#'
#' @export


county_profile=function(fips_list, year_list, vars=""){
  # subject to change, but this is the server URL
  url_stub="http://red-meteor-147235.nitrousapp.com:4001/profile?"

  # Type checks for each list so that we don't get any text sent to the API for a SQL Injection.
  if (!is.numeric(fips_list))
    stop("FIPS should be numeric.")
  if (!is.numeric(year_list))
    stop("Years should be numeric.")

  if(any(year_list<1985) || any(year_list>2014))
    stop("One or more year is out of range. Years should be between 1985 and 2014")

  # Checks for two special call types 300 is for all counties, 0 is for state total, puts proper list together for call
  suppressWarnings(if (fips_list==300 | fips_list ==0) {
    fips=c(1,3,5,7,9,11,13,14,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,
           51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,
           99,101,103,105,107,109,111,113,115,117,119,121,123,125)
  } else {

    fips=fips_list
  }
)
  # Creates the URL for the API call
  call=paste0(url_stub,
              "&county=", paste(fips, collapse=","),
              "&year=", paste(year_list, collapse=","),
              "&vars=", paste(vars, collapse = ","))

  # Makes the API call and converts the JSON to a data frame
  data=county_names%>%
    inner_join(jsonlite::fromJSON(call, simplifyVector = TRUE), by="countyfips")

  # Checks if the State Total call (0) is used and if so creates the total, otherwise fixes the type and naming for totalPopulation
  suppressWarnings(if(fips_list==0){
    data=data%>%
      select(-vacancyrate, -householdsize)%>%
      tidyr::gather(variable, value, -countyfips:-year)%>%
      group_by(year, variable)%>%
      summarize(value=sum(as.numeric(value)))%>%
      tidyr::spread(variable, value)%>%
      mutate(householdsize=round(householdpopulation/households, 2),
             vacancyrate=round((vacanthousingunits/totalhousingunits)*100,2))
    data=bind_cols(data.frame(countyfips=0), data)
  } else {
    data=data
  }
)
  # tells the function what to return and changes it from a dplyr tbl object back to a generic data frame
  return(as.data.frame(data))
}
