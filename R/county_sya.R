#' API Wrapper to call Colorado County Single Year of Age Data
#'
#' This API wrapper returns a dataframe of county-level age data.
#'
#' The functions takes county and year and can return single year of age data and grouped data.
#'
#' Grouping options are as follows:
#'    opt1= Group by year
#'    opt2= Group by county and year
#'    opt3= Group by age and year
#'
#' Data are estimates or forecasts depending on the most recent vintage, they are noted as such in the returned data.
#'
#' @param fips_list Numeric FIPS code(s) for the county (0 for the state) (no leading 0's)
#' @param year_list Numeric list of years between 1990 and 2050
#' @param group string taking values of opt1, opt2, opt2, see descriptiong what what each does
#' @importFrom jsonlite fromJSON
#' @import tidyr
#' @import dplyr
#'
#' @export
