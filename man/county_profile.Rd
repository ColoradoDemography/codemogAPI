% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/county_profile.R
\name{county_profile}
\alias{county_profile}
\title{API Wrapper to call Colorado County Demographic Profile Variables}
\usage{
county_profile(fips_list, year_list, vars = "", group = "none")
}
\arguments{
\item{fips_list}{Numeric FIPS code(s) for the county (0 for the state) (no leading 0's)}

\item{year_list}{Numeric list of years between 2010 and 2014}

\item{vars}{list of variables (noted in description) for API to pull Defaults to All}
}
\description{
This API wrapper returns a dataframe of county-level demographic variables.
}
\details{
These variables include:births, deaths, naturalincrease, netmigration, censusbuildingpermits, groupquarterspopulation,
householdpopulation, households, householdsize, totalhousingunits, vacancyrate, vacanthousingunits

Note: Selecting all of the counties can be done by providing 300 as the fips_list
}

