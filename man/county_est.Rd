% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/county_est.r
\name{county_est}
\alias{county_est}
\title{API Wrapper to call Colorado county population estimates}
\usage{
county_est(fips_list, year_list, vars = "totalpopulation")
}
\arguments{
\item{fips_list}{Numeric FIPS code(s) for the county (0 for the state) (no leading 0's)}

\item{year_list}{Numeric list of years between 2010 and 2014}
}
\description{
This API wrapper returns a dataframe of county populations for the years
 and counties selected.
}
\details{
Note: Selecting all of the counties can be done by providing 300 as the fips_list
}

