% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangle.R
\name{yhrecode}
\alias{yhrecode}
\title{yhrecode uses dplyr::recode to recode levels and (i) returns numeric (if num_output =TRUE) (ii) set NA for unspecified levels}
\usage{
yhrecode(var, recode_rules, num_output = TRUE)
}
\arguments{
\item{num_output}{TRUE FALSE}
}
\description{
yhrecode uses dplyr::recode to recode levels and (i) returns numeric (if num_output =TRUE) (ii) set NA for unspecified levels
}
\examples{
\dontrun{
 yhrecode(c(1,2,3), c("2" = "1", "3"="0"))
 yhrecode will give NA to unspecified levels
 mtcars \%>\% mutate(fcyl = yhrecode(cyl, c("4" = "four", "6" = "six") ))
}

}
