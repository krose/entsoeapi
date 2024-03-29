% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/en_outages_generation_units.R
\name{en_outages_tidy_to_ts}
\alias{en_outages_tidy_to_ts}
\title{Convert the outages to an hourly timeseries.}
\usage{
en_outages_tidy_to_ts(out_df)
}
\arguments{
\item{out_df}{Tidy data.frame of outages.}
}
\description{
The function removes tidy observations less than 59 minutes.
}
