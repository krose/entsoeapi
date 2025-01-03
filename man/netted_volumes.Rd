% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/en_balancing.R
\name{netted_volumes}
\alias{netted_volumes}
\title{Get Netted Volumes (IFs IN 3.10)}
\usage{
netted_volumes(
  eic = NULL,
  period_start = lubridate::ymd(Sys.Date() - lubridate::days(x = 7L), tz = "CET"),
  period_end = lubridate::ymd(Sys.Date(), tz = "CET"),
  tidy_output = TRUE,
  security_token = Sys.getenv("ENTSOE_PAT")
)
}
\arguments{
\item{eic}{Energy Identification Code of the area}

\item{period_start}{POSIXct or YYYY-MM-DD HH:MM:SS format
One day range limit applies}

\item{period_end}{POSIXct or YYYY-MM-DD HH:MM:SS format
One day range limit applies}

\item{tidy_output}{Defaults to TRUE. flatten nested tables}

\item{security_token}{Security token for ENTSO-E transparency platform}
}
\description{
The net position informs whether the given area imports or exports energy.
Those rows which hold the queried eic value in the
'ts_connecting_domain_mrid' column
show the export value.
Those rows which hold the queried eic value in the
'ts_acquiring_domain_mrid' column
show the import value.
}
\examples{
df <- entsoeapi::netted_volumes(
  eic          = "10YCZ-CEPS-----N",
  period_start = lubridate::ymd(x = "2022-08-16", tz = "CET"),
  period_end   = lubridate::ymd(x = "2022-08-17", tz = "CET"),
  tidy_output  = TRUE
)
str(df)

}
