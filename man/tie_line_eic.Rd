% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/en_helpers.R
\name{tie_line_eic}
\alias{tie_line_eic}
\title{Get Tie Line T Energy Identification Codes}
\usage{
tie_line_eic()
}
\value{
A tibble of accordingly filtered EIC codes, which contains such columns as
\code{EicCode}, \code{EicDisplayName}, \code{EicLongName}, \code{EicParent},
\code{EicResponsibleParty}, \code{EicStatus}, \code{MarketParticipantPostalCode},
\code{MarketParticipantIsoCountryCode}, \code{MarketParticipantVatCode},
\code{EicTypeFunctionList} and \code{type}.
}
\description{
This function downloads approved tie line T
energy identification codes from this site:
https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
It covers a transmission line that connects different areas
excluding HVDC interconnectors.
}
\examples{
eic_tie_line <- entsoeapi::tie_line_eic()

str(eic_tie_line)

}
