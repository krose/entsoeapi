#' @title
#' Get Party_X Energy Identification Codes
#'
#' @description
#' This function downloads approved party X
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' It covers market participants.
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_party <- party_eic()
#'
#' str(eic_party)
#'
party_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/X_eiccodes.csv"
  )
}



#' @title
#' Get Area Y Energy Identification Codes
#'
#' @description
#' This function downloads approved area Y
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_area <- area_eic()
#'
#' str(eic_area)
#'
area_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Y_eiccodes.csv"
  )
}



#' @title
#' Get Accounting Point Z Energy Identification Codes
#'
#' @description
#' This function downloads approved accounting point Z
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' An entity under balance responsibility where balance supplier change
#' can take place and for which commercial business processes are defined.
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_accounting_point <- accounting_point_eic()
#'
#' str(eic_accounting_point)
#'
accounting_point_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/Z_eiccodes.csv"
  )
}



#' @title
#' Get Tie Line T Energy Identification Codes
#'
#' @description
#' This function downloads approved tie line T
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' It covers a transmission line that connects different areas
#' excluding HVDC interconnectors.
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_tie_line <- tie_line_eic()
#'
#' str(eic_tie_line)
#'
tie_line_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/T_eiccodes.csv"
  )
}



#' @title
#' Get Location V Energy Identification Codes
#'
#' @description
#' This function downloads approved location V
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' It covers an endpoint, or an IT-system.
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_location <- location_eic()
#'
#' str(eic_location)
#'
location_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/V_eiccodes.csv"
  )
}



#' @title
#' Get Resource Object W Energy Identification Codes
#'
#' @description
#' This function downloads approved resource object W
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' A resource that can either produce or consume energy
#' and that is reported in a schedule.
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_resource_object <- resource_object_eic()
#'
#' str(eic_resource_object)
#'
resource_object_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/W_eiccodes.csv"
  )
}



#' @title
#' Get Substation A Energy Identification Codes
#'
#' @description
#' This function downloads all approved substation A
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' Substation is a facility equipment that steps up or
#' steps down the voltage in utility power lines.
#' Voltage is stepped up where power is sent through
#' long distance transmission lines, and stepped down
#' where the power is to enter the local distribution lines.
#' They can be classified as normal outside substation,
#' armoured substation and underground substation.
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_substation <- substation_eic()
#'
#' str(eic_substation)
#'
substation_eic <- function() {
  get_eiccodes(
    f = "https://eepublicdownloads.entsoe.eu/eic-codes-csv/A_eiccodes.csv"
  )
}



#' @title
#' Get all Approved Energy Identification Codes
#'
#' @description
#' This function downloads approved all
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' Further details are under:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/#eic-documentation
#'
#' @returns
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examples
#'
#' library(entsoeapi)
#'
#' eic_all <- all_approved_eic()
#'
#' str(eic_all)
#'
all_approved_eic <- function() {
  list(party_eic(),
       area_eic(),
       accounting_point_eic(),
       tie_line_eic(),
       location_eic(),
       resource_object_eic(),
       substation_eic()) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
    unique() |>
    tibble::as_tibble()
}
