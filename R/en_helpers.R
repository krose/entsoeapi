#' @title
#' Download and cache an EIC CSV file
#'
#' @param csv_file Character scalar. The CSV filename
#'   (e.g. `"X_eicCodes.csv"`).
#' @param cache_key Character scalar. The cache key
#'   (e.g. `"party_eic_df_key"`).
#'
#' @noRd
fetch_eic_csv <- function(csv_file, cache_key) {
  cache_get_or_compute( # nolint: object_usage_linter
    key = cache_key,
    label = paste(csv_file, "file"),
    compute_fn = \() get_eiccodes(f = csv_file)
  )
}


#' @title
#' Get Party_X Energy Identification Codes
#'
#' @description
#' This function downloads approved party X
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' It covers market participants.
#'
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_party <- entsoeapi::party_eic()
#'
#' dplyr::glimpse(eic_party)
#'
party_eic <- function() {
  fetch_eic_csv(
    csv_file = "X_eicCodes.csv",
    cache_key = "party_eic_df_key"
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
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_area <- entsoeapi::area_eic()
#'
#' dplyr::glimpse(eic_area)
#'
area_eic <- function() {
  fetch_eic_csv(
    csv_file = "Y_eicCodes.csv",
    cache_key = "area_eic_df_key"
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
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_accounting_point <- entsoeapi::accounting_point_eic()
#'
#' dplyr::glimpse(eic_accounting_point)
#'
accounting_point_eic <- function() {
  fetch_eic_csv(
    csv_file = "Z_eicCodes.csv",
    cache_key = "accounting_point_eic_df_key"
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
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_tie_line <- entsoeapi::tie_line_eic()
#'
#' dplyr::glimpse(eic_tie_line)
#'
tie_line_eic <- function() {
  fetch_eic_csv(
    csv_file = "T_eicCodes.csv",
    cache_key = "tie_line_eic_df_key"
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
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_location <- entsoeapi::location_eic()
#'
#' dplyr::glimpse(eic_location)
#'
location_eic <- function() {
  fetch_eic_csv(
    csv_file = "V_eicCodes.csv",
    cache_key = "location_eic_df_key"
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
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_resource_object <- entsoeapi::resource_object_eic()
#'
#' dplyr::glimpse(eic_resource_object)
#'
resource_object_eic <- function() {
  fetch_eic_csv(
    csv_file = "W_eicCodes.csv",
    cache_key = "resource_object_eic_df_key"
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
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_substation <- entsoeapi::substation_eic()
#'
#' dplyr::glimpse(eic_substation)
#'
substation_eic <- function() {
  fetch_eic_csv(
    csv_file = "A_eicCodes.csv",
    cache_key = "substation_eic_df_key"
  )
}


#' @title
#' Get all Approved Energy Identification Codes
#'
#' @description
#' This function downloads all approved
#' energy identification codes from this site:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/eic-approved-codes
#' Further details are under:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/#eic-documentation
#'
#' @return
#' A tibble of accordingly filtered EIC codes, which contains such columns as
#' `EicCode`, `EicDisplayName`, `EicLongName`, `EicParent`,
#' `EicResponsibleParty`, `EicStatus`, `MarketParticipantPostalCode`,
#' `MarketParticipantIsoCountryCode`, `MarketParticipantVatCode`,
#' `EicTypeFunctionList` and `type`.
#'
#' @export
#'
#' @examplesIf there_is_provider() && nchar(Sys.getenv("ENTSOE_PAT")) > 0L
#' eic_all <- entsoeapi::all_approved_eic()
#'
#' dplyr::glimpse(eic_all)
#'
all_approved_eic <- function() {
  list(
    party_eic(),
    area_eic(),
    accounting_point_eic(),
    tie_line_eic(),
    location_eic(),
    resource_object_eic(),
    substation_eic()
  ) |>
    dplyr::bind_rows() |>
    unique() |>
    tibble::as_tibble()
}


#' @title
#' Get all Allocated Energy Identification Codes
#'
#' @description
#' Beware, this is a REAL SLOW function, it runs for ages!
#' Be patient!!
#' This function downloads all allocated
#' energy identification codes from this link:
#' https://eepublicdownloads.blob.core.windows.net/
#' cio-lio/xml/allocated-eic-codes.xml
#' Further details are under:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/
#'
#' @return
#' A tibble of all allocated EIC codes, which contains such columns as
#' `doc_status`, `doc_status_value`, `revision_number`, `created_date_time`,
#' `eic_code`, `instance_component_attribute`, `long_name`, `display_name`,
#' `last_request_date`, `eic_code_deactivation_requested_date_and_or_time_date`,
#' `description`, `eic_code_market_participant_vat_code_name`,
#' `eic_code_market_participant_acer_code_name` and
#' `parent_market_document_mrid`
#'
#' @importFrom stats setNames
#'
#' @noRd
all_allocated_eic <- function() {
  cache_get_or_compute( # nolint: object_usage_linter
    key = "all_allocated_eic_df_key",
    label = "all_allocated_eic table",
    compute_fn = get_all_allocated_eic
  )
}
