#' @title
#' Download and cache an EIC CSV file
#'
#' @param csv_file Character scalar. The CSV filename
#'   (e.g. `"X_eicCodes.csv"`).
#' @param cache_key Character scalar. The cache key
#'   (e.g. `"party_eic_df_key"`).
#'
#' @return A tibble extracted from the source csv.
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
#' @examplesIf there_is_provider()
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
#' @examplesIf there_is_provider()
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
#' @examplesIf there_is_provider()
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
#' @examplesIf there_is_provider()
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
#' @examplesIf there_is_provider()
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
#' @examplesIf there_is_provider()
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
#' @examplesIf there_is_provider()
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
#' @importFrom dplyr bind_rows
#'
#' @export
#'
#' @examplesIf there_is_provider()
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
    bind_rows() |>
    unique() |>
    as_tbl()
}


#' @title
#' Get all Allocated Energy Identification Codes
#'
#' @description
#' Beware, this is a REAL SLOW function, it runs for minutes, be patient!
#' This function downloads all allocated
#' energy identification codes from this link:
#' https://eepublicdownloads.blob.core.windows.net/
#' cio-lio/xml/allocated-eic-codes.xml
#' Further details are under:
#' https://www.entsoe.eu/data/energy-identification-codes-eic/
#' It is an alternative of `all_approved_eic()` function call
#' providing more details.
#'
#' @export
#'
#' @return
#' A tibble of all allocated EIC codes, which contains such columns as
#' `revision_number`, `created_date_time`, `eic_code`, `doc_status_value`,
#' `doc_status`, `instance_component_attribute`, `long_name`, `display_name`,
#' `last_request_date`, `deactivation_requested_date_and_or_time_date`,
#' `eic_code_market_participant_street_address`,
#' `market_participant_vat_code_name`, `market_participant_acer_code_name`,
#' `description`, `responsible_market_participant_mrid`, `function_names`
#'  and `parent_market_document_mrid`
#'
#' @examplesIf there_is_provider()
#' eic_all <- entsoeapi::all_allocated_eic()
#'
#' dplyr::glimpse(eic_all)
#'
all_allocated_eic <- function() {
  cache_get_or_compute( # nolint: object_usage_linter
    key = "all_allocated_eic_df_key",
    label = "all_allocated_eic table",
    compute_fn = get_all_allocated_eic
  )
}


#' @title
#' Display the ENTSO-E Transparency Platform news feed
#'
#' @description
#' Fetches the RSS news feed from the ENTSO-E Transparency Platform and
#' displays the entries in the console. Useful for checking platform
#' maintenance windows, data publication delays, and other announcements
#' that may affect API availability.
#'
#' @param feed_url the URL of the RSS news feed from the ENTSO-E
#'                 Transparency Platform.
#' @param n Integer scalar. Maximum number of feed items to display.
#'          Defaults to `5L`. Use `Inf` to show all items.
#'
#' @return A tibble of feed items with columns `title`, `pub_date`, and
#'   `description`, returned invisibly.
#'
#' @export
#'
#' @importFrom httr2 request req_method req_user_agent req_timeout req_retry
#'   resp_body_xml
#' @importFrom xml2 xml_find_all xml_find_first xml_text read_html
#' @importFrom cli cli_h1 cli_h2 cli_text cli_alert_info
#'
#' @examplesIf there_is_provider()
#' entsoeapi::get_news()
#'
get_news <- function(feed_url = .feed_url, n = 5L) {
  resp <- feed_url |>
    request() |>
    req_method(method = "GET") |>
    req_user_agent(string = user_agent_string) |>
    req_timeout(seconds = 30L) |>
    req_retry(
      max_tries = 3L,
      is_transient = \(resp) resp_status(resp) == 503L,
      backoff = \(x) 10
    ) |>
    req_perform()

  feed_xml <- resp_body_xml(resp = resp)
  items <- xml_find_all(x = feed_xml, xpath = "//item")

  n_show <- min(length(items), n)

  titles <- character(n_show)
  dates <- character(n_show)
  descriptions <- character(n_show)

  cli_h1("ENTSO-E Transparency Platform News")

  for (i in seq_len(n_show)) {
    titles[[i]] <- xml_find_first(items[[i]], "title") |>
      xml_text()
    dates[[i]] <- xml_find_first(items[[i]], "pubDate") |>
      xml_text()
    raw_desc <- xml_find_first(items[[i]], "description") |>
      xml_text()
    descriptions[[i]] <- tryCatch(
      expr = {
        paste0("<body>", raw_desc, "</body>") |>
          read_html() |>
          xml_text() |>
          trimws()
      },
      error = \(e) trimws(raw_desc)
    )

    cli_h2("{titles[[i]]}")
    cli_alert_info("{dates[[i]]}")
    cli_text("{descriptions[[i]]}")
  }

  result <- data.frame(
    title = titles,
    pub_date = dates,
    description = descriptions
  ) |>
    as_tbl()

  invisible(result)
}
