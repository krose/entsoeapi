utils::globalVariables(
  c(
    "get_eiccodes",
    "type_def",
    "type",
    "eic_code_status",
    "eic_code_status_value",
    "doc_status"
  )
)



#' @title
#' instantiate a memory cache store for maximum 1 hour
#'
#' @importFrom cachem cache_mem
#'
#' @noRd
mh <- cachem::cache_mem(max_age = 3600)



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
#' eic_party <- entsoeapi::party_eic()
#'
#' str(eic_party)
#'
party_eic <- function() {
  # set the link of the csv file
  f <- "X_eicCodes.csv"

  # check if there is any cached value of 'party_eic_name'
  cache_key <- "party_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_area <- entsoeapi::area_eic()
#'
#' str(eic_area)
#'
area_eic <- function() {
  # set the link of the csv file
  f <- "Y_eicCodes.csv"

  # check if there is any cached value of 'area_eic_name'
  cache_key <- "area_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_accounting_point <- entsoeapi::accounting_point_eic()
#'
#' str(eic_accounting_point)
#'
accounting_point_eic <- function() {
  # set the link of the csv file
  f <- "Z_eicCodes.csv"

  # check if there is any cached value of 'accounting_point_eic_name'
  cache_key <- "accounting_point_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_tie_line <- entsoeapi::tie_line_eic()
#'
#' str(eic_tie_line)
#'
tie_line_eic <- function() {
  # set the link of the csv file
  f <- "T_eicCodes.csv"

  # check if there is any cached value of 'tie_line_eic_name'
  cache_key <- "tie_line_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_location <- entsoeapi::location_eic()
#'
#' str(eic_location)
#'
location_eic <- function() {
  # set the link of the csv file
  f <- "V_eicCodes.csv"

  # check if there is any cached value of 'location_eic_name'
  cache_key <- "location_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_resource_object <- entsoeapi::resource_object_eic()
#'
#' str(eic_resource_object)
#'
resource_object_eic <- function() {
  # set the link of the csv file
  f <- "W_eicCodes.csv"

  # check if there is any cached value of 'resource_object_eic_name'
  cache_key <- "resource_object_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_substation <- entsoeapi::substation_eic()
#'
#' str(eic_substation)
#'
substation_eic <- function() {
  # set the link of the csv file
  f <- "A_eicCodes.csv"

  # check if there is any cached value of 'substation_eic_name'
  cache_key <- "substation_eic_df_key"
  if (mh$exists(key = cache_key)) {

    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_eiccodes(f = f))
    message("\npulling ", f, " file from cache")

  } else {

    # download and import the csv file
    message("\ndownloading ", f, " file ...")
    res_df <- get_eiccodes(f = f)

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)

  }

  res_df
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
#' eic_all <- entsoeapi::all_approved_eic()
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
#' @returns
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
  cache_key <- "all_allocated_eic_df_key"

  # check if there is any cached value of 'all_allocated_eic'
  if (mh$exists(key = cache_key)) {
    # recall res_df values
    res_df <- mh$get(key = cache_key, missing = get_all_allocated_eic())
    message("\npulling all_allocated_eic table from cache")
  } else {
    # download and import the csv file
    message("\ndownloading all_allocated_eic table ...")
    res_df <- get_all_allocated_eic()

    # cache res_df as cache_key
    mh$set(key = cache_key, value = res_df)
  }

  res_df
}
