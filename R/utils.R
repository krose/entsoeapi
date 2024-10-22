#' @title
#' instantiate a memory cache store for maximum 1 hour
#'
#' @importFrom cachem cache_mem
#'
#' @noRd
m <- cachem::cache_mem(max_age = 3600)



utils::globalVariables(
  c(
    "asset_types",
    "auction_types",
    "business_types",
    "category_types",
    "document_types",
    "direction_types",
    "object_aggregation_types",
    "process_types",
    "reason_code_types",
    "area_eic",
    "document_types",
    "category_types",
    "reason_code_types",
    "object_aggregation_types",
    "m"
  )
)



#' @title
#' calculate offset URLs
#'
#' @noRd
calc_offset_urls <- function(reason, query_string) {
  # extract the number of the allowed documents
  docs_allowed <- stringr::str_extract(
    string = reason,
    pattern = "allowed: [0-9]{1,8}"
  ) |>
    stringr::str_extract(pattern = "[0-9]{1,8}") |>
    as.integer()

  # extract the number of the requested documents
  docs_requested <- stringr::str_extract(
    string = reason,
    pattern = "requested: [0-9]{1,8}"
  ) |>
    stringr::str_extract(pattern = "[0-9]{1,8}") |>
    as.integer()

  # calculate how many offset round is needed
  all_offset_nr <- docs_requested %/% docs_allowed +
    ceiling(docs_requested %% docs_allowed / docs_allowed)
  all_offset_seq <- (seq(all_offset_nr) - 1L) * docs_allowed

  # recompose offset URLs
  message("*** The request has been rephrased. ***")
  return(paste0(query_string, "&offset=", all_offset_seq))
}



#' @title
#' read XML content from a zip compressed file
#'
#' @noRd
read_zipped_xml <- function(temp_file_path) {
  # safely decompress zip file into several files on disk
  unzip_safe <- purrr::safely(utils::unzip)
  unzipped_files <- unzip_safe(
    zipfile = temp_file_path,
    overwrite = TRUE,
    exdir = fs::path_dir(temp_file_path)
  )

  # read the xml content from each the decompressed files
  en_cont_list <- unzipped_files$result |>
    purrr::map(~{
      xml_content <- xml2::read_xml(.x)
      message(.x, " has read in")
      return(xml_content)
    })

  # return with the xml content list
  return(en_cont_list)

}



#' @title
#' call request against the ENTSO-E API and converts the response into xml
#'
#' @noRd
api_req <- function(
  api_scheme = "https://",
  api_domain = "web-api.tp.entsoe.eu/",
  api_name = "api?",
  query_string = NULL,
  security_token = NULL
) {
  if (is.null(query_string)) {
    stop("The argument 'query_string' is missing!")
  }
  if (is.null(security_token)) {
    stop("The argument 'security_token' is not provided!")
  } else {
    # add the canonical API prefix and suffix to the request url
    url <- paste0(
      api_scheme, api_domain, api_name, query_string, "&securityToken="
    )
    message(url, "<...>")
  }

  resp <- httr::GET(
    url = paste0(url, security_token),
    httr::content_type_xml(),
    httr::write_memory()
  )

  if (is.integer(httr::status_code(resp))) message("response has arrived")

  # if the get request is successful, then ...
  if (httr::status_code(resp) == "200") {

    # if the request is a zip file, then ...
    rhct <- resp$headers$`content-type`
    if (rhct == "application/zip") {

      # redownload again, but into disk this time
      temp_file_path <- tempfile(fileext = ".zip")
      resp <- httr::GET(
        url = paste0(url, security_token),
        httr::content_type_xml(),
        httr::write_disk(path = temp_file_path, overwrite = TRUE)
      )

      # read the xml content from each the decompressed files
      en_cont_list <- read_zipped_xml(temp_file_path)

      # return with the xml content list
      return(en_cont_list)

    } else if (rhct %in% c("text/xml", "application/xml")) {

      # read the xml content from the response
      en_cont <- httr::content(resp, encoding = "UTF-8")

      # return with the xml content
      return(en_cont)

    } else {

      stop("Not known response content-type: ", resp$headers$`content-type`)

    }

  } else {

    # extract reason from reason text
    response_reason <- resp |>
      httr::content(encoding = "utf-8") |>
      xml2::as_list() |>
      purrr::pluck("Acknowledgement_MarketDocument", "Reason", "text") |>
      unlist()
    if (lengths(response_reason) > 0) {
      message("*** ", response_reason, " ***")
    }

    # check if offset usage needed or not
    offset_needed <- stringr::str_detect(
      string = response_reason,
      pattern = "The amount of requested data exceeds allowed limit"
    )

    # check if query offset is allowed
    offset_allowed <- stringr::str_detect(
      string = query_string,
      pattern = "documentType=A63&businessType=A85",
      negate = TRUE
    )

    # if offset usage needed, then ...
    if (isTRUE(offset_needed) && isTRUE(offset_allowed)) {

      # calculate offset URLs
      offset_query_strings <- calc_offset_urls(
        reason = response_reason,
        query_string = query_string
      )

      # recursively call the api_req() function itself
      en_cont_list <- offset_query_strings |>
        purrr::map(
          ~api_req(
            query_string = .x,
            security_token = security_token
          )
        ) |>
        unlist(recursive = FALSE)

      return(en_cont_list)

    } else {

      stop(httr::content(resp, encoding = "UTF-8"))

    }

  }
}



#' @title
#' safely call api_req() function
#'
#' @noRd
api_req_safe <- purrr::safely(api_req)



#' @title
#' converts the given POSIXct or character timestamp into the acceptable format
#'
#' @noRd
url_posixct_format <- function(x) {
  if (is.null(x)) {
    y <- NULL
  } else if (inherits(x = x, what = "POSIXct")) {
    y <- strftime(x = x, format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
  } else if (inherits(x = x, what = "character")) {
    y <- lubridate::parse_date_time(x      = x,
                                    orders = c("%Y-%m-%d %H:%M:%S",
                                               "%Y-%m-%d %H:%M",
                                               "%Y-%m-%d",
                                               "%Y.%m.%d %H:%M:%S",
                                               "%Y.%m.%d %H:%M",
                                               "%Y.%m.%d",
                                               "%Y%m%d%H%M%S",
                                               "%Y%m%d%H%M",
                                               "%Y%m%d"),
                                    tz     = "UTC",
                                    quiet  = TRUE) |>
      strftime(format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
    if (is.na(y)) {
      stop("Only the class POSIXct or '%Y-%m-%d %H:%M:%S' formatted text ",
           "are supported by the converter.")
    } else {
      warning("The ", x, " value has interpreted as UTC!", call. = FALSE)
    }
  } else {
    stop("The argument 'x' is not in an acceptable timestamp format!")
  }

  return(y)
}



#' @title
#' create a time series table
#'
#' @description
#' between "from" till "to" with "seq_resolution" frequency
#' using "pos" position and "qty" quantity values
#'
#' @noRd
dt_seq_helper <- function(from, to, seq_resolution = "PT60M", pos, qty) {
  # define those variables as NULL which are used under non-standard evaluation
  qty <- qty

  # calculate "by" value from "seq_resolution" value
  by <- tryCatch(
    expr = data.table::fcase(
      identical(x = seq_resolution, y = "PT1M"), "1 min",
      identical(x = seq_resolution, y = "PT15M"), "15 mins",
      identical(x = seq_resolution, y = "PT30M"), "30 mins",
      identical(x = seq_resolution, y = "PT60M"), "1 hour",
      identical(x = seq_resolution, y = "P1D"), "1 DSTday",
      identical(x = seq_resolution, y = "P7D"), "7 DSTdays",
      identical(x = seq_resolution, y = "P1M"), "1 month",
      identical(x = seq_resolution, y = "P1Y"), "1 year",
      default = "n/a"
    ),
    error = function(e) stop(e)
  )

  # check if we got a valid resolution
  if (by == "n/a") {

    stop("The 'resolution' value of the response is not supported yet.",
         "\nPlease use 'PT1M', 'PT15M', 'PT30M', 'PT60M', 'P1D', ",
         "'P7D', 'P1M' or 'P1Y'.")

  } else if (by %in% c("1 DSTday", "7 DSTdays", "1 month", "1 year")) {

    # create a datetime vector from "from" (incl.) to "to" (excl.) by "by"
    tzone <- data.table::fcase(
      format(x = from, format = "%H", tz = "UTC") == "00", "UTC",
      format(x = from, format = "%H", tz = "WET") == "00", "WET",
      format(x = from, format = "%H", tz = "CET") == "00", "CET",
      format(x = from, format = "%H", tz = "EET") == "00", "EET",
      format(x = from, format = "%H", tz = "W-SU") == "00", "W-SU",
      default = "not_known"
    )
    if (tzone == "not_known") {
      stop("The from date should denote the midnight hour either ",
           "in 'UTC', 'WET', 'CET', 'EET' or Europe/Moscow timezone!")
    }
    dts <- seq(
      from = lubridate::with_tz(time = from, tzone = tzone),
      length.out = length(pos),
      by = by
    ) |>
      lubridate::with_tz(tzone = "UTC")

  } else {

    # create a datetime vector from "from" (incl.) to "to" (excl.) by "by"
    dts <- seq(
      from = from,
      length.out = length(pos),
      by = by
    )
    # round down the datetime vector elements to "by" unit
    dts <- dts |>
      lubridate::floor_date(unit = by)

  }

  # compose a tibble from the expanded periods
  # and the provided quantity using starting positions
  dt_tbl <- merge(x = tibble::tibble(start_dt = dts),
                  y = tibble::tibble(start_dt = dts[pos],
                                     qty),
                  by = "start_dt",
                  all.x = TRUE)
  if ("qty" %in% names(dt_tbl)) {
    dt_tbl$qty <- data.table::nafill(
      x = dt_tbl[["qty"]],
      type = "locf"
    )
  } else {
    dt_tbl$qty <- NA_real_
  }

  return(tibble::as_tibble(dt_tbl))
}



#' @title
#' downloads approved Energy Identification Codes
#'
#' @description
#' from ENTSO-E Transparency Platform under link "f"
#'
#' @noRd
get_eiccodes <- function(f) {
  # reading input file into a character vector
  # and replacing erroneous semicolons to commas
  # unfortunately there is no general rule for that,
  # hence it must be set manually!!
  readlines_quiet <- purrr::quietly(readLines)
  content <- readlines_quiet(con = f, encoding = "UTF-8")
  lns <- content$result |>
    stringr::str_replace_all(pattern     = "tutkimustehdas;\\sImatra",
                             replacement = "tutkimustehdas, Imatra") |>
    stringr::str_replace_all(pattern     = "; S\\.L\\.;",
                             replacement = ", S.L.;") |>
    stringr::str_replace_all(pattern     = "\\$amp;",
                             replacement = "&")

  # reading lines as they would be a csv
  eiccodes <- data.table::fread(
    text       = lns,
    sep        = ";",
    na.strings = c("", "n / a", "n/a", "N/A", "-", "-------", "."),
    encoding   = "UTF-8"
  )

  # trimming character columns
  eiccodes <- eiccodes |>
    purrr::map(~{
      if (is.character(.x)) {
        trimws(x = .x, which = "both")
      } else {
        .x
      }
    }) |>
    tibble::as_tibble()

  return(eiccodes)
}



#' @title
#' unpack an xml section into a tabular row
#'
#' @noRd
unpack_xml <- function(section, parent_name = NULL) {
  result_vector <- xml2::as_list(section) |>
    unlist(recursive = TRUE)
  if (is.null(result_vector)) {
    tbl <- tibble::tibble()
    return(tbl)
  } else {
    names(result_vector) <- stringr::str_c(parent_name,
                                           xml2::xml_name(section),
                                           names(result_vector),
                                           sep = ".")
    tbl <- tibble::as_tibble_row(result_vector)
    return(tbl)
  }
}



#' @title
#' an own version of snakecase::to_snakecase() function
#'
#' @description
#' read and convert the column names of the provided data frame
#' into the required snakecase format
#'
#' @noRd
my_snakecase <- function(tbl) {
  if (isFALSE(is.data.frame(tbl))) {
    stop("The provided argument is not a valid data frame!")
  }
  names(tbl) |>
    stringr::str_replace_all(
      pattern = "mRID",
      replacement = "mrid"
    ) |>
    stringr::str_replace_all(
      pattern = "TimeSeries",
      replacement = "ts"
    ) |>
    stringr::str_remove(pattern = "^process") |>
    stringr::str_replace_all(
      pattern = "unavailability_Time_Period",
      replacement = "unavailability"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[p|P]roduction_RegisteredResource.pSRType",
      replacement = "ts.production"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[p|P]roduction_RegisteredResource",
      replacement = "ts.production"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[a|A]sset_RegisteredResource.pSRType",
      replacement = "ts.asset"
    ) |>
    stringr::str_replace_all(
      pattern = "ts.[a|A]sset_RegisteredResource",
      replacement = "ts.asset"
    ) |>
    stringr::str_replace_all(
      pattern = "[p|P]owerSystemResources",
      replacement = "psr"
    ) |>
    snakecase::to_snake_case() |>
    stringr::str_replace_all(
      pattern = "psr_type_psr_type",
      replacement = "psr_type"
    ) |>
    stringr::str_replace_all(
      pattern = "asset_psr_type",
      replacement = "psr_type"
    ) |>
    stringr::str_replace_all(
      pattern = "_direction_direction",
      replacement = "_direction"
    ) |>
    stringr::str_remove(
      pattern = "ts_mkt_psr_type_voltage_psr_"
    )
}


#' @title
#' detect the number of grand children per each child
#'
#' @noRd
xml_grand_children_lengths <- function(xml_content) {
  xml2::xml_children(xml_content) |> xml2::xml_length()
}



#' @title
#' create a specific merge function which adds the needed definitions
#'
#' @noRd
def_merge <- function(x, y, code_name, definition_name) {
  x <- x |>
    data.table::data.table()
  y <- y |>
    subset(select = c("CODE", "DEFINITION")) |>
    data.table::data.table()
  names(y) <- c(code_name, definition_name)
  data.table::merge.data.table(
    x = x,
    y = y,
    by = code_name,
    suffixes = c("_x", "_y"),
    all.x = TRUE
  )
}



#' @title
#' create a specific merge function which adds the EIC names
#'
#' @noRd
eic_name_merge <- function(x, y, eic_code_name, eic_name_name) {
  y <- y |>
    subset(select = c("eic_code", "eic_name")) |>
    data.table::data.table()
  names(y) <- c(eic_code_name, eic_name_name)
  data.table::merge.data.table(
    x = x,
    y = y,
    by = eic_code_name,
    all.x = TRUE
  )
}



#' @title
#' add type names to codes
#'
#' @noRd
add_type_names <- function(tbl) {
  # pre-define some built-in tables to avoid non-standard evaluation issues
  # within the current function
  asset_types <- asset_types
  auction_types <- auction_types
  business_types <- business_types
  category_types <- category_types
  contract_types <- contract_types
  document_types <- document_types
  object_aggregation_types <- object_aggregation_types
  process_types <- process_types
  reason_code_types <- reason_code_types

  # convert tbl to data.table in order to join faster
  tbl <- data.table::data.table(tbl)

  # define an empty vector to collect those column names
  # which will get definitions by add_type_names() function
  affected_cols <- c()

  # add type names to codes
  if ("type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "type")
    tbl <- def_merge(
      x = tbl,
      y = document_types,
      code_name = "type",
      definition_name = "type_def"
    )
  }
  if ("ts_business_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_business_type")
    tbl <- def_merge(
      x = tbl,
      y = business_types,
      code_name = "ts_business_type",
      definition_name = "ts_business_type_def"
    )
  }
  if ("ts_mkt_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_mkt_psr_type")
    tbl <- def_merge(
      x = tbl,
      y = asset_types,
      code_name = "ts_mkt_psr_type",
      definition_name = "ts_mkt_psr_type_def"
    )
  }
  if ("ts_asset_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_asset_psr_type")
    tbl <- def_merge(
      x = tbl,
      y = asset_types,
      code_name = "ts_asset_psr_type",
      definition_name = "ts_asset_psr_type_def"
    )
  }
  if ("ts_production_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_production_psr_type")
    tbl <- def_merge(
      x = tbl,
      y = asset_types,
      code_name = "ts_production_psr_type",
      definition_name = "ts_production_psr_type_def"
    )
  }
  if ("process_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "process_type")
    tbl <- def_merge(
      x = tbl,
      y = process_types,
      code_name = "process_type",
      definition_name = "process_type_def"
    )
  }
  if ("ts_contract_market_agreement_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_contract_market_agreement_type")
    tbl <- def_merge(
      x = tbl,
      y = contract_types,
      code_name = "ts_contract_market_agreement_type",
      definition_name = "ts_contract_market_agreement_type_def"
    )
  }
  if ("ts_auction_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_auction_type")
    tbl <- def_merge(
      x = tbl,
      y = auction_types,
      code_name = "ts_auction_type",
      definition_name = "ts_auction_type_def"
    )
  }
  if (length(affected_cols) == 0L) {
    warning("No additional type names added!")
  }

  return(tbl)
}



#' @title
#' add names to EIC codes
#'
#' @noRd
add_eic_names <- function(tbl) {
  # define those variables as NULL which are used under non-standard evaluation
  eic_code <- eic_long_name <- eic_name <- NULL

  # convert tbl to data.table in order to join faster
  tbl <- data.table::data.table(tbl)

  # check if there is any cached value of 'area_eic_name'
  aen_cache_key <- "area_eic_name_key"
  if (m$exists(key = aen_cache_key)) {

    # recall area_eic_name values
    area_eic_name <- m$get(aen_cache_key)

  } else {
    # download & convert area_eic() table to data.table
    # in order to join faster
    area_eic_name <- area_eic() |>
      subset(select = c("EicCode", "EicLongName")) |>
      dplyr::rename_with(snakecase::to_snake_case) |>
      dplyr::group_by(eic_code) |>
      dplyr::mutate(eic_name = stringr::str_c(
        eic_long_name,
        collapse = " - "
      )) |>
      dplyr::ungroup() |>
      dplyr::select(eic_code, eic_name) |>
      data.table::data.table()

    # cache aen_dt as aen_cache_key
    m$set(aen_cache_key, area_eic_name)
  }

  # define an empty vector to collect those EIC column names
  # which will get definitions by add_eic_names() function
  affected_cols <- c()

  # add names to eic codes
  if ("ts_registered_resource_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_registered_resource_mrid")

    # check if there is any cached value of 'area_eic_name'
    roe_cache_key <- "resource_object_eic_name_key"
    if (m$exists(key = roe_cache_key)) {

      # recall resource_object_eic_name values
      resource_object_eic <- m$get(roe_cache_key)

    } else {

      # download & convert resource_object_eic() table to data.table
      # in order to join faster
      resource_object_eic <- resource_object_eic() |>
        subset(select = c("EicCode", "EicLongName")) |>
        dplyr::rename_with(snakecase::to_snake_case) |>
        dplyr::rename(ts_registered_resource_mrid = eic_code,
                      ts_registered_resource_name = eic_long_name) |>
        data.table::data.table()

      # cache roe_dt as cache_key
      m$set(roe_cache_key, resource_object_eic)

    }

    tbl <- tbl |>
      dplyr::select_if(!names(tbl) %in% c("ts_registered_resource_name"))
    tbl <- tbl |>
      merge(y = resource_object_eic,
            by = "ts_registered_resource_mrid",
            all.x = TRUE)
  }
  if ("ts_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_bidding_zone_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_bidding_zone_domain_mrid",
        eic_name_name = "ts_bidding_zone_domain_name"
      )
  }
  if ("ts_in_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_in_bidding_zone_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_in_bidding_zone_domain_mrid",
        eic_name_name = "ts_in_bidding_zone_domain_name"
      )
  }
  if ("ts_out_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_out_bidding_zone_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_out_bidding_zone_domain_mrid",
        eic_name_name = "ts_out_bidding_zone_domain_name"
      )
  }
  if ("ts_in_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_in_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_in_domain_mrid",
        eic_name_name = "ts_in_domain_name"
      )
  }
  if ("ts_out_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_out_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "ts_out_domain_mrid",
        eic_name_name = "ts_out_domain_name"
      )
  }
  if ("control_area_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "control_area_domain_mrid")
    tbl <- tbl |>
      eic_name_merge(
        y = area_eic_name,
        eic_code_name = "control_area_domain_mrid",
        eic_name_name = "control_area_domain_name"
      )
  }
  if (length(affected_cols) == 0L) {
    warning("No additional eic names added!")
  }

  return(tbl)
}



#' @title
#' add definitions to codes
#'
#' @noRd
add_definitions <- function(tbl) {
  # define those variables as NULL which are used under non-standard evaluation
  reason_text_x <- reason_text_y <- NULL
  ts_reason_text_x <- ts_reason_text_y <- NULL

  # convert tbl to data.table in order to join faster
  tbl <- data.table::data.table(tbl)

  # define an empty vector to collect those column names
  # which will get definitions by add_definitions() function
  affected_cols <- c()

  # add definitions to codes
  if ("doc_status_value" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "doc_status_value")
    tbl <- def_merge(
      x = tbl,
      y = document_types,
      code_name = "doc_status_value",
      definition_name = "doc_status"
    )
  }
  if ("ts_auction_category" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_auction_category")
    tbl <- def_merge(
      x = tbl,
      y = category_types,
      code_name = "ts_auction_category",
      definition_name = "ts_auction_category_def"
    )
  }
  if ("ts_flow_direction" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_flow_direction")
    tbl <- def_merge(
      x = tbl,
      y = direction_types,
      code_name = "ts_flow_direction",
      definition_name = "ts_flow_direction_def"
    )
  }
  if ("reason_code" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "reason_code")
    tbl <- def_merge(
      x = tbl,
      y = reason_code_types,
      code_name = "reason_code",
      definition_name = "reason_text"
    )
    if ("reason_text_x" %in% names(tbl) &&
          "reason_text_y" %in% names(tbl)) {
      tbl <- tbl |>
        dplyr::mutate(
          reason_text = paste(
            reason_text_y,
            reason_text_x,
            sep = " - "
          ) |>
            stringr::str_remove(pattern = "^NA - | - NA$"),
          reason_text_x = NULL,
          reason_text_y = NULL
        )
    }
  }
  if ("ts_reason_code" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_reason_code")
    tbl <- def_merge(
      x = tbl,
      y = reason_code_types,
      code_name = "ts_reason_code",
      definition_name = "ts_reason_text"
    )
    if ("ts_reason_text_x" %in% names(tbl) &&
          "ts_reason_text_y" %in% names(tbl)) {
      tbl <- tbl |>
        dplyr::mutate(
          ts_reason_text = paste(
            ts_reason_text_y,
            ts_reason_text_x,
            sep = " - "
          ) |>
            stringr::str_remove(pattern = "^NA - | - NA$"),
          ts_reason_text_x = NULL,
          ts_reason_text_y = NULL
        )
    }
  }
  if ("ts_object_aggregation" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_object_aggregation")
    tbl <- def_merge(
      x = tbl,
      y = object_aggregation_types,
      code_name = "ts_object_aggregation",
      definition_name = "ts_object_aggregation_def"
    )
  }
  if (length(affected_cols) == 0L) {
    warning("No additional definitions added!")
  }

  return(tbl)
}



#' @title
#' process the ridge of the xml_content
#'
#' @noRd
process_ridge <- function(xml_content) {
  # pick those children which have less then 3 grand child(ren)
  ridge_ind <- which(xml_grand_children_lengths(xml_content) <= 2)
  ridge_children <- xml2::xml_children(xml_content)[ridge_ind]

  # unpack each ridge child and bind them as columns
  ridge_tbl <- ridge_children |>
    purrr::map(unpack_xml, parent_name = NULL) |>
    dplyr::bind_cols()

  # drop the not necessary sender/receiver market participant columns
  ridge_tbl <- ridge_tbl |>
    dplyr::select(!dplyr::starts_with(match = "sender_MarketParticipant")) |>
    dplyr::select(!dplyr::starts_with(match = "receiver_MarketParticipant"))

  # convert datetime-like columns to POSIXct and numeric-like columns to numeric
  ridge_tbl <- ridge_tbl |>
    dplyr::mutate(
      dplyr::across(tidyselect::matches("[t|T]ime$|start$|end$"),
                    ~as.POSIXct(x = .x,
                                tryFormats = c("%Y-%m-%dT%H:%MZ",
                                               "%Y-%m-%dT%H:%M:%SZ"),
                                tz = "UTC"))
    ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::matches("number$|position$|quantity$|ts_mrid"),
                    ~as.numeric(x = .x))
    )

  # return with the ridge table
  ridge_tbl
}



#' @title
#' process the detailed data of the xml_content
#'
#' @noRd
process_detailed <- function(xml_content, tidy_output) {
  # define those variables as NULL which are used under non-standard evaluation
  start_dt <- qty <- NULL

  # pick those children which have more than 2 grand children
  det_ind <- which(xml_grand_children_lengths(xml_content) > 2)
  det_children <- xml2::xml_children(xml_content)[det_ind]

  # unpack each detailed child and bind them as rows
  det_tbl <- det_children |>
    purrr::map(\(det_child) {
      # extract the name of the det_child xml element
      det_child_name <- xml2::xml_name(det_child)

      # extract the grand children elements of current child element
      det_grand_children <- xml2::xml_children(det_child)

      # divide det_grand_children into 2 parts
      # based on their name endings
      det_grand_children_names <- xml2::xml_name(det_grand_children)
      ps_ind <- which(
        endsWith(
          x = det_grand_children_names,
          suffix = "Period"
        )
      )
      nps_ind <- which(
        !endsWith(
          x = det_grand_children_names,
          suffix = "Period"
        )
      )
      # period-like grand children
      det_grand_children_ps <- det_grand_children[ps_ind]
      # non-period-like grand children
      det_grand_children_nps <- det_grand_children[nps_ind]

      # iterate over the detailed grand children and bind them as columns
      res_dt_ps <- det_grand_children_ps |>
        purrr::map(\(det_grand_child_ps) {

          # extract grand-grand-children elements
          det_grand_grand_children_ps <- xml2::xml_children(
            det_grand_child_ps
          )

          # detect which child NOT contains data points
          not_point_ind <- which(
            xml2::xml_name(det_grand_grand_children_ps) != "Point"
          )
          np_tbl <- det_grand_grand_children_ps[not_point_ind] |>
            purrr::map(unpack_xml, parent_name = det_child_name) |>
            dplyr::bind_cols()

          # convert the datetime-like columns to POSIXct
          np_tbl <- np_tbl |>
            dplyr::mutate(
              dplyr::across(tidyselect::matches("Time$|start$|end$"),
                            ~as.POSIXct(x = .x,
                                        tryFormats = c("%Y-%m-%dT%H:%MZ",
                                                       "%Y-%m-%dT%H:%M:%SZ"),
                                        tz = "UTC"))
            )

          # detect which child contains data points
          point_ind <- which(
            xml2::xml_name(det_grand_grand_children_ps) == "Point"
          )

          # unpack each datapoint xml each by each and append them
          p_tbl <- det_grand_grand_children_ps[point_ind] |>
            purrr::map(unpack_xml, parent_name = det_child_name) |>
            dplyr::bind_rows()

          # convert all data point columns to numeric
          p_tbl <- p_tbl |>
            dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

          # if tidy output is needed, then
          if (tidy_output == TRUE) {
            if ("TimeSeries.Point.price.amount" %in% names(p_tbl)) {
              p_tbl <- dt_seq_helper(
                from = np_tbl$TimeSeries.timeInterval.start,
                to = np_tbl$TimeSeries.timeInterval.end,
                seq_resolution = np_tbl$TimeSeries.resolution,
                pos = p_tbl$TimeSeries.Point.position,
                qty = p_tbl$TimeSeries.Point.price.amount
              ) |>
                dplyr::rename(
                  ts_point_dt_start = start_dt,
                  ts_point_price = qty
                )
            } else if (
              "TimeSeries.Point.congestionCost_Price.amount" %in% names(p_tbl)
            ) {
              p_tbl <- dt_seq_helper(
                from = np_tbl$TimeSeries.timeInterval.start,
                to = np_tbl$TimeSeries.timeInterval.end,
                seq_resolution = np_tbl$TimeSeries.resolution,
                pos = p_tbl$TimeSeries.Point.position,
                qty = p_tbl$TimeSeries.Point.congestionCost_Price.amount
              ) |>
                dplyr::rename(ts_point_dt_start = start_dt,
                              ts_point_congestion_cost = qty)
            } else if ("TimeSeries.Point.quantity" %in% names(p_tbl)) {
              p_tbl <- dt_seq_helper(
                from = np_tbl$TimeSeries.timeInterval.start,
                to = np_tbl$TimeSeries.timeInterval.end,
                seq_resolution = np_tbl$TimeSeries.resolution,
                pos = p_tbl$TimeSeries.Point.position,
                qty = p_tbl$TimeSeries.Point.quantity
              ) |>
                dplyr::rename(ts_point_dt_start = start_dt,
                              ts_point_quantity = qty)
            } else if (names(p_tbl) == c("TimeSeries.Point.position")) {
              p_tbl <- dt_seq_helper(
                from = np_tbl$TimeSeries.timeInterval.start,
                to = np_tbl$TimeSeries.timeInterval.end,
                seq_resolution = np_tbl$TimeSeries.resolution,
                pos = p_tbl$TimeSeries.Point.position,
                qty = NA_real_
              ) |>
                purrr::discard_at("qty") |>
                dplyr::rename(ts_point_dt_start = start_dt)
            } else {
              stop("No appropriate TimeSeries.Point resolver implemented.")
            }
          } else {
            names(p_tbl) <- my_snakecase(tbl = p_tbl)
            p_tbl <- p_tbl |>
              tidyr::nest(.key = "ts_point")
          }

          # return with the column-wise appended results
          return(dplyr::bind_cols(np_tbl, p_tbl))

        }) |>
        data.table::rbindlist(use.names = TRUE, fill = TRUE)

      # in special cases it checks for duplicate node indices
      det_grand_children_nps_names <- xml2::xml_name(det_grand_children_nps)
      if ("Asset_RegisteredResource" %in% det_grand_children_nps_names) {

        # detect the index of 'Asset_RegisteredResource' node element(s)
        # in the non-period-like grand children
        arr_ind <- det_grand_children_nps |>
          xml2::xml_name() |>
          grep(pattern = "^Asset_RegisteredResource$")

        # calculate the left over indices
        simple_ind <- base::setdiff(
          x = seq_along(det_grand_children_nps),
          y = unlist(arr_ind)
        )

        # if there are multiple "Asset_RegisteredResource" nodes
        if (length(arr_ind) > 1) {
          # extract them into as many tables as many occurences there are
          arr <- det_grand_children_nps[arr_ind] |>
            purrr::map(\(det_grand_child_nps) {
              # unpack xml into table
              # and convert numeric-like columns to numeric
              unpack_xml(
                section = det_grand_child_nps,
                parent_name = det_child_name
              ) |>
                dplyr::mutate(
                  dplyr::across(
                    tidyselect::matches(
                      "TimeSeries.mRID$|\\.nominalP$"
                    ),
                    as.numeric
                  )
                )
            }) |>
            data.table::rbindlist(use.names = TRUE, fill = TRUE)

          # assign 'ts_psr_type_def' to
          # 'TimeSeries.Asset_RegisteredResource.pSRType.psrType' column
          scope_col <- "TimeSeries.Asset_RegisteredResource.pSRType.psrType"
          if (scope_col %in% names(arr)) {
            arr <- def_merge(
              x = arr,
              y = asset_types,
              code_name = scope_col,
              definition_name = "ts_psr_type_def"
            )
          }

          # then concatenate the column values by which we will have
          # a 1 row data frame
          arr <- arr |>
            purrr::map(paste, collapse = " | ") |>
            tibble::as_tibble()

        } else {

          # add the single Asset_RegisteredResource index
          # to the single ind vector
          simple_ind <- c(simple_ind, arr_ind)

          # set arr as an empty data frame
          arr <- tibble::tibble()

        }

      } else {

        # extract indices all the simple elements
        # of the non-period-like grand children
        simple_ind <- seq_along(det_grand_children_nps)

        # set arr as an empty data frame
        arr <- tibble::tibble()

      }

      # iterate over the detailed grand children and bind them as columns
      res_dt_nps <- det_grand_children_nps[simple_ind] |>
        purrr::map(\(det_grand_child_nps) {
          # unpack xml into table and convert numeric-like columns to numeric
          unpack_xml(
            section = det_grand_child_nps,
            parent_name = det_child_name
          ) |>
            dplyr::mutate(
              dplyr::across(
                tidyselect::matches(
                  "TimeSeries.mRID$|\\.nominalP$"
                ),
                as.numeric
              )
            )
        }) |>
        dplyr::bind_cols(.name_repair = "minimal")

      # check if we have a valid Asset_RegisteredResource data frame
      if (nrow(arr) > 0) {

        # append the columns of the one row
        # Asset_RegisteredResource data frame to res_dt_nps
        res_dt_nps <- list(res_dt_nps, arr) |>
          dplyr::bind_cols(.name_repair = "minimal")

      }

      # check if there are multiple reason codes and/or texts
      dupl_reason <- base::intersect(
        x = names(res_dt_nps)[names(res_dt_nps) |> duplicated()],
        y = c("TimeSeries.Reason.code", "TimeSeries.Reason.text")
      )
      # if so, then ...
      for (col in dupl_reason) {
        # collapse the multiple values
        indices <- which(names(res_dt_nps) == col)
        first_idx <- indices[1]
        rest_idx <- base::setdiff(x = indices, y = first_idx)
        res_dt_nps[, first_idx] <- paste(res_dt_nps[, indices],
                                         collapse = " | ")
        res_dt_nps[, rest_idx] <- NULL
      }

      # return with the column-wise appended results
      if (ncol(res_dt_ps) > 0L) {
        return(dplyr::bind_cols(res_dt_nps, res_dt_ps))
      } else {
        return(res_dt_nps)
      }

    }) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE)

  # return with the detailed data table
  det_tbl
}



#' @title
#' convert xml content to table
#'
#' @noRd
xml_to_table <- function(xml_content, tidy_output = FALSE) {
  if (isFALSE(inherits(x = xml_content, what = "xml_document"))) {
    stop("The 'xml_content' should be an xml document!")
  }

  # process the ridge (metadata) of the xml document
  ridge_tbl <- process_ridge(xml_content = xml_content)

  # process the detailed data of the xml document
  det_tbl <- process_detailed(
    xml_content = xml_content,
    tidy_output = tidy_output
  )

  # compose the result table from ridge and the detailed family table
  if (ncol(ridge_tbl) && ncol(det_tbl)) {
    result_tbl <- dplyr::bind_cols(ridge_tbl, det_tbl)
  } else if (ncol(ridge_tbl)) {
    result_tbl <- ridge_tbl
  } else if (ncol(det_tbl)) {
    result_tbl <- det_tbl
  } else {
    stop("The result table is not a valid data frame!")
  }

  # rename columns to snakecase
  names(result_tbl) <- my_snakecase(tbl = result_tbl)

  # add type names to codes
  result_tbl <- add_type_names(tbl = result_tbl)

  # add eic names to eic codes
  result_tbl <- add_eic_names(tbl = result_tbl)

  # add definitions to codes
  result_tbl <- add_definitions(tbl = result_tbl)

  # select and reorder columns
  needed_cols <- c("ts_bidding_zone_domain_mrid",
                   "ts_bidding_zone_domain_name",
                   "ts_in_bidding_zone_domain_mrid",
                   "ts_in_bidding_zone_domain_name",
                   "ts_out_bidding_zone_domain_mrid",
                   "ts_out_bidding_zone_domain_name",
                   "control_area_domain_mrid",
                   "ts_in_domain_mrid", "ts_in_domain_name",
                   "ts_out_domain_mrid", "ts_out_domain_name",
                   "ts_production_mrid", "ts_production_name",
                   "ts_production_psr_mrid",
                   "ts_production_psr_name",
                   "doc_status_value", "doc_status",
                   "ts_mkt_psr_type_psr_mrid",
                   "ts_mkt_psr_type_psr_name",
                   "ts_registered_resource_mrid",
                   "ts_registered_resource_name",
                   "ts_asset_location_name",
                   "ts_asset_mrid", "ts_asset_name",
                   "ts_production_mrid", "ts_production_name",
                   "type", "type_def", "process_type",
                   "process_type_def",
                   "ts_contract_market_agreement_type",
                   "ts_contract_market_agreement_type_def",
                   "ts_auction_mrid", "ts_auction_type",
                   "ts_auction_type_def",
                   "ts_auction_category",
                   "ts_auction_category_def",
                   "ts_object_aggregation",
                   "ts_object_aggregation_def",
                   "ts_flow_direction", "ts_flow_direction_def",
                   "ts_business_type", "ts_business_type_def",
                   "ts_mkt_psr_type", "ts_mkt_psr_type_def",
                   "ts_asset_psr_type", "ts_asset_psr_type_def",
                   "ts_psr_type", "ts_psr_type_def",
                   "ts_production_psr_type",
                   "ts_production_psr_type_def",
                   "created_date_time",
                   "reason_code", "reason_text",
                   "ts_reason_code", "ts_reason_text",
                   "revision_number",
                   "time_period_time_interval_start",
                   "time_period_time_interval_end",
                   "unavailability_time_interval_start",
                   "unavailability_time_interval_end",
                   "ts_resolution", "ts_time_interval_start",
                   "ts_time_interval_end", "ts_mrid",
                   "ts_point", "ts_point_dt_start",
                   "ts_production_psr_nominal_p",
                   "ts_point_quantity", "ts_point_price",
                   "ts_point_congestion_cost",
                   "ts_currency_unit_name",
                   "ts_price_measure_unit_name",
                   "ts_quantity_measure_unit_name",
                   "high_voltage_limit")
  needed_cols <- base::intersect(x = needed_cols,
                                 y = names(result_tbl))

  # check if any columns left to keep
  if (length(needed_cols)) {
    # filter on the needed columns
    result_tbl <- result_tbl |>
      dplyr::select(dplyr::all_of(needed_cols))

    # reorder the rows
    sort_cols <- base::intersect(x = c("created_date_time", "ts_mrid",
                                       "ts_business_type", "ts_mkt_psr_type",
                                       "ts_time_interval_start",
                                       "ts_point_dt_start"),
                                 y = names(result_tbl))
    data.table::setorderv(x = result_tbl, cols = sort_cols)

    # convert the result to tibble
    result_tbl <- tibble::as_tibble(result_tbl)

    return(result_tbl)
  } else {
    stop("There is no interesting columns in the result table!")
  }

}



#' @title
#' extract the response from content list
#'
#' @noRd
extract_response <- function(content, tidy_output = TRUE) {

  # check if the content is in the required list format
  if (is.list(content) &&
        length(content) == 2L &&
        all(names(content) == c("result", "error"))) {

    # extract the possible failure reason
    reason <- content$error

    # if valid content got
    if (is.null(reason)) {

      # if the response is not list, then convert it to list
      if (inherits(x = content$result, what = "list")) {
        # convert XMLs to tables
        response_length <- length(content$result)
        result_tbl <- purrr::imap(content$result,
                                  \(x, idx) {
                                    times <- response_length - idx + 1
                                    message(
                                      idx, " ", rep(x = "<", times = times)
                                    )
                                    xml_to_table(
                                      xml_content = x,
                                      tidy_output = tidy_output
                                    )
                                  }) |>
          data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
          tibble::as_tibble()
      } else {
        result_tbl <- xml_to_table(
          xml_content = content$result,
          tidy_output = tidy_output
        )
      }
      return(result_tbl)

    } else {

      stop(reason)

    }
  } else {

    stop("The content is not in the required list format!")

  }
}
