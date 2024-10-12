# calculate offset URLs
calc_offset_urls <- function(reason, url) {
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
  message("*** The amount of requested data exceeds allowed limit, ",
          "therefore the request has been rephrased. ***")
  return(paste0(url, "&offset=", all_offset_seq))
}



# read XML content from a zip compressed file
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


# call request against the ENTSO-E API and converts the response into xml
api_req <- function(url = NULL) {
  if (is.null(url)) {
    stop("The argument 'url' is missing!")
  }
  api_url <- "https://web-api.tp.entsoe.eu/api"
  if (!startsWith(x = url, prefix = api_url)) {
    stop("The argument 'url' is not valid!")
  }
  message(url, " ...")
  temp_file_path <- tempfile()
  resp <- httr::GET(url,
                    httr::write_disk(path = temp_file_path, overwrite = TRUE))
  message("downloaded")

  # if the get request is successful, then ...
  if (httr::status_code(resp) == "200") {

    # if the request is a zip file, then ...
    if (resp$headers$`content-type` == "application/zip") {

      # read the xml content from each the decompressed files
      en_cont_list <- read_zipped_xml(temp_file_path)

      # return with the xml content list
      return(en_cont_list)

    } else {

      # read the xml content from the response
      en_cont <- httr::content(resp, encoding = "UTF-8")

      # return with the xml content
      return(en_cont)

    }

  } else {

    # extract reason from reason text
    response_reason <- httr::content(resp, encoding = "utf-8") |>
      xml2::as_list() |>
      purrr::pluck("Acknowledgement_MarketDocument", "Reason", "text") |>
      unlist()

    # check if offset usage needed or not
    offset_needed <- stringr::str_detect(
      string = response_reason,
      pattern = "The amount of requested data exceeds allowed limit."
    )

    # if offset usage needed, then ...
    if (isTRUE(offset_needed)) {

      # calculate offset URLs
      offset_urls <- calc_offset_urls(
        reason = response_reason,
        url = url
      )

      # recursively call the api_req() function itself
      en_cont_list <- purrr::map(offset_urls, api_req) |>
        unlist(recursive = FALSE)

      return(en_cont_list)

    } else {

      stop(httr::content(resp, encoding = "UTF-8"))

    }

  }
}



# safely call api_req() function
api_req_safe <- purrr::safely(api_req)



# converts the given POSIXct or character timestamp into the acceptable format
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



# create a time series table between "from" till "to"
# with "seq_resolution" frequency
# using "pos" position and "qty" quantity values
dt_seq_helper <- function(from, to, seq_resolution = "PT60M", pos, qty) {
  # define those variables as NULL which are used under non-standard evaluation
  start_dt <- NULL
  qty <- qty

  # calculate "by" value from "seq_resolution" value
  by <- tryCatch(
    dplyr::case_when(seq_resolution == "PT1M" ~ "1 min",
                     seq_resolution == "PT15M" ~ "15 mins",
                     seq_resolution == "PT30M" ~ "30 mins",
                     seq_resolution == "PT60M" ~ "1 hour",
                     seq_resolution == "P1D" ~ "1 DSTday",
                     seq_resolution == "P7D" ~ "7 DSTdays",
                     seq_resolution == "P1Y" ~ "1 year",
                     .default = "n/a"),
    error = function(cond) {
      message(conditionMessage(cond))
      message("The sequence resolution does not seem to be valid.")
      message(paste("Its type is:", typeof(seq_resolution)))
      message(paste("Its structure is:", utils::capture.output(utils::str(seq_resolution))))
      # set a return value in case of error
      "n/a"
    }
  )

  # check if we got a valid resolution
  if (by == "n/a") {

    stop("The 'resolution' value of the response is not supported yet.",
         "\nPlease use 'PT1M', 'PT15M', 'PT30M', 'PT60M', 'P1D', ",
         "'P7D' or 'P1Y'.")

  } else if (by %in% c("1 DSTday", "7 DSTdays", "P1Y" ~ "1 year")) {

    # create a datetime vector from "from" (incl.) to "to" (excl.) by "by"
    tzone <- dplyr::case_when(
      format(x = from, format = "%H", tz = "UTC") == "00" ~ "UTC",
      format(x = from, format = "%H", tz = "WET") == "00" ~ "WET",
      format(x = from, format = "%H", tz = "CET") == "00" ~ "CET",
      format(x = from, format = "%H", tz = "EET") == "00" ~ "EET",
      format(x = from, format = "%H", tz = "Europe/Moscow") == "00" ~ "Europe/Moscow",
      .default = "not_known"
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
  dt_tbl <- merge(x = data.table::data.table(start_dt = dts),
                  y = data.table::data.table(start_dt = dts[pos],
                                             qty),
                  by = "start_dt",
                  all.x = TRUE)
  data.table::set(x     = dt_tbl,
                  j     = "qty",
                  value = data.table::nafill(x = dt_tbl$qty, type = "locf"))

  return(tibble::as_tibble(dt_tbl))
}



# downloads approved Energy Identification Codes
# from ENTSO-E Transparency Platform
# under link "f"
get_eiccodes <- function(f) {
  message("\ndownloading ", f, " file ...")

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



# unpack an xml section into a tabular row
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



# an own version of snakecase::to_snakecase() function
# read and convert the column names of the provided data frame
# into the required snakecase format
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
    stringr::str_remove(
      pattern = "ts_mkt_psr_type_voltage_psr_"
    )
}



# detect the number of grand children per each child
xml_grand_children_lengths <- function(xml_content) {
  xml2::xml_children(xml_content) |> xml2::xml_length()
}



# add definitions to codes
add_definitions <- function(tbl) {
  if (isFALSE(is.data.frame(tbl))) {
    stop("The provided argument is not a valid data frame!")
  }
  # define those variables as NULL which are used under non-standard evaluation
  reason_text.x <- reason_text.y <- CODE <- DEFINITION <- NULL
  EicCode <- EicLongName <- eic_code <- eic_long_name <- eic_name <- NULL

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

  # convert result_tbl to data.table in order to join faster
  tbl <- tbl |>
    data.table::data.table()

  # convert area_eic() table to data.table in order to join faster
  area_eic_name <- area_eic()
  area_eic_name <- area_eic_name |>
    dplyr::select(EicCode, EicLongName) |>
    dplyr::rename_with(snakecase::to_snake_case) |>
    dplyr::group_by(eic_code) |>
    dplyr::mutate(eic_name = stringr::str_c(
      eic_long_name,
      collapse = " - "
    )) |>
    dplyr::ungroup() |>
    dplyr::select(eic_code, eic_name) |>
    data.table::data.table()

  # define an empty vector to collect those column names
  # which will get definitions by add_definitions() function
  affected_cols <- c()

  # add definitions to codes
  if ("type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "type")
    tbl <- tbl |>
      merge(y = document_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(type = CODE,
                            type_def = DEFINITION) |>
              data.table::data.table(),
            by = "type",
            all.x = TRUE)
  }
  if ("ts_business_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_business_type")
    tbl <- tbl |>
      merge(y = business_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_business_type = CODE,
                            ts_business_type_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_business_type",
            all.x = TRUE)
  }
  if ("ts_mkt_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_mkt_psr_type")
    tbl <- tbl |>
      merge(y = asset_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_mkt_psr_type = CODE,
                            ts_mkt_psr_type_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_mkt_psr_type",
            all.x = TRUE)
  }
  if ("doc_status_value" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "doc_status_value")
    tbl <- tbl |>
      merge(y = document_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(doc_status_value = CODE,
                            doc_status = DEFINITION) |>
              data.table::data.table(),
            by = "doc_status_value",
            all.x = TRUE)
  }
  if ("ts_asset_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_asset_psr_type")
    tbl <- tbl |>
      merge(y = asset_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_asset_psr_type = CODE,
                            ts_asset_psr_type_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_asset_psr_type",
            all.x = TRUE)
  }
  if ("ts_production_psr_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_production_psr_type")
    tbl <- tbl |>
      merge(y = asset_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_production_psr_type = CODE,
                            ts_production_psr_type_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_production_psr_type",
            all.x = TRUE)
  }
  if ("process_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "process_type")
    tbl <- tbl |>
      merge(y = process_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(process_type = CODE,
                            process_type_def = DEFINITION) |>
              data.table::data.table(),
            by = "process_type",
            all.x = TRUE)
  }
  if ("ts_contract_market_agreement_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_contract_market_agreement_type")
    my_contract_types <- contract_types |>
      dplyr::select(CODE, DEFINITION) |>
      dplyr::rename(
        ts_contract_market_agreement_type = CODE,
        ts_contract_market_agreement_type_def = DEFINITION
      ) |>
      data.table::data.table()
    tbl <- tbl |>
      merge(y = my_contract_types,
            by = "ts_contract_market_agreement_type",
            all.x = TRUE)
  }
  if ("ts_auction_type" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_auction_type")
    tbl <- tbl |>
      merge(y = auction_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_auction_type = CODE,
                            ts_auction_type_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_auction_type",
            all.x = TRUE)
  }
  if ("ts_auction_category" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_auction_category")
    tbl <- tbl |>
      merge(y = category_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_auction_category = CODE,
                            ts_auction_category_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_auction_category",
            all.x = TRUE)
  }
  if ("reason_code" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "reason_code")
    tbl <- tbl |>
      merge(y = reason_code_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(reason_code = CODE,
                            reason_text = DEFINITION) |>
              data.table::data.table(),
            by = "reason_code",
            all.x = TRUE)
    if ("reason_text.x" %in% names(tbl) &&
          "reason_text.y" %in% names(tbl)) {
      tbl <- tbl |>
        dplyr::mutate(
          reason_text = paste(reason_text.y, reason_text.x,
                              sep = " - "),
          reason_text.x = NULL,
          reason_text.y = NULL
        )
    }
  }
  if ("ts_object_aggregation" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_object_aggregation")
    tbl <- tbl |>
      merge(y = object_aggregation_types |>
              dplyr::select(CODE, DEFINITION) |>
              dplyr::rename(ts_object_aggregation = CODE,
                            ts_object_aggregation_def = DEFINITION) |>
              data.table::data.table(),
            by = "ts_object_aggregation",
            all.x = TRUE)
  }
  if ("ts_registered_resource_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_registered_resource_mrid")
    resource_object_eic <- resource_object_eic() |>
      dplyr::select(EicCode, EicLongName) |>
      dplyr::rename(ts_registered_resource_mrid = EicCode,
                    ts_registered_resource_name = EicLongName) |>
      data.table::data.table()
    tbl <- tbl |>
      dplyr::select(
        purrr::discard(
          names(tbl),
          identical,
          y = "ts_registered_resource_name"
        )
      ) |>
      merge(y = resource_object_eic,
            by = "ts_registered_resource_mrid",
            all.x = TRUE)
  }
  if ("ts_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_bidding_zone_domain_mrid")
    tbl <- tbl |>
      merge(y = area_eic_name |>
              dplyr::rename(ts_bidding_zone_domain_name = eic_name,
                            ts_bidding_zone_domain_mrid = eic_code),
            by = "ts_bidding_zone_domain_mrid",
            all.x = TRUE)
  }
  if ("ts_in_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_in_bidding_zone_domain_mrid")
    tbl <- tbl |>
      merge(y = area_eic_name |>
              dplyr::rename(ts_in_bidding_zone_domain_name = eic_name,
                            ts_in_bidding_zone_domain_mrid = eic_code),
            by = "ts_in_bidding_zone_domain_mrid",
            all.x = TRUE)
  }
  if ("ts_out_bidding_zone_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_out_bidding_zone_domain_mrid")
    tbl <- tbl |>
      merge(y = area_eic_name |>
              dplyr::rename(ts_out_bidding_zone_domain_name = eic_name,
                            ts_out_bidding_zone_domain_mrid = eic_code),
            by = "ts_out_bidding_zone_domain_mrid",
            all.x = TRUE)
  }
  if ("ts_in_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_in_domain_mrid")
    tbl <- tbl |>
      merge(y = area_eic_name |>
              dplyr::rename(ts_in_domain_name = eic_name,
                            ts_in_domain_mrid = eic_code),
            by = "ts_in_domain_mrid",
            all.x = TRUE)
  }
  if ("ts_out_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "ts_out_domain_mrid")
    tbl <- tbl |>
      merge(y = area_eic_name |>
              dplyr::rename(ts_out_domain_name = eic_name,
                            ts_out_domain_mrid = eic_code),
            by = "ts_out_domain_mrid",
            all.x = TRUE)
  }
  if ("control_area_domain_mrid" %in% names(tbl)) {
    affected_cols <- c(affected_cols, "control_area_domain_mrid")
    tbl <- tbl |>
      merge(y = area_eic_name |>
              dplyr::rename(control_area_domain_name = eic_name,
                            control_area_domain_mrid = eic_code),
            by = "control_area_domain_mrid",
            all.x = TRUE)
  }
  if (length(affected_cols) == 0L) {
    warning(
      "column names: ",
      paste(names(tbl), collapse = " - "),
      "\n  No additional definitions added!"
    )
  }

  return(tbl)
}



# convert xml content to table
xml_to_table <- function(xml_content, tidy_output = FALSE) {
  if (isFALSE(inherits(x = xml_content, what = "xml_document"))) {
    stop("The 'xml_content' should be an xml document!")
  }

  # define those variables as NULL which are used under non-standard evaluation
  start_dt <- qty <- NULL

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
      det_grand_children_ps <- det_grand_children[ps_ind]
      det_grand_children_nps <- det_grand_children[nps_ind]

      # iterate over the detailed grand children and bind them as columns
      res_dt_ps <- det_grand_children_ps |>
        purrr::map(\(det_grand_child_ps) {

          # extract elements
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
                dplyr::rename(ts_point_dt_start = start_dt,
                              ts_point_price = qty)
            } else {
              p_tbl <- dt_seq_helper(
                from = np_tbl$TimeSeries.timeInterval.start,
                to = np_tbl$TimeSeries.timeInterval.end,
                seq_resolution = np_tbl$TimeSeries.resolution,
                pos = p_tbl$TimeSeries.Point.position,
                qty = p_tbl$TimeSeries.Point.quantity
              ) |>
                dplyr::rename(ts_point_dt_start = start_dt,
                              ts_point_quantity = qty)
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

      # iterate over the detailed grand children and bind them as columns
      res_dt_nps <- det_grand_children_nps |>
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
                                         collapse = "|")
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

  # compose the result table from ridge and the detailed family table
  if (ncol(ridge_tbl) && ncol(det_tbl)) {
    result_tbl <- dplyr::bind_cols(ridge_tbl, det_tbl)
  } else if (ncol(ridge_tbl)) {
    result_tbl <- ridge_tbl
  } else if (ncol(det_tbl)) {
    result_tbl <- det_tbl
  }

  # rename columns to snakecase
  names(result_tbl) <- my_snakecase(tbl = result_tbl)

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
                   "ts_business_type", "ts_business_type_def",
                   "ts_mkt_psr_type", "ts_mkt_psr_type_def",
                   "ts_asset_psr_type", "ts_asset_psr_type_def",
                   "ts_production_psr_type",
                   "ts_production_psr_type_def",
                   "created_date_time", "reason_code",
                   "reason_text", "ts_reason_code", "ts_reason_text",
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



# extract the response
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

      # return with an empty table
      stop(reason)

    }
  } else {

    stop("The content is not in the required list format!")

  }
}
