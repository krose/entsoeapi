en_capacity_api_req_helper <- function(in_domain,
                                       out_domain,
                                       period_start,
                                       period_end,
                                       security_token,
                                       contract_type    = NULL,
                                       auction_category = NULL,
                                       document_type,
                                       business_type) {

  ## composing url(s)
  paste0(
    "https://web-api.tp.entsoe.eu/api",
    "?documentType=", document_type,
    "&businessType=", business_type,
    if(!is.null( contract_type )) paste0("&contract_MarketAgreement.Type=", contract_type),
    if(!is.null( auction_category )) paste0("&Auction.Category=", auction_category),
    "&in_Domain=", in_domain,
    "&out_Domain=", out_domain,
    "&periodStart=",period_start,
    "&periodEnd=", period_end,
    "&securityToken=", security_token
  )

}

ts_capacity_helper <- function(ts) {

  period    <- ts$Period
  ts$Period <- NULL

  points    <- purrr::map_dfr(period[ names(period) == "Point" ],
                              ~list(position = as.integer(.x$position[[ 1L ]]),
                                    quantity = as.integer(.x$quantity[[ 1L ]]))) %>%
    tibble::add_column(dt = dt_helper(tz_start      = strptime(x = period$timeInterval$start[[ 1L ]], format = "%Y-%m-%dT%H:%MZ", tz = "UTC") %>% as.POSIXct(tz = "UTC"),
                                      tz_resolution = period$resolution[[ 1L ]],
                                      tz_position   = .$position))

  ts        <- purrr::map_dfc(ts, unlist) %>%
    tibble::add_column( points = list(points) ) %>%
    tidyr::unnest("points")

  ts
}


#' Get total nominated capacity from Entsoe
#'
#' @param in_domain Energy Identification Code of the bidding zone or control area (TSO)
#' @param out_domain Energy Identification Code of the bidding zone or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param tidy_output if TRUE then column names harmonized and definitive columns added, defaults to FALSE
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' CZ_DE50HTZ_201902 <- en_capacity_total_nominated(in_domain = "10YDE-VE-------2",
#'                                                  out_domain = "10YCZ-CEPS-----N",
#'                                                  period_start = lubridate::ymd("2019-02-01", tz = "CET"),
#'                                                  period_end = lubridate::ymd("2019-03-01", tz = "CET"))
#'

en_capacity_total_nominated <- function(in_domain,
                                        out_domain,
                                        period_start,
                                        period_end,
                                        tidy_output = FALSE,
                                        security_token = Sys.getenv("ENTSOE_PAT"),
                                        document_type = "A26",
                                        business_type = "B08") {

  stopifnot(length(in_domain) == 1L,
            length(out_domain) == 1L,
            length(period_start) == 1L,
            length(period_end) == 1L,
            difftime(period_end, period_start, units = "day") <= 365,
            is.logical(tidy_output),
            length(security_token) == 1L,
            document_type == "A26",
            business_type == "B08")

  url_list     <- en_capacity_api_req_helper(in_domain      = in_domain,
                                             out_domain     = out_domain,
                                             period_start   = url_posixct_format(period_start),
                                             period_end     = url_posixct_format(period_end),
                                             security_token = security_token,
                                             document_type  = document_type,
                                             business_type  = business_type)

  rqst    <- purrr::map(url_list, api_req_safe)

  if(!is.null(purrr::map(rqst, "error")[[ 1L ]])) {

    purrr::map(rqst, "error") %>% purrr::map_chr("message") %>% warning()
    return(tibble::tibble())

  } else {

    en_cont <- purrr::map(rqst, "result")
    en_cont[vapply(X = en_cont, FUN = is.null, FUN.VALUE = TRUE)] <- NULL

    en_cont <- en_cont %>%
      purrr::map(xml2::as_list) %>%
      purrr::map("Publication_MarketDocument") %>%
      unlist(recursive = FALSE)

    en_cont <- en_cont[names(en_cont) == "TimeSeries"] %>%
      purrr::map_dfr(ts_capacity_helper) %>%
      tibble::add_column(document_type = document_type)

    ## if the output should be tidy
    if(tidy_output & nrow(en_cont) > 0L) {

      ## querying area EIC codes
      eic  <- area_eic()[, c("EicCode", "EicDisplayName")]

      ## remove not necessary columns
      en_cont   <- en_cont %>% dplyr::select(-mRID, -curveType, -position)
      ## renaming columns
      names(en_cont) <- names(en_cont) %>%
        gsub(pattern = ".", replacement = "_", fixed = TRUE) %>%
        gsub(pattern = "Type", replacement = "_Type", fixed = TRUE) %>%
        gsub(pattern = "Agreement", replacement = "_Agreement", fixed = TRUE) %>%
        tolower()
      ## adding definitions to codes
      ## and reordering columns
      en_cont   <- en_cont %>%
        dplyr::left_join(y     = StandardBusinessTypeList[, c("CODE", "DEFINITION")] %>%
                                   dplyr::rename(business_type = CODE, business_type_def = DEFINITION),
                         by    = "business_type",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = StandardContractTypeList[, c("CODE", "DEFINITION")] %>%
                                   dplyr::rename(contract_market_agreement_type = CODE, contract_market_agreement_type_def = DEFINITION),
                         by    = "contract_market_agreement_type",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = eic %>% dplyr::rename(in_domain_mrid = EicCode, in_domain_mrid_def = EicDisplayName),
                         by    = "in_domain_mrid",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = eic %>% dplyr::rename(out_domain_mrid = EicCode, out_domain_mrid_def = EicDisplayName),
                         by    = "out_domain_mrid",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = StandardDocumentTypeList[, c("CODE", "DEFINITION")] %>%
                           dplyr::rename(document_type = CODE, document_type_def = DEFINITION),
                         by    = "document_type",
                         all.x = TRUE) %>%
        dplyr::select(base::intersect(x = c("document_type", "document_type_def",
                                            "business_type", "business_type_def",
                                            "in_domain_mrid", "in_domain_mrid_def",
                                            "out_domain_mrid", "out_domain_mrid_def",
                                            "contract_market_agreement_type",
                                            "contract_market_agreement_type_def", "dt",
                                            "quantity", "quantity_measure_unit_name"),
                                      y = names(.))) %>%
        dplyr::arrange(dt)
    }

    return(en_cont)

  }
}




#' Get total capacity already allocated from Entsoe
#'
#' @param in_domain Energy Identification Code of the bidding zone or control area (TSO)
#' @param out_domain Energy Identification Code of the bidding zone or control area (TSO)
#' @param period_start POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param period_end POSIXct or YYYY-MM-DD HH:MM:SS format  One year range limit applies
#' @param contract_type Contract market agreement type, valid values can be checked from StandardContractTypeList table
#' @param auction_category Auction category, valid values can be checked from StandardAuctionTypeList table
#' @param tidy_output if TRUE then column names harmonized and definitive columns added, defaults to FALSE
#' @param security_token Security token for ENTSO-E transparency platform
#'
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' library(entsoeapi)
#'
#' CZ_DE50HTZ_201902w1 <- en_capacity_already_allocated(in_domain        = "10YDE-VE-------2",
#'                                                      out_domain       = "10YCZ-CEPS-----N",
#'                                                      period_start     = lubridate::ymd("2019-02-01", tz = "CET"),
#'                                                      period_end       = lubridate::ymd("2019-02-08", tz = "CET"),
#'                                                      auction_category = "A01",
#'                                                      contract_type    = "A07")
#'

en_capacity_already_allocated <- function(in_domain,
                                          out_domain,
                                          period_start,
                                          period_end,
                                          tidy_output = FALSE,
                                          security_token = Sys.getenv("ENTSOE_PAT"),
                                          contract_type = "A07",
                                          auction_category = NULL,
                                          document_type = "A26",
                                          business_type = "A29") {

  stopifnot(length(in_domain) == 1L,
            length(out_domain) == 1L,
            length(period_start) == 1L,
            length(period_end) == 1L,
            is.logical(tidy_output),
            length(contract_type) == 1L,
            difftime(period_end, period_start, units = "day") <= 365,
            length(security_token) == 1L,
            document_type == "A26",
            business_type == "A29")

  url_list     <- en_capacity_api_req_helper(in_domain        = in_domain,
                                             out_domain       = out_domain,
                                             period_start     = url_posixct_format(period_start),
                                             period_end       = url_posixct_format(period_end),
                                             security_token   = security_token,

                                             contract_type    = contract_type,
                                             auction_category = auction_category,

                                             document_type    = document_type,
                                             business_type    = business_type)

  rqst    <- purrr::map(url_list, api_req_safe)

  if(!is.null(purrr::map(rqst, "error")[[ 1L ]])) {

    purrr::map(rqst, "error") %>% purrr::map_chr("message") %>% warning()
    return(tibble::tibble())

  } else {

    en_cont <- purrr::map(rqst, "result")
    en_cont[vapply(X = en_cont, FUN = is.null, FUN.VALUE = TRUE)] <- NULL

    en_cont <- en_cont %>%
      purrr::map(xml2::as_list) %>%
      purrr::map("Publication_MarketDocument") %>%
      unlist(recursive = FALSE)

    en_cont <- en_cont[names(en_cont) == "TimeSeries"] %>%
      purrr::map_dfr(ts_capacity_helper) %>%
      tibble::add_column(document_type = document_type)

    ## if the output should be tidy
    if(tidy_output & nrow(en_cont) > 0L) {

      ## querying area EIC codes
      eic  <- area_eic()[, c("EicCode", "EicDisplayName")]

      ## remove not necessary columns
      en_cont   <- en_cont %>% dplyr::select(-mRID, -curveType, -position)
      ## renaming columns
      names(en_cont) <- names(en_cont) %>%
        gsub(pattern = ".", replacement = "_", fixed = TRUE) %>%
        gsub(pattern = "Type", replacement = "_Type", fixed = TRUE) %>%
        gsub(pattern = "Agreement", replacement = "_Agreement", fixed = TRUE) %>%
        tolower()
      ## adding definitions to codes
      ## and reordering columns
      en_cont   <- en_cont %>%
        dplyr::left_join(y     = StandardAuctionTypeList[, c("CODE", "DEFINITION")] %>%
                           dplyr::rename(auction_type = CODE, auction_type_def = DEFINITION),
                         by    = "auction_type",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = StandardCategoryTypeList[, c("CODE", "DEFINITION")] %>%
                           dplyr::rename(auction_category = CODE, auction_category_def = DEFINITION),
                         by    = "auction_category",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = StandardBusinessTypeList[, c("CODE", "DEFINITION")] %>%
                           dplyr::rename(business_type = CODE, business_type_def = DEFINITION),
                         by    = "business_type",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = StandardContractTypeList[, c("CODE", "DEFINITION")] %>%
                           dplyr::rename(contract_market_agreement_type = CODE, contract_market_agreement_type_def = DEFINITION),
                         by    = "contract_market_agreement_type",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = eic %>% dplyr::rename(in_domain_mrid = EicCode, in_domain_mrid_def = EicDisplayName),
                         by    = "in_domain_mrid",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = eic %>% dplyr::rename(out_domain_mrid = EicCode, out_domain_mrid_def = EicDisplayName),
                         by    = "out_domain_mrid",
                         all.x = TRUE) %>%
        dplyr::left_join(y     = StandardDocumentTypeList[, c("CODE", "DEFINITION")] %>%
                           dplyr::rename(document_type = CODE, document_type_def = DEFINITION),
                         by    = "document_type",
                         all.x = TRUE) %>%
        dplyr::select(base::intersect(x = c("auction_mrid", "auction_type", "auction_type_def",
                                            "auction_category", "auction_category_def",
                                            "document_type", "document_type_def",
                                            "business_type", "business_type_def",
                                            "in_domain_mrid", "in_domain_mrid_def",
                                            "out_domain_mrid", "out_domain_mrid_def",
                                            "contract_market_agreement_type",
                                            "contract_market_agreement_type_def", "dt",
                                            "quantity", "quantity_measure_unit_name"),
                                      y = names(.))) %>%
        dplyr::arrange(dt)
    }

    return(en_cont)

  }
}



