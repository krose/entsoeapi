

api_req <- function(url){

  message(url, " ...")
  req <- httr::GET(url)

  if(httr::status_code(req) != "200"){
    stop(httr::content(req, encoding = "UTF-8"))
  }
  en_cont <- httr::content(req, encoding = "UTF-8")
  message("downloaded")

  en_cont
}

api_req_safe <- function(..., otherwise = NULL, quiet = TRUE) {
  purrr:::capture_error(api_req(...), otherwise = otherwise, quiet = quiet)
}


url_posixct_format <- function(x){

  if(any(class(x) == "POSIXct")){
    y <- strftime(x = x, format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
  } else {
    y <- lubridate::parse_date_time(x      = x,
                                    orders = c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
                                               "%Y.%m.%d %H:%M:%S", "%Y.%m.%d %H:%M", "%Y.%m.%d",
                                               "%Y%m%d%H%M%S",      "%Y%m%d%H%M",     "%Y%m%d"),
                                    tz     = "UTC",
                                    quiet  = TRUE) %>%
      strftime(format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
    if(is.na(y)) {
      stop("Only the class POSIXct or '%Y-%m-%d %H:%M:%S' formatted text are supported by the converter.")
    } else {
      warning("The ", x, " value interpreted as UTC.", call. = FALSE)
    }
  }

  y
}


dt_helper <- function(tz_start, tz_resolution, tz_position){
  if(!lubridate::is.POSIXct(tz_start)) {
    tz_start <- as.POSIXct( x = tz_start,
                            tryFormats = c("%Y-%m-%dT%H:%MZ",
                                           "%Y-%m-%dT%H:%M:%SZ",
                                           "%Y-%m-%d %H:%M",
                                           "%Y-%m-%d %H:%M:%S"),
                            tz = "UTC")
  }
  if(tz_resolution == "PT60M"){
    dt <- tz_start + lubridate::hours(tz_position - 1)
  } else if(tz_resolution == "PT15M"){
    dt <- tz_start + lubridate::minutes((tz_position - 1) * 15)
  } else if(tz_resolution == "PT30M"){
    dt <- tz_start + lubridate::minutes((tz_position - 1) * 30)
  } else if(tz_resolution == "P1D"){
    dt <- tz_start + lubridate::days((tz_position - 1))
  } else {
    stop("Resolution not supported.")
  }

  dt
}

dt_seq_helper <- function(from, to, seq_resolution = NULL, qty){

  # if(seq_resolution == "PT1M"){
    # df_dt <- data.frame(dt = seq(from, to, by = "1 mins"))
    # df_dt$dt <- df_dt$dt + lubridate::minutes(1) * (seq_along(df_dt$dt) - 1)
  # } else {
    df_dt <- data.frame(dt = lubridate::floor_date(seq(from, to, by = "hours"), unit = "hours"))
    df_dt$qty <- qty

    # df_dt <- data.table::as.data.table(df_dt)
    #
    # df_dt <-
    #   df_dt %>%
    #   dplyr::group_by(dt) %>%
    #   dplyr::summarise(qty = mean(qty)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::as_tibble()
  # }

  df_dt
}

get_eiccodes <- function( f ) {
  message( "\ndownloading ", f, " file ..." )

  ## readding input file into a character vector
  ## and replacing erroneous semicolons to commas
  ## unfortunately there is no general rule for that hence it must be set manually!!
  lns        <- readLines( con = f, encoding = "UTF-8" ) %>%
    iconv(x = ., "UTF-8", "ASCII", sub="") %>%
    iconv(x = ., "", "ASCII", "byte") %>%
    gsub(x = ., pattern     = "<\\s*U\\+[0-9A-Fa-f]{4,6}\\s*>",
         replacement = "",
         perl        = TRUE) %>%
    enc2utf8() %>%
    gsub(x = ., pattern     = "tutkimustehdas;\\sImatra",
          replacement = "tutkimustehdas, Imatra",
          perl        = TRUE ) %>%
    gsub(x = ., pattern     = "; S\\.L\\.;",
          replacement = ", S.L.;",
          perl        = TRUE ) %>%
    gsub(x = ., pattern     = "\\$amp;",
          replacement = "&",
          perl        = TRUE ) %>%
    gsub(x = ., pattern = "<e2><80><93> ",
         replacement = "",
         perl = TRUE)

  ## looking for those lines (elements) which end are not
  ## according to the general rules
  clps_ind   <- grep( x       = lns,
                      pattern = ";type$|;X$|;Y$|;Z$|;T$|;V$|;W$|;A$",
                      perl    = TRUE,
                      invert  = TRUE )

  ## if there are being collapsible elements
  if( length( x = clps_ind ) > 0L ) {

    ## iterating related elements thru from the last till the first element
    for( i in rev( clps_ind ) ) {
      ## collapsing related line (element) with its subsequent neighbor
      lns[ i ]      <- paste0( lns[ i ], lns[ i + 1L ], collapse = "" )
    }

    ## removing subsequent neighbors (after collapse)
    lns <- lns[ -( clps_ind + 1L ) ]

  }

  ## reading lines as they would be a csv
  eiccodes   <- data.table::fread( text       = lns,
                                   sep        =";",
                                   na.strings = c( "", "n / a", "n/a", "N/A", "-", "-------", "." ),
                                   encoding   = "UTF-8" )

  ## trimming character columns
  eiccodes <-
    lapply( X   = names( x = eiccodes ),
          FUN = function( col ) {
            if( is.character( eiccodes[[ col ]] ) ) {
              data.table::set( x     = eiccodes,
                               j     = col,
                               value = trimws( x     = eiccodes[[ col ]],
                                               which = "both" ) )
            }
          } )

  return( eiccodes )
}

tm_quantity_helper <- function(x, patt){
  x <- x$Publication_MarketDocument[names(x$Publication_MarketDocument) == "TimeSeries"]
  x <- purrr::map(x, ~timeseries_extract_quantity(.x, patt))
  x <- dplyr::bind_rows(x)

  x
}

timeseries_extract_quantity <- function(x, patt){
  value <- as.numeric(unlist(purrr::map(x$Period, patt)))
  resolution <- x$Period$resolution[[1]]
  position <- as.integer(unlist(purrr::map(x$Period, "position")))
  dt <- dt_helper(tz_start = strptime(x = x$Period$timeInterval$start[[ 1L ]], format = "%Y-%m-%dT%H:%MZ", tz = "UTC") %>% as.POSIXct(tz = "UTC"), tz_resolution = resolution, tz_position = position)
  res <- tibble::tibble(dt, value, resolution)

  res
}
