

api_req <- function(url){

  req <- httr::GET(url)

  if(httr::status_code(req) != "200"){
    stop(httr::content(req, encoding = "UTF-8"))
  }
  en_cont <- httr::content(req, encoding = "UTF-8")

  en_cont
}


url_posixct_format <- function(x){

  if("POSIXct" %in% class(x)){
    x <- strftime(x = x, format = "%Y%m%d%H%M", tz = "UTC", usetz = FALSE)
  } else {
    stop("Only the class POSIXct is supported by the converter.")
  }

  x
}


dt_helper <- function(tz_start, tz_resolution, tz_position){
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

dt_seq_helper <- function(from, to, seq_resolution, qty){

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
    gsub( pattern     = "tutkimustehdas;\\sImatra",
          replacement = "tutkimustehdas, Imatra",
          perl        = TRUE ) %>%
    gsub( pattern     = "; S\\.L\\.;",
          replacement = ", S.L.;",
          perl        = TRUE ) %>%
    gsub( pattern     = "\\$amp;",
          replacement = "&",
          perl        = TRUE )

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
