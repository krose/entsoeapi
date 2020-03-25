

api_req <- function(url){

  req <- httr::GET(url)

  if(httr::status_code(req) != "200"){
    stop(httr::content(req, encoding = "UTF-8"))
  }
  en_cont <- httr::content(req, encoding = "UTF-8")

  en_cont
}

#' save and unzip the zip file with xml data.
api_req_zip <- function(url){

  temp_file_path <- paste0("~/temp-entsoe")

  if(!dir.exists(temp_file_path)){
    dir.create(temp_file_path)
  } else {
    un_res <- unlink(x = temp_file_path, recursive = TRUE)
    if(un_res == 0){
      dir.create(temp_file_path)
    } else {
      stop("Could not create dir.")
    }
  }

  req <- httr::GET(url, httr::write_disk(path = paste0(temp_file_path, "/file.zip"), overwrite = TRUE))

  if(httr::status_code(req) != "200"){
    stop(httr::content(req, encoding = "UTF-8"))
  }

  unzip(zipfile = paste0(temp_file_path, "/file.zip"), exdir = temp_file_path)

  if(file.exists(paste0(temp_file_path, "/file.zip"))){file.remove(paste0(temp_file_path, "/file.zip"))}

  temp_file_path
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
    df_dt <- data.frame(dt = lubridate::floor_date(seq(from, to, by = "hours"), unit = "15 mins"))
    df_dt$qty <- qty

    df_dt <- dtplyr::lazy_dt(df_dt)

    df_dt <-
      df_dt %>%
      dplyr::group_by(dt) %>%
      dplyr::summarise(dt = min(dt),
                       qty = mean(qty)) %>%
      dplyr::ungroup() %>%
      dplyr::as_tibble()
  # }

  df_dt
}

