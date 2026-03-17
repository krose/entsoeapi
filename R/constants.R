.api_scheme <- "https://"
.api_domain <- "web-api.tp.entsoe.eu/"
.api_name <- "api?"
.req_timeout <- 60
.max_age <- 3600
possible_eic_chars <- stats::setNames(
  object = 0L:36L,
  nm     = c(as.character(0:9), LETTERS, "-")
)
