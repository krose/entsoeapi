.api_scheme <- "https://"
.api_domain <- "web-api.tp.entsoe.eu/"
.api_name <- "api?"
.req_timeout <- 60
.max_age <- 3600
.pd_scheme <- "https://"
.pd_domain <- "eepublicdownloads.blob.core.windows.net"
.pd_alloc_eic <- "cio-lio/xml/allocated-eic-codes.xml"
.pd_csv_eic <- "/cio-lio/csv/"
.feed_url <- "https://external-api.tp.entsoe.eu/news/feed"
.test_token <- "497dcba3-ecbf-4587-a2dd-5eb0665e6880"
possible_eic_chars <- stats::setNames(
  object = 0L:36L,
  nm     = c(as.character(0:9), LETTERS, "-")
)
user_agent_string <- "entsoeapi (https://krose.github.io/entsoeapi/)"
