#' @title
#' An R Interface to the ENTSO-E Transparency Platform API
#'
#' @description
#' an easy wrapper around the ENTSO-E
#' [API](https://documenter.getpostman.com/view/7009892/2s93JtP3F6)'s data and
#' transform them to tabular format without effort.
#'
#' @section Options:
#' \describe{
#'   \item{`entsoeapi.max_parallel`}{Maximum number of API requests run
#'     concurrently when a query is split into several requests (offset
#'     pagination, day-by-day queries). Default `4`; set it to `1` for
#'     strictly sequential requests. Requests are additionally throttled to
#'     300 per minute to stay under the platform's rate limit.}
#'   \item{`entsoeapi.cache_dir`}{Directory of a persistent cache for the
#'     downloaded EIC code tables, e.g.
#'     `tools::R_user_dir("entsoeapi", "cache")`. Default `NULL`: the tables
#'     are cached in memory for one hour and downloaded again in every new
#'     R session.}
#'   \item{`entsoeapi.cache_max_age`}{Lifetime of the disk cache entries in
#'     seconds. Default `86400` (one day).}
#' }
#'
#' @name entsoeapi
#' @aliases entsoeapi-package
#' @keywords internal
NULL
