#' golem
#'
#' Retrieves geolocation data for IP addresses from http://freegeoip.live.
#'
#' \code{golem()} will retrieve the country, region, city, zip, time code, latitude, longitude, and metro code for IP addresses you give it.
#'
#' Requires: \code{stringi} and \code{magrittr} (pipe)
#'
#' @param ip A vector one or more IP addresses
#' @return A data frame with geolocation data.
#' @examples
#' ips <- c('68.38.164.93', '192.198.85.59', '73.32.208.180', '2604:3d08:1580:2d0:1cd4:dafd:f848:4fb9')
#' ip_dat <- golem(ips)
#'
#' @importFrom magrittr %>%
#'
#' @export
golem <- function(ip) {
  # Checks
  if ('stringi' %in% utils::installed.packages() == FALSE) {
    stop('Error: stringi not installed; use install.packages("stringi")')
  }
  if ('magrittr' %in% utils::installed.packages() == FALSE) {
    stop('Error: stringi not installed; use install.packages("magrittr")')
  }

  dat <- data.frame()

  # Retrieve data
  start <- Sys.time()
  for (i in 1:length(ip)) {
    rowDat <- readLines(paste0('http://freegeoip.live/csv/', ip[i])) %>%
      stringi::stri_split(fixed = ',') %>%
      unlist() %>%
      t()
    dat <- rbind(dat, rowDat)
  }
  fin <- Sys.time()
  if (length(ip) > 1) {
    cat('Retrieving geolocation data for these', length(ip), 'IP addresses took', round(fin - start, digits = 2), 'seconds', '\n')
  } else {
    cat('Retrieving geolocation data for this IP address took', round(fin - start, digits = 2), 'seconds', '\n')
  }

  # Structure data
  colnames(dat) <- c('ip', 'country_code', 'country_name', 'region_code', 'region_name', 'city',
                     'zip', 'time_code', 'latitude', 'longitude', 'metro_code')
  dat[, c('latitude', 'longitude')] <- apply(dat[, c('latitude', 'longitude')], 2, function(x) as.numeric(as.character(x)))

  invisible(dat)
}
