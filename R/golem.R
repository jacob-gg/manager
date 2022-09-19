#' golem
#'
#' Retrieves geolocation data for IP addresses from http://freegeoip.live and returns results as a data frame.
#'
#' \code{golem()} will retrieve the country, region, city, zip, time code, latitude, longitude, and metro code for IP addresses.
#'
#' Requires: \code{magrittr} (pipe)
#'
#' @param ip A vector one or more IP addresses.
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
  stopifnot(exprs = {
    'magrittr' %in% utils::installed.packages()
    is.vector(ip)
    }
  )

  # Retrieve data
  dat <- data.frame(matrix(nrow = length(ip), ncol = 11))
  start <- Sys.time()
  for (i in 1:length(ip)) {
    row_dat <- readLines(paste0('http://freegeoip.live/csv/', ip[i])) %>%
      strsplit(x = ., split = ',', fixed = T) %>%
      unlist() %>%
      t()
    dat[i, ] <- row_dat
  }
  fin <- Sys.time()

  cat('Retrieving geolocation data for', ifelse(length(ip) == 1, 'this IP address', 'these IP addresses'),
      'took', round(fin - start, digits = 2), 'seconds\n')

  # Structure data
  colnames(dat) <- c('ip', 'country_code', 'country_name', 'region_code', 'region_name',
                     'city', 'zip', 'time_code', 'latitude', 'longitude', 'metro_code')
  dat[, c('latitude', 'longitude')] <- apply(dat[, c('latitude', 'longitude')], 2, function(x) as.numeric(as.character(x)))

  dat
}
