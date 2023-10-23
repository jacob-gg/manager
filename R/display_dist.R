#' display_dist
#'
#' Displays the approximate shape of a distribution in the console (or other i/o system) using Unicode Block Elements glyphs.
#'
#' \code{display_dist()} is motivated by \code{precis::histospark()} (Hadley Wickham) and \code{rethinking::precis()} (Richard McElreath).
#' It displays a slightly more-granular distribution than those functions, and the peak(s) of the distribution is easily identified via the
#' use of a distinctive Block Elements glyph.
#'
#' @param x A numeric vector (double or integer).
#' @param width The number of blocks comprising the printed distribution.
#' @return Console output.
#' @examples
#' # Normal
#' set.seed(99)
#' x <- rnorm(1000, 0, 1)
#' display_dist(x, 10)
#'
#' # Poisson
#' set.seed(99)
#' x <- rpois(1000, lambda = 6)
#' display_dist(x, 10)
#'
#' @export
display_dist <- function(x, width = 10) {
  if (typeof(x) %in% c('double', 'integer') == FALSE) {
    stop('x must be double or integer', call. = FALSE)
  }
  blocks <- c('\u2581', '\u2582', '\u2583', '\u2584', '\u2585', '\u2586', '\u2587', '\u2593')
  hist_ <- graphics::hist(x, breaks = width, plot = FALSE)
  breaks <- seq(0, 1, length = length(blocks) + 1)
  bars <- cut(hist_$counts / max(hist_$counts), breaks = breaks, labels = blocks, include.lowest = TRUE)
  paste0(bars, collapse = '')
}
