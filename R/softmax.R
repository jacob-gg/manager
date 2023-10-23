#' softmax
#'
#' Calculates the softmax function for mapping a set of real values to a probability distribution.
#' The maxima/um receives quite a bit of weight via softmax: Given the values \code{[4, 4, 4, 5]}, softmax
#' returns a probability of .475 for the final element and 0.175 for each of the first three.
#'
#' @param x A double or integer vector.
#' @param digits The number of digits to round the output probabilities to.
#' @return A length-one vector.
#' @examples
#' x <- c(3.5, 4.0, 6.5)
#' softmax(x)
#' softmax(x, digits = 4)
#'
#' x <- c(NA, 3.5, 4.0, 6.5)
#' softmax(x, 4)
#'
#' @export
softmax <- function(x, digits = NULL) {
  if (typeof(x) %in% c('double', 'integer') == FALSE) {
    stop('x must be double or integer', call. = FALSE)
  }
  if (!is.null(digits) && typeof(digits) %in% c('double', 'integer') == FALSE) {
    stop('digits must be double or integer', call. = FALSE)
  }
  if (any(is.na(x))) {
    x <- x[is.na(x) == FALSE]
    warning('>=1 NA found and removed from x before calculating softmax', call. = FALSE)
  }
  out <- exp(x) / sum(exp(x))
  if (!is.null(digits)) {
    round(out, digits = digits)
  } else {
    out
  }
}
