#' gen_monotonic
#'
#' Generate increasing or decreasing monotonic data.
#'
#' No dependencies.
#'
#' @param n The number of monotonic values to generate.
#' @param increasing Should data monotonically increase? \code{TRUE} (default) or \code{FALSE}.
#' @param start The starting value of the output vector.
#' @param step_change_min The minimum change between values.
#' @param step_change_max The maximum change between values.
#' @param round_to The number of digits to round output values to. \code{NULL} (default) or a number.
#'
#' @return A numeric vector of length \code{n}.
#'
#' @examples
#' gen_monotonic(n = 5, step_change_min = .5, step_change_max = .5)
#' gen_monotonic(n = 100, increasing = FALSE, round_to = 2)
#'
#' @export
gen_monotonic <- function(n, increasing = TRUE, start = 0,
                          step_change_min = 0, step_change_max = 1,
                          round_to = NULL) {
  stopifnot('n must be length=1, numeric, and >=2' =
              length(n) == 1 && is.numeric(n) && n >= 2,
            'increasing must be length=1 and TRUE or FALSE' =
              length(increasing) == 1 && increasing %in% c(TRUE, FALSE),
            'start must be length=1 and numeric' =
              length(start) == 1 && is.numeric(start),
            'step_change_min must be length=1, numeric, and >=0' =
              length(step_change_min) == 1 && is.numeric(step_change_min) && step_change_min >= 0,
            'step_change_max must be length=1, numeric, and >0' =
              length(step_change_max) == 1 && is.numeric(step_change_max) && step_change_max >= 0,
            'step_change_min must be <= step_change_max' =
              step_change_min <= step_change_max,
            'if non-null, round_to must be length=1 and numeric' =
              is.null(round_to) || (length(round_to) == 1 && is.numeric(round_to)))

  mult <- ifelse(increasing, 1, -1)
  step_changes <- mult*stats::runif(n-1, step_change_min, step_change_max)
  x <- cumsum(c(start, step_changes))

  if (is.null(round_to)) return(x) else round(x, digits = round_to)
}
