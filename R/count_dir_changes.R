#' count_dir_changes
#'
#' Count the number of direction changes in a vector.
#'
#' \code{count_dir_changes()} returns the number of direction changes in a
#' vector. For example given \code{c(1, 5, 0, NA, -10, -1, NA)}, the function
#' returns \code{2} (from 1-->5 to 5-->0 and from 0-->-10 to -10-->-1. \code{NA}
#' values are removed prior to counting.
#'
#' @param x The vector in which to count direction changes.
#' @param verbose if \code{TRUE}, report the number of \code{NA} values removed from \code{x} prior to counting.
#'
#' @return A numeric vector with a length of one.
#'
#' @examples
#' count_dir_changes(c(1, 5, 0, NA, -10, -1, NA))
#' count_dir_changes(c(1, 1, 2, 2, 3, 3))
#'
#' @export
count_dir_changes <- function(x, verbose = FALSE) {
  stopifnot('x must be a numeric atomic vector' =
              is.atomic(x) && is.numeric(x),
            'verbose must be length=1 and TRUE or FALSE' =
              length(verbose) == 1 &&
              verbose %in% c(TRUE, FALSE))

  if (any(is.na(x))) {
    if (verbose) {
      cat('Removing', sum(is.na(x)), 'NA values from vector before computing\n')
    }
    x <- x[!is.na(x)]
  }

  # Determine directions of differences between sequential values
  directions <- diff(x) |> sign()
  # Trim zeros (indicating unchanging sequential values)
  directions <- directions[directions != 0]
  # Take differences of directions; -2 and 2 indicate direction changes
  direction_changes <- diff(directions)
  sum(direction_changes %in% c(-2, 2))
}
