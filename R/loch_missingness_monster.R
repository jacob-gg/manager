#' loch_missingness_monster
#'
#' Provides an easy-to-interpret in-console breakdown of missingness in rectangular data (data frames and matrices).
#'
#' \code{loch_missingness_monster()} provides:
#' \itemize{
#'   \item The total number of missing values (\code{NA}) in the data set.
#'   \item A table indicating the count and percent of missing values per column.
#'   \item Optionally, the number and indexes of rows missing values in excess of a percent that you specify.
#' }
#'
#' @param dat A data frame or matrix.
#' @param percent A number [0, 100] (optional).
#'
#' @return Text output in the console and an invisible list. If \code{percent}
#' is \code{NULL}, the list has a length of one and contains a missingness
#' table; if \code{percent} is given, the list has a length of two, and the
#' second element is a vector of the indexes of rows with missingness in excess
#' of \code{percent} (\code{integer(0)} is no rows meet that criterion).
#'
#' @examples
#' dat <- data.frame(x = c(1, 2, 3, NA, NA),
#'                   y = c(1, 2, 3, 4, NA),
#'                   z = c(1, NA, NA, NA, 5))
#' loch_missingness_monster(dat, 50)
#'
#' @export
loch_missingness_monster <- function(dat, percent = NULL) {
  stopifnot('dat must be a data frame or matrix' = inherits(dat, 'data.frame') || inherits(dat, 'matrix'),
            'percent argument must be NULL or [0, 100]' = is.null(percent) || (length(percent) == 1 && (percent >= 0 & percent <= 100)))

  # Column-wise NA counts
  col_NA_count <- apply(is.na(dat), 2, sum)

  # Table of column-wise missingness
  missing_table <- rbind(data.frame(t(col_NA_count)), round(col_NA_count / nrow(dat), digits = 2))
  rownames(missing_table) <- c('NA count:', 'NA percent:')
  out <- list('missing_table' = missing_table)

  # Print result
  cat("Total NA in data set:", sum(col_NA_count), "\n\nMissingness table:\n")
  print(missing_table)
  cat('\n')

  # If percent is given, calculate row-wise missingness and identify rows with missingness > percent
  if (!is.null(percent)) {
    percent_NA_by_row <- (apply(is.na(dat), 1, sum) / ncol(dat)) * 100
    exceeds_percent <- sum(percent_NA_by_row > percent)
    cat(exceeds_percent, 'rows are missing more than', percent, 'percent of values\n')
    out$rows_exceeding_percent <- which(percent_NA_by_row > percent)
  }

  invisible(out)
}
