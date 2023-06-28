#' loch_missingness_monster
#'
#' Provides an easy-to-interpret in-console breakdown of missingness in rectangular data (data frames and matrices).
#'
#' \code{loch_missingness_monster()} tells you:
#' \itemize{
#'   \item The total number of missing values (\code{NA}) in the data set.
#'   \item A table indicating the count and percent of missing values per column.
#'   \item Optionally, the number of rows missing values in excess of a percent that you specify.
#' }
#' (The output comes in sentence form if there are less or equal to than 10 variables; it comes in table form if there are more than 10.)
#'
#' @param dat A data frame or matrix.
#' @param percent A number [0, 100] (optional).
#' @return Text output.
#'
#' @examples
#' lmm_example <- replicate(12, sample(1:10, 3, replace = TRUE))
#' lmm_example[sample(length(lmm_example), 13, replace = FALSE)] <- NA
#' colnames(lmm_example) <- paste0('var', 1:12, sep = '')
#' loch_missingness_monster(lmm_example, 40)
#'
#' @export
loch_missingness_monster <- function(dat, percent = NA) {
  stopifnot('dat must be a data frame or matrix' = (is.data.frame(dat) | is.matrix(dat)))
  stopifnot('percent argument must be [0, 100] or NA' = ((percent >= 0 & percent <= 100) | is.na(percent)))

  # Column-wise NA counts
  col_NA_count <- apply(is.na(dat), 2, sum)

  # Table of column-wise missingness
  missing_table <- rbind(data.frame(t(col_NA_count)), round(col_NA_count / nrow(dat), digits = 2))
  rownames(missing_table) <- c('NA count:', 'NA percent:')

  # Prints
  cat("Total NA in data set:", sum(col_NA_count), "\n\nMissingness table:\n")
  print(missing_table)
  cat('\n')

  # If a percent argument is given, calculate row-wise missingness and ID rows with missingness in excess of `percent`
  if (is.na(percent) == FALSE) {
    percent_NA_by_row <- (apply(is.na(dat), 1, sum) / ncol(dat)) * 100
    exceeds_percent <- sum(percent_NA_by_row > percent)
    cat(exceeds_percent, ifelse(exceeds_percent > 1, 'rows are', 'row is'), 'missing more than', percent, 'percent of values\n')
  }
}
