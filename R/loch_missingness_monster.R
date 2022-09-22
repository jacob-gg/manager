#' loch_missingness_monster
#'
#' Provides an easy-to-interpret in-console breakdown of missingness in data sets.
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
#' @param percent A number (optional).
#' @return \code{loch_missingness_monster()} just returns output in the console.
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
  cat("Total NA in data set:", sum(col_NA_count), "\n\n")

  # Table of column-wise missingness
  cat("Missingness table:\n")
  missing_table <- data.frame(t(col_NA_count))
  missing_table <- rbind(missing_table, round(col_NA_count / nrow(dat), digits = 3))
  rownames(missing_table) <- c('NA count:', 'NA percent:')
  print(missing_table)
  cat('\n')

  # If a percent argument is given, calculate row-by-row missingness and determine which rows have missingness in excess of the specified percentage
  if (is.na(percent) == F) {
    percent_na_per_row <- (apply(is.na(dat), 1, sum) / ncol(dat)) * 100
    exceeds_percent <- percent_na_per_row > percent
    cat(sum(exceeds_percent), ifelse(sum(exceeds_percent) > 1, 'rows are', 'row is'),
                                     'missing more than', percent, 'percent of values.\n')
  }
}
