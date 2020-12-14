#' loch_missingness_monster
#'
#' Provides an easy-to-interpret in-console breakdown of missingness in data sets.
#'
#' \code{loch_missingness_monster} will tell you:
#' \itemize{
#'   \item The total number of missing values (NA) in the data set
#'   \item The maximum number of values that any variable is missing
#'   \item How many values each variable is missing
#'   \item Which rows are missing values in excess of a percent that you specify
#' }
#' (The output comes in sentence form if there are less or equal to than 10 variables, and table form if there are more than 10.)
#'
#' @param data A data frame
#' @param percent A percent (numeric)
#' @return \code{loch_missingness_monster} just returns output in the console
#' @examples
#' # Case with <=10 variables:
#' lmm_example <- data.frame(c(1, NA, 3, 3, 6, 3),
#'                             c(3, NA, NA, NA, NA, 6),
#'                             c(3, 7, 2, NA, NA, 6))
#' colnames(lmm_example) <- c("var1", "var2", "var3")
#' loch_missingness_monster(lmm_example, 65)
#'
#' # Case with > 10 variables:
#' set.seed <- 4
#' lmm_example2 <- replicate(12, sample(1:10, 3, replace = TRUE))
#' lmm_example2[sample(length(lmm_example2), 13, replace = FALSE)] <- NA
#' colnames(lmm_example2) <- paste0('var', 1:12, sep = '')
#' loch_missingness_monster(lmm_example2, 40)
#'
#' @export
loch_missingness_monster <- function(data, percent) {
  # Count NAs per column
  col_missing_count <- apply(is.na(data), 2, sum)

  # Report total NAs in the data
  cat("There are", sum(col_missing_count), "missing values in the dataset", "\n")

  # Report column with max missingness
  cat("The maximum number of values that any variable is missing is", max(col_missing_count), "\n \n")

  # Report variable-by-variable missingness
  if (ncol(data) <= 10) {
    for (i in 1:ncol(data)) {
      cat("The variable", colnames(data)[i], "has", col_missing_count[[i]], "missing values", "\n")
    }
  } else {
    missing_table <- data.frame(matrix(col_missing_count, nrow = 1, ncol = length(col_missing_count)))
    colnames(missing_table) <- colnames(data)
    cat("Number of missing values per variable:", "\n")
    print(missing_table, row.names = FALSE)
  }
  cat("\n")

  # Calculate row-by-row missingness and determine which rows have missingness in excess of the specified percentage
  missing_per_row <- (apply(is.na(data), 1, sum)/ncol(data))*100
  exceeds_percent <- c()
  for (p in 1:length(missing_per_row)) {
    exceeds_percent[p] <- missing_per_row[p] > percent
    if (exceeds_percent[p] == TRUE) {
      cat("Row", p, "is missing more than", percent, "percent of values", "\n")
    }
  }
}
