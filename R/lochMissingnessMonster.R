#' lochMissingnessMonster
#'
#' Provides an easy-to-interpret in-console breakdown of missingness in data sets.
#'
#' \code{lochMissingnessMonster} will tell you:
#' \itemize{
#'   \item The total number of missing values (NA) in the data set.
#'   \item The maximum number of values that any variable is missing.
#'   \item How many values each variable is missing.
#'   \item Which rows are missing values in excess of a percent that you specify.
#' }
#' (The output comes in sentence form if there are less or equal to than 10 variables; table form if there are more than 10.)
#'
#' @param data A data frame.
#' @param percent A percent (numeric).
#' @return \code{lochMissingnessMonster} just returns output in the console. That's it; this function's simple.
#' @examples
#' # Case with <=10 variables:
#' lmmSampleData1 <- data.frame(c(1, NA, 3, 3, 6, 3),
#'                             c(3, NA, NA, NA, NA, 6),
#'                             c(3, 7, 2, NA, NA, 6))
#' colnames(lmmSampleData1) <- c("var1", "var2", "var3")
#'
#' lochMissingnessMonster(lmmSampleData1, 65)
#'
#' # Case with > 10 variables:
#' set.seed <- 4
#' lMMSampleData2 <- replicate(12, sample(1:10, 3, replace = TRUE))
#' lMMSampleData2[sample(length(lMMSampleData2), 13, replace = FALSE)] <- NA
#' colnames(lMMSampleData2) <- paste0('var', 1:12, sep = '')
#'
#' lochMissingnessMonster(lMMSampleData2, 40)
#'
#' @export
lochMissingnessMonster <- function(data, percent){
  colMissingCount <- apply(is.na(data), 2, sum)
  cat("\n")
  cat("There are", sum(colMissingCount), "missing values in the dataset", "\n \n")
  cat("The maximum number of values that any variable is missing is", max(colMissingCount), "\n \n")
  if(ncol(data) <= 10) {
    for(i in 1:ncol(data)) {
      cat("The variable", colnames(data)[i], "has", colMissingCount[[i]], "missing values", "\n")
    }
  } else {
    missingTable <- data.frame(matrix(colMissingCount, nrow = 1, ncol = length(colMissingCount)))
    colnames(missingTable) <- colnames(data)
    cat("Number of missing values per variable:", "\n")
    print(missingTable, row.names = FALSE)
  }
  cat("\n")
  missingPerRow <- (apply(is.na(data), 1, sum)/ncol(data))*100
  exceedsPercent <- c()
  for (p in 1:length(missingPerRow)) {
    exceedsPercent[p] <- missingPerRow[p] > percent
    if(exceedsPercent[p] == TRUE) {
      cat("Row", p, "is missing more than", percent, "percent of values", "\n")
    }
  }
}
