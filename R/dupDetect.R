#' dupDetect
#'
#' Identifies rows in a data set with duplicate values for a given column (e.g., survey responses with the same IP address, which may pose a bot risk) and aids in removing them.
#'
#' \code{dupDetect} is more than \code{base::duplicated}. If you fed a vector with the values \code{c(5, 5, 7)} to \code{duplicated}, the function would identify one duplicated value (just the second five). But for the purposes of bot detection and data wrangling, we often want to identify \emph{all} matching values in, say, a column of IP addresses (i.e., identify both 5s in the vector above as duplicates, not just the second one).
#'
#' \code{dupDetect} will:
#' \itemize{
#'   \item Tell you the total number of duplicated values in a data frame column (/vector) you identify (e.g., IP addresses; latitude; longitude).
#'   \item Tell you which rows (/vector elements) match each specific duplicated value.
#'   \item If you wish, produce a global variable, (\code{everyDuplicatedRow}), indexing rows (/vector elements) with duplicated values, which you can use to easily drop those from the data frame or vector. Use \code{save = T} or \code{save = F} to indicate whether you want a global variable saved to your environment (T is default).
#' }
#'
#' @param testCol A vector (generally a column from a data frame).
#' @param save \code{T} or \code{F} (default is \code{T})
#' @return
#' \itemize{
#'   \item Console output with a breakdown of duplicativeness.
#'   \item \code{everyDuplicatedRow}: If desired, a vector saved in the global environment indexing duplicated rows (/vector elements).
#' }
#' @examples
#' # Create 1000 IPv4 addresses and use dupDetect on them:
#' set.seed(4)
#' IP_draw <- 0:255
#' IP_addresses <- NA
#' for(i in 1:1000) {
#'   IP_addresses[i] <- paste0(sample(IP_draw, 4, replace = TRUE), collapse = ".")
#' }
#' IP_addresses <- as.factor(IP_addresses)
#' IP_addresses[sample(1:1000, 10, replace = F)] <- sample(IP_addresses, 10, replace = F)
#'
#' dupDetect(IP_addresses)
#'
#' @export
dupDetect <- function(testCol, save = T) {
  duplicatedRows <- which(duplicated(testCol))
  if(length(duplicatedRows) == 0) {stop("Function halted. There are no duplicated values. Congrats!")}
  duplicatedValues <- unique(testCol[duplicatedRows])
  if(save == T){
    everyDuplicatedRow <<- which(testCol %in% duplicatedValues)
  } else {
    everyDuplicatedRow <- which(testCol %in% duplicatedValues)
  }
  dupRowString <- NA
  cat("\n")
  cat("Total # of rows with a duplicate value for the specified variable: ", length(everyDuplicatedRow), "\n", sep = "")
  for(i in 1:length(duplicatedValues)) {
    dupRowString[i] <- capture.output(cat(which(testCol %in% duplicatedValues[i]), sep = ", "))
    cat("The following rows share the value of ", as.character(duplicatedValues[i]), ": ", dupRowString[i], "\n", sep = "")
  }
  cat("(Variable type: ", class(testCol), ")", "\n", sep = "")
}
