#' dup_detect
#'
#' Identifies all duplicated elements in vector, including "original" duplicated values.
#'
#' \code{dup_detect()} is different than \code{base::duplicated()}. If you fed \code{c(5, 5, 7)} to \code{duplicated()}, the function would identify one duplicated value (the second five).
#' But for the purposes of bot detection and data wrangling, we often want to identify \emph{all} matching values in, say, a column of IP addresses (i.e., identify both 5s in the vector above as duplicates, not just the second one).
#' \code{dup_detect()} does that.
#'
#' \code{dup_detect()} will:
#' \itemize{
#'   \item Tell you the total number of duplicated values in a vector (e.g., IP addresses; latitudes; longitudes).
#'   \item Optionally, tell you which vector elements match each specific duplicated value (\code{verbose} argument)
#'   \item Return a vector (invisibly) with the indexes of every duplicated element (including "original" duplicates)
#' }
#'
#' @param test_col A vector.
#' @param verbose \code{T/F} indicating whether detailed output should be printed to the console.
#' @return
#' \itemize{
#'   \item Console output with information on duplicates (\code{verbose = T/F}).
#'   \item \code{every_duplicated_row}: Invisible vector indexing duplicate vector elements (including "original" duplicates).
#' }
#' @examples
#' # Create 100 IPv4 addresses and use dup_detect on them:
#' set.seed(4)
#' ip_draw <- 0:255
#' ip_addresses <- vector(length = 100)
#' for(i in 1:100) {
#'   ip_addresses[i] <- paste0(sample(ip_draw, 4, replace = TRUE), collapse = ".")
#' }
#' ip_addresses[sample(1:100, 10, replace = FALSE)] <- sample(ip_addresses, 10, replace = FALSE)
#' dup_detect(ip_addresses)
#'
#' @export
dup_detect <- function(test_col, verbose = T) {
  stopifnot('test_col should be a vector.' = is.vector(test_col))

  # Identify rows containing duplicated values
  duplicated_rows <- which(duplicated(test_col))

  # Halt if no duplicates
  if (length(duplicated_rows) == 0) {stop("There are no duplicated values; function halted.", call. = F)}

  # Identify duplicated values
  duplicated_values <- unique(test_col[duplicated_rows])

  # Produce global variable depending on save = T|F
  every_duplicated_row <- which(test_col %in% duplicated_values)

  # Format and generate output
  cat("Total # of elements with a duplicated value: ", length(every_duplicated_row), "\n", sep = "")
  if (verbose == T) {
    for (i in 1:length(duplicated_values)) {
      dup_row_string <- paste0(which(test_col %in% duplicated_values[i]), collapse = ", ")
      cat("The following elements share the value of ", as.character(duplicated_values[i]), ": ", dup_row_string, "\n", sep = "")
    }
  }

  # Identify and output variable class
  cat("(Variable type: ", class(test_col), ")", "\n", sep = "")

  invisible(every_duplicated_row)
}
