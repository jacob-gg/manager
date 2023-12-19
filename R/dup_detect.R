#' dup_detect
#'
#' Identifies all duplicated elements in an atomic vector, including "original" duplicated values.
#'
#' \code{dup_detect()} is different than \code{base::duplicated()}. If you passed \code{c(5, 5, 7)} to \code{duplicated()}, the function would identify one duplicated value (the second \code{5}).
#' But we often want to identify \emph{all} matching values in a vector of value (i.e., flag both \code{5} values in the vector above as duplicates, not just the second one).
#' \code{dup_detect()} does that.
#'
#' \code{dup_detect()} will:
#' \itemize{
#'   \item Return a logical vector (invisibly) indicating which elements are duplicated (including "original" duplicates)
#'   \item Optionally, tell you the total number of duplicated values in a vector and the number of duplicate vector elements for each duplicated value (\code{verbose} argument)
#' }
#'
#' @param test An atomic vector.
#' @param verbose \code{TRUE/FALSE} indicating whether detailed output should be printed to the console.
#' @return
#' \itemize{
#'   \item Console output with information on duplicates (\code{verbose = TRUE|FALSE}).
#'   \item A logical vector indicating which vector elements are duplicates (including "original" duplicates).
#' }
#' @examples
#' # Create 20 IPv4 addresses and use dup_detect on them:
#' set.seed(4)
#' ip_addresses <- replicate(20, paste0(sample(0:255, 4, replace = TRUE), collapse = "."))
#' duplicate <- sample(seq_along(ip_addresses), size = 3)
#' ip_addresses[duplicate + 1] <- ip_addresses[duplicate]
#' dup_detect(ip_addresses, verbose = TRUE)
#'
#' @export
dup_detect <- function(test, verbose = FALSE) {
  stopifnot('test argument must be an atomic vector.' = is.atomic(test) && is.vector(test))
  stopifnot('verbose argument must be logical' = typeof(verbose) == 'logical')

  which_duplicated <- duplicated(test)
  unique_duplicated_vals <- unique(test[which_duplicated])
  every_duplicated_val <- test %in% unique_duplicated_vals

  if (!is.na(verbose) && verbose == TRUE && length(unique_duplicated_vals) > 0) {
    cat("Total # of elements with a duplicated value: ", sum(every_duplicated_val), "\n", sep = "")
    for (i in 1:length(unique_duplicated_vals)) {
      cat(paste0('# of elements of test vector that share a value of ', unique_duplicated_vals[i], ': ', sum(test %in% unique_duplicated_vals[i]), '\n'))
    }
  }

  if (sum(which_duplicated) == 0) { warning("There are no duplicated values", call. = F) }
  every_duplicated_val
}
