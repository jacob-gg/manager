#' Regex pattern for identifying non-person names
#'
#' A regex pattern that matches names that are likely to be non-persons (e.g., businesses, educational institutions, government entities, etc.)
#'
#' User \code{check_regex()} to check \code{non_person_regex} against a test data set, \code{test_names}.
#'
#' @format A character string
#'
#' @source Created by github.com/jacob-gg
'non_person_regex'

#' Set of names for testing the efficacy of \code{non_person_regex}
#'
#' A data frame containing a mix of non-person names (e.g., "Downtown Books," "Supreme Vacuums, LLC") and individuals' names that can be used to test the efficacy of the regex pattern \code{non_person_regex}.
#'
#' @format A data frame with 232 rows and 2 variables:
#' \describe{
#'   \item{\code{name}}{The name}
#'   \item{\code{type}}{The type of name, non-person or person}
#' }
#' @source Created by github.com/jacob-gg
'test_names'

#' check_regex
#'
#' Checks efficacy of \code{non_person_regex} in flagging non-person names.
#'
#' \code{check_regex} checks whether the pattern contained in \code{non_person_regex} successfully tags all of the non-person names in the \code{test_names} dataset as non-persons while skipping individuals' "distraction" names.
#'
#' Requires: \code{stringi}
#'
#' @param ... no arguments needed
#' @return Output in the console describing the results of the check.
#' @examples
#' check_regex()
#'
#' @export
check_regex <- function(...) {
  matched <- stringi::stri_detect(manager::test_names$name, regex = manager::non_person_regex)
  cat('non_person_regex has been passed to stringi::stri_detect() to comb through the test_names dataset, yielding the following:\n',
    'Successfully matching:', paste0(sum(matched), '/',
                                       nrow(manager::test_names[manager::test_names$type == 'non-person', ])),
      'non-person names\n',
      'Successfully skipping:', paste0(sum(matched == F), '/',
                                       nrow(manager::test_names[manager::test_names$type == 'person', ])),
      "individuals' names\n",
      'Unmatched by the pattern:', paste0(manager::test_names$name[!matched], collapse = ', '), '\n',
    'Call non_person_regex to see the pattern or use View(test_names) to see the test dataset.')
}
