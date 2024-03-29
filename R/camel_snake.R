#' to_snake
#'
#' Converts a vector of strings (e.g., column names) from camelCase to snake_case.
#'
#' \code{to_snake()} will return the input as a character vector with a warning
#' unless there is at least one capital letter not in the first position somewhere
#' in the set of input strings.
#'
#' E.g., \code{to_snake(c('aaa', 'bbb', 'Ccc'))} will return \code{x} and a warning.
#'
#' No dependencies.
#'
#' @param x An atomic vector (character or coercible to character).
#' @param nums_to_snake Should numbers be snaked as well? ('z1' --> 'z_1')
#' @return A vector.
#' @examples
#' camels <- c('dromedaryCamel', 'wildBactrianCamel', 'BactrianLookingUp',
#' 'aCaravanOf10Camels', 'aLargerCaravanOf11')
#' to_snake(camels)
#'
#' @export
to_snake <- function(x, nums_to_snake = T) {
  if ((is.atomic(x) && is.vector(x)) == FALSE) {
    stop('x must be an atomic vector')
  }
  if (typeof(x) != 'character') {
    x <- as.character(x)
  }

  # Check presence of capitals in positions other than first
  if (length(grep('(?<!^)([A-Z])', x, perl = T)) == 0) {
    warning('No camel humps found. Returning input as character vector.', call. = FALSE)
    return(x)
  }

  # Determine regex based on whether numbers should be snaked as well
  pattern <- ifelse(nums_to_snake == T,
                    '(?<!^)([A-Z]|\\d{1,})',
                    '(?<!^)([A-Z])')

  # Insert _'s and convert to lowercase
  gsub(pattern,  '_\\L\\1', x, perl = T)
}

#' to_camel
#'
#' Converts a vector of strings (e.g., column names) from snake_case to camelCase. (Specifically, lower camel case.)
#'
#' \code{to_camel()} will return the input as a character vector with a warning
#' unless there is at least one underscore not in the first position somewhere
#' in the set of input strings.
#'
#' E.g., \code{to_camel(c('aaa', 'bbb', '_ccc'))} will return \code{x} and a warning.
#'
#' No dependencies.
#'
#' @param x An atomic vector (character or coercible to character).
#' @param nums_to_camel Should numbers be cameled as well? ('z_1_a' --> 'z1A')
#' @return A vector.
#' @examples
#' snakes <- c('rainbow_boa', 'blue_racer', 'dragon_snake',
#' 'a_set_of_5_garter_snakes', 'a_larger_set_of_6_kingsnakes')
#' to_camel(snakes)
#'
#' @export
to_camel <- function(x, nums_to_camel = T) {
  if ((is.atomic(x) && is.vector(x)) == FALSE) {
    stop('x must be an atomic vector')
  }
  if (typeof(x) != 'character') {
    x <- as.character(x)
  }

  # Check presence of underscores in positions other than the first
  if (length(grep('(?<!^)(_)', x, perl = T)) == 0) {
    warning('No snake segments found. Returning input as character vector.', call. = FALSE)
    return(x)
  }

  # Determine regex based on whether numbers should be cameled as well
  pattern <- ifelse(nums_to_camel == T,
                    '(?<!^)(_{1,})([a-z]|[A-Z]|\\d{1,})',
                    '(?<!^)(_{1,})([a-z]|[A-Z])')

  # Camel
  gsub(pattern, '\\U\\2', x, perl = T)
}

