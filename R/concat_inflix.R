#' \%+\%
#'
#' String-concatenation inflix operator a la '+' in Python.
#'
#' No dependencies.
#'
#' @param lhs String (or string-coercible object) to be concatenated with the rhs (right-hand side).
#' @param rhs String (or string-coercible object) to be concatenated with the lhs (left-hand side).
#'
#' @return A character vector with length equal to the maximum length of the inputs.
#' @examples
#' \dontrun{
#' 'aaa' %+% 'bbb' %+% 'ccc' %+% 'ddd'
#' # Zero-length inputs are concatenated in the resulting string as ''
#' 'aaa' %+% 'bbb' %+% 'ccc' %+% 'ddd'[F]
#' # If an input has length > 1, the result has length > 1 and a warning is given
#' c('aaa', 'bbb') %+% 'zzz'
#' }
#'
#' @export
'%+%' <- function(lhs, rhs) {
  if (any(sapply(list(lhs, rhs), FUN = length) == 0)) {cat('Warning: Zero-length string detected.\n')}
  if (any(sapply(list(lhs, rhs), FUN = length) > 1)) {cat('Warning: Argument with length >1 detected; output has length >1.\n')}
  paste(lhs, rhs, sep = ' ', recycle0 = FALSE)
}
