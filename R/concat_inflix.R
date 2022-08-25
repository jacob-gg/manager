#' \%+\%
#'
#' String-concatenation inflix operator a la '+' in Python.
#'
#' No dependencies.
#'
#' @param ... Strings (or string-coercible objects) placed on either side of the \%+\% operator.
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
'%+%' <- function(...) {
  if (any(sapply(list(...), FUN = length) == 0)) {cat('Warning: Zero-length string detected.\n')}
  if (any(sapply(list(...), FUN = length) > 1)) {cat('Warning: Argument with length > 1 detected; output will have length > 1.\n')}
  paste(..., sep = ' ', recycle0 = FALSE)
}
