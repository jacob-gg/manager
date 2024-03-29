% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/concat_infix.R
\name{\%+\%}
\alias{\%+\%}
\title{\%+\%}
\usage{
lhs \%+\% rhs
}
\arguments{
\item{lhs}{String (or string-coercible object) to be concatenated with the rhs (right-hand side).}

\item{rhs}{String (or string-coercible object) to be concatenated with the lhs (left-hand side).}
}
\value{
A character vector with length equal to the maximum length of the inputs.
}
\description{
String-concatenation infix operator a la '+' in Python.
}
\details{
No dependencies.
}
\examples{
\dontrun{
'aaa ' \%+\% 'bbb ' \%+\% 'ccc ' \%+\% 'ddd'
# Zero-length inputs are concatenated in the resulting string as ''
'aaa ' \%+\% 'bbb ' \%+\% 'ccc ' \%+\% 'ddd'[F]
# If an input has length > 1, the result has length > 1 and a warning is given
c('aaa ', 'bbb ') \%+\% 'zzz'
}

}
