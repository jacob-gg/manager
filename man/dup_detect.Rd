% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dup_detect.R
\name{dup_detect}
\alias{dup_detect}
\title{dup_detect}
\usage{
dup_detect(test_col, save = T)
}
\arguments{
\item{test_col}{A vector (generally a column from a data frame).}

\item{save}{\code{T} or \code{F} (default is \code{T})}
}
\value{
\itemize{
  \item Console output with a breakdown of duplicativeness.
  \item \code{every_duplicated_row}: If desired, a vector saved in the global environment indexing duplicated rows (/vector elements).
}
}
\description{
Identifies rows in a data set with duplicate values for a given column (e.g., survey responses with the same IP address, which may pose a bot risk) and aids in removing them.
}
\details{
\code{dup_detect} is more than \code{base::duplicated}. If you fed a vector with the values \code{c(5, 5, 7)} to \code{duplicated}, the function would identify one duplicated value (just the second five). But for the purposes of bot detection and data wrangling, we often want to identify \emph{all} matching values in, say, a column of IP addresses (i.e., identify both 5s in the vector above as duplicates, not just the second one).

\code{dup_detect} will:
\itemize{
  \item Tell you the total number of duplicated values in a data frame column (/vector) you identify (e.g., IP addresses; latitude; longitude).
  \item Tell you which rows (/vector elements) match each specific duplicated value.
  \item If you wish, produce a global variable, (\code{every_duplicated_row}), indexing rows (/vector elements) with duplicated values, which you can use to easily drop those from the data frame or vector. Use \code{save = T} or \code{save = F} to indicate whether you want a global variable saved to your environment (T is default).
}
}
\examples{
# Create 1000 IPv4 addresses and use dup_detect on them:
set.seed(4)
ip_draw <- 0:255
ip_addresses <- NA
for(i in 1:1000) {
  ip_addresses[i] <- paste0(sample(ip_draw, 4, replace = TRUE), collapse = ".")
}
ip_addresses <- as.factor(ip_addresses)
ip_addresses[sample(1:1000, 10, replace = F)] <- sample(ip_addresses, 10, replace = F)
dup_detect(ip_addresses)

}