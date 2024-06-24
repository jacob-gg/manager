#' stat4DS_data
#'
#' Retrieve data sets used in \emph{Foundations of Statistics for Data Scientists With R and Python}
#' (Agresti & Kateri, 2021) for use as demo/testing data.
#'
#' @param dat A length-one character vector; the name of a data set or \code{'all'} to return names of all possible data sets.
#' @param preview If \code{TRUE}, only return the first few rows of the data set; default is \code{FALSE}.
#'
#' @return A data set if \code{dat} is a data set name; console output (and \code{NULL} invisibly) if \code{dat} is \code{'all'}.
#' @examples
#' stat4DS_data('Florida', preview = TRUE)
#' stat4DS_data('all')
#'
#' @export
stat4DS_data <- function(dat, preview = FALSE) {
  stopifnot('dat must be a character string with length of 1' =
              !is.null(dat) && is.character(dat) && length(dat) == 1)

  d <- rvest::read_html('https://stat4ds.rwth-aachen.de/data/') |>
    rvest::html_elements(css = 'a') |>
    rvest::html_text()
  d <- d[grepl('\\.(dat|csv)$', d)]

  switch(dat,
         all = {
           d_nms <- gsub('(^.+)(\\.(dat|csv)$)', '\\1', d)
           max_nchar <- max(sapply(d_nms, nchar))
           cat(sapply(1:length(d_nms),
                      \(x) sprintf(paste0(ifelse(x == 1, ' ', ''),
                                          '%-*s',
                                          ifelse(x %% 4 == 0, '\n', '')),
                                   max_nchar + 1, d_nms[x])))
         },
         {
           which_file <- grepl(ifelse(grepl('\\.(dat|csv)$', dat),
                                      dat, paste0(dat, '\\.(dat|csv)$')), d)
           stopifnot('dat must be a retrievable data set name; get_data(dat = "all" for options)' =
                       any(which_file))

           out <- utils::read.table(paste0('https://stat4ds.rwth-aachen.de/data/', d[which_file]),
                                    header = TRUE,
                                    sep = ifelse(grepl('\\.csv$', d[which_file]), ',', ''),
                                    check.names = F)

           if (preview == TRUE) {
             cat('Only showing preview; full data set is',
                 nrow(out), 'x', ncol(out), '\n\n')
             utils::head(out)
           } else { out }
         }
  )
}
