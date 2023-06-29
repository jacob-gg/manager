#' winograd
#'
#' Each time \code{winograd()} is run, it return the text of one Winograd schema from the following link (website created/maintained by Ernest Davis; available under a CC 4.0 license): https://cs.nyu.edu/~davise/papers/WinogradSchemas/WSCollection.html
#'
#' A Winograd schema is a sentence (really, a pair of possible sentences) structured such that there's an ambiguous pronoun that could be associated with either of two antecedent nouns. Which antecedent noun the pronoun is tied to depends on which of two words/phrases is swapped in elsewhere in the sentence. For example:
#'
#' \emph{I spread the cloth on the table in order to [protect/display] it.}
#'
#' \emph{[I]t} is the ambiguous pronoun: If the sentence is written with the word "protect," the pronoun refers to the table; if the sentence is written with the word "display," the pronoun refers to the cloth.
#'
#' Winograd schemas can be used to check for or try to foil bots. Simply pick one construction of the sentence (e.g., "...to protect it" or "...to display it") and ask a respondent to identify the pronoun (e.g., "What is being [protected][displayed]?").
#'
#' @param seed A seed to determine which Winograd schema is selected; omit for random selection based on \code{Sys.time()}
#' @examples
#' winograd() # random selection
#' winograd(seed = 10) # repeatable selection
#'
#' @export
winograd <- function(seed = Sys.time()) {
  if (inherits(seed, 'numeric') & is.na(seed) == FALSE) {
    set.seed(seed)
  }

  # Pull in Winograds
  wino_html <- rvest::read_html('https://cs.nyu.edu/~davise/papers/WinogradSchemas/WSCollection.html')
  wino_nodes <- rvest::html_nodes(wino_html, 'li')
  wino_nodes_text <- rvest::html_nodes(wino_nodes, 'text')

  # Remove Winograds with spaghetti HTML, pick one, and get its text
  exclude <- which(unlist(lapply(lapply(wino_nodes_text, function(x) rvest::html_text(x, 'text')), nchar)) > 500)
  wino_num <- sample((1:length(wino_nodes_text))[-exclude], 1)
  wino_text <- rvest::html_text(wino_nodes_text[[wino_num]], 'text')

  # Clean text
  wino_text <- gsub('\n', ' ', wino_text)
  wino_text <- gsub('\\[', ' \\[', wino_text)
  wino_text <- gsub('\\]', '\\] ', wino_text)
  wino_text <- gsub('(\\])(\\s{1,5})(\\.)', '].', wino_text)
  wino_text <- gsub('\\s{2,5}', ' ', wino_text)
  wino_text <- gsub('^\\s*|\\s*$', '', wino_text)
  if (grepl('\\.$', wino_text) == F) {
    wino_text <- paste0(wino_text, '.')
  }

  wino_text
}
