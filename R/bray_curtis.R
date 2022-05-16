#' bray_curtis
#'
#' Calculate the Bray-Curtis dissimilarity index (or Sorensen-Dice similarity index) between two sites (with site compositions given as vectors).
#'
#' No dependencies.
#'
#' @param site1 A vector containing a set of observations (the observations at the first site).
#' @param site2 A vector containing a set of observations (the observations at the second site).
#' @param sorensen_dice If TRUE, the Sørensen-Dice similarity index (one minus Bray-Curtis dissimilarity index) is returned (FALSE by default)
#'
#' @return A number [0, 1].
#' @examples
#' area1 <- c('bubble-tip anemone', 'bubble-tip anemone', 'beaded sea anemone', 'sebae anemone', 'magnificent sea anemone')
#' area2 <- c('bubble-tip anemone', 'bubble-tip anemone', 'bubble-tip anemone', 'corkscrew tentacle sea anemone')
#' bray_curtis(area1, area2)
#'
#' @export
bray_curtis <- function(site1, site2, sorensen_dice = FALSE) {
  if (is.vector(site1) == FALSE | is.vector(site2) == FALSE) {stop('`site1` and `site2` must be supplied as vectors.')}
  if (sorensen_dice %in% c(TRUE, FALSE) == FALSE) {warning('sorensen_dice is not TRUE or FALSE; printing Bray-Curtis dissimilarity index by default.', call. = FALSE)}

  if (any(is.na(site1))) {
    n_na <- sum(is.na(site1))
    site1 <- site1[is.na(site1) == FALSE]
    cat(n_na, ifelse(n_na == 1, 'NA', 'NAs'), 'found in the `site1` data and ignored during calculation.\n')
  }
  if (any(is.na(site2))) {
    n_na <- sum(is.na(site2))
    site2 <- site2[is.na(site2) == FALSE]
    cat(n_na, ifelse(n_na == 1, 'NA', 'NAs'), 'found in the `site2` data and ignored during calculation.\n')
  }

  site1 <- data.frame(table(site1))
  site2 <- data.frame(table(site2))
  colnames(site1) <- colnames(site2) <- c('type', 'count')
  combined_counts <- merge(site1, site2, by = 'type', all = TRUE)
  combined_counts$lesser <- as.numeric(apply(combined_counts[, c('count.x', 'count.y')], 1, min))

  x2_Cij <- 2 * sum(combined_counts$lesser, na.rm = TRUE)
  s1_plus_s2 <- sum(combined_counts$count.x, combined_counts$count.y, na.rm = TRUE)
  bc <- 1 - (x2_Cij / s1_plus_s2)

  ifelse(sorensen_dice == TRUE, 1 - bc, bc)
}
