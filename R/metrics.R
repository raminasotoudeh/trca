# CLAIM: C2, C3, C4 — Quantify cluster recovery and structural balance
# METHOD: E5 — Adjusted Rand Index
# METHOD: E6 — Structural Balance Score

#' Adjusted Rand Index
#'
#' Computes the Adjusted Rand Index between recovered cluster assignments
#' and ground truth labels. Wraps [mclust::adjustedRandIndex()].
#'
#' @param clusters Integer vector of recovered cluster assignments.
#' @param truth Integer vector of ground truth labels.
#'
#' @return Numeric ARI value in \[-1, 1\], where 1 = perfect agreement.
#' @examples
#' if (requireNamespace("mclust", quietly = TRUE)) {
#'   adjusted_rand_index(c(1, 1, 2, 2), c(2, 2, 1, 1))
#' }
#' @export
adjusted_rand_index <- function(clusters, truth) {
  if (!requireNamespace("mclust", quietly = TRUE)) {
    stop("Package 'mclust' is required for adjusted_rand_index(). Please install it.")
  }
  mclust::adjustedRandIndex(clusters, truth)
}

#' Normalized Mutual Information
#'
#' Computes the Normalized Mutual Information (sqrt normalization) between
#' two cluster assignments. Unlike ARI, NMI gives full credit when one
#' partition perfectly refines the other (e.g., 4 clusters that are subsets
#' of 2 ground-truth classes score 1.0).
#'
#' @param clusters Integer vector of cluster assignments.
#' @param truth Integer vector of ground truth labels.
#'
#' @return Numeric NMI value in \[0, 1\], where 1 = perfect agreement or
#'   perfect refinement.
#' @examples
#' normalized_mutual_info(c(1, 1, 2, 2), c(2, 2, 1, 1))
#' @export
normalized_mutual_info <- function(clusters, truth) {
  n <- length(clusters)
  tab <- table(clusters, truth)
  p_joint <- tab / n
  p_c <- rowSums(p_joint)
  p_t <- colSums(p_joint)

  mi <- 0
  for (i in seq_len(nrow(tab))) {
    for (j in seq_len(ncol(tab))) {
      if (p_joint[i, j] > 0) {
        mi <- mi + p_joint[i, j] * log2(p_joint[i, j] / (p_c[i] * p_t[j]))
      }
    }
  }

  h_c <- -sum(p_c[p_c > 0] * log2(p_c[p_c > 0]))
  h_t <- -sum(p_t[p_t > 0] * log2(p_t[p_t > 0]))

  denom <- sqrt(h_c * h_t)
  if (denom == 0) return(1)
  mi / denom
}
