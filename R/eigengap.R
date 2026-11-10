# METHOD: E1 — Signed Spectral Clustering (steps 4–5)
# THREAT: T6 — Automates k selection via spectral decomposition

#' Select k via Eigengap Heuristic
#'
#' Examines the eigenvalue spectrum of the Signed Normalized Laplacian and
#' selects k as the number of clusters indicated by the largest gap between
#' consecutive sorted eigenvalues.
#'
#' Because the Signed Normalized Laplacian has no trivial zero eigenvalue
#' (unlike the unsigned Laplacian), a k-faction structure produces k-1
#' small eigenvalues separated from the bulk. The number of clusters is
#' therefore **gap position + 1**.
#'
#' A clean gap indicates well-defined factional structure. A muddy spectrum
#' suggests the field does not decompose neatly, which is itself a
#' substantive finding.
#'
#' @param eigenvalues Numeric vector of eigenvalues from the Signed
#'   Normalized Laplacian, sorted in ascending order.
#' @param max_k Maximum number of clusters to consider. Default `10`.
#' @param min_k Minimum number of clusters. Default `2`.
#'
#' @return A list with components:
#'   \item{k}{Selected number of clusters.}
#'   \item{gap_position}{Index of the largest gap in the eigenvalue sequence.}
#'   \item{gaps}{Vector of gaps between consecutive eigenvalues.}
#'   \item{eigenvalues}{The input eigenvalues (for diagnostic plotting).}
#' @examples
#' eigenvalues <- c(0, 0.02, 0.05, 0.9, 0.95, 1.0, 1.1)
#' eigengap_select_k(eigenvalues)$k
#' @export
eigengap_select_k <- function(eigenvalues, max_k = 10, min_k = 2) {
  eigenvalues <- sort(eigenvalues)
  n_consider <- min(max_k, length(eigenvalues) - 1)
  gaps <- diff(eigenvalues[seq_len(n_consider + 1)])
  # For the signed Laplacian: k factions → k-1 small eigenvalues → gap at
  # position k-1. So k = gap_position + 1.
  # Search gap positions that yield k in [min_k, max_k].
  search_start <- max(1, min_k - 1)
  search_end <- min(n_consider, max_k - 1)
  search_range <- seq(search_start, search_end)
  gap_pos <- search_range[which.max(gaps[search_range])]
  k <- gap_pos + 1
  list(k = k, gap_position = gap_pos, gaps = gaps, eigenvalues = eigenvalues)
}
