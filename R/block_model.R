# CLAIM: C2, C3, C4, C5 — Block matrices reveal inter-faction topology
# METHOD: E2 — Block Density Matrix
# METHOD: E3 — Topology Classification

#' Detect Games from Block Matrix
#'
#' Thresholds the absolute block matrix to create a meta-adjacency matrix,
#' then identifies connected components. Factions in the same component
#' are "playing the same game."
#'
#' @param B Signed block density matrix (from [trca_block_density()]).
#' @param threshold Minimum |B_uv| to define a game link. Default `0.25`.
#'
#' @return A list with components:
#'   \item{n_games}{Number of distinct games.}
#'   \item{membership}{Named vector mapping factions to games.}
#'   \item{meta_graph}{igraph object of the meta-adjacency.}
#' @examples
#' B <- matrix(c(1, -0.6, 0, 0, -0.6, 1, 0, 0, 0, 0, 1, -0.5, 0, 0, -0.5, 1), 4, 4,
#'             dimnames = list(paste0("F", 1:4), paste0("F", 1:4)))
#' detect_games(B)$n_games
#' @export
detect_games <- function(B, threshold = 0.25) {
  B[is.na(B)] <- 0
  adj <- abs(B) > threshold
  diag(adj) <- FALSE
  mode(adj) <- "numeric"
  meta_graph <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  igraph::V(meta_graph)$name <- rownames(B)
  comp <- igraph::components(meta_graph)
  membership <- stats::setNames(comp$membership, rownames(B))
  list(n_games = comp$no, membership = membership, meta_graph = meta_graph)
}

#' Classify Topology of a Game
#'
#' Classifies the sign pattern of a block sub-matrix into a sociological
#' category:
#' - **Consensus**: Single faction or all-positive B
#' - **Bipolar Opposition**: 2 factions with negative B_12
#' - **Triadic Impasse**: 3 factions, all off-diagonals negative (frustrated)
#' - **Multipolar Opposition**: 4+ factions, all off-diagonals negative
#' - **Coalition**: Two blocs with positive within-bloc and negative between-bloc ties
#' - **Hub-and-Spoke**: Center with negative ties to periphery, periphery orthogonal
#' - **Complex**: Multi-polar with mixed signs
#'
#' @param B Block density matrix (full or sub-matrix for one game).
#' @param threshold Minimum |B_uv| to count as a substantive tie. Default `0.1`.
#'
#' @return Character string naming the topology type.
#' @examples
#' B <- matrix(c(1, -0.6, -0.6, 1), 2, 2, dimnames = list(c("A", "B"), c("A", "B")))
#' classify_topology(B)
#' @export
classify_topology <- function(B, threshold = 0.1) {
  k <- nrow(B)
  if (k == 1) return("Consensus")
  off_diag <- B[upper.tri(B)]
  # Categorize each off-diagonal tie
  negative <- off_diag < -threshold
  positive <- off_diag > threshold
  orthogonal <- !negative & !positive
  if (k == 2) {
    if (all(negative)) return("Bipolar Opposition")
    if (all(positive)) return("Consensus")
    return("Complex")
  }
  # k >= 3: all off-diagonals negative → mutual opposition
  if (all(negative)) {
    if (k == 3) return("Triadic Impasse")
    return("Multipolar Opposition")
  }
  if (all(positive)) return("Consensus")
  # Coalition: check if factions partition into two blocs where
  # within-bloc ties are positive and between-bloc ties are negative.
  # Try all possible 2-partitions of k factions (balanced or near-balanced).
  if (k >= 4) {
    coalition_found <- .detect_coalition(B, k, threshold)
    if (coalition_found) return("Coalition")
  }
  # Hub-and-Spoke: one faction negative to all others, others orthogonal
  # to each other. Check each faction as potential hub.
  for (h in seq_len(k)) {
    hub_ties <- B[h, -h]
    spoke_pairs <- B[-h, -h]
    spoke_off <- spoke_pairs[upper.tri(spoke_pairs)]
    hub_neg <- all(hub_ties < -threshold)
    spokes_orth <- all(abs(spoke_off) <= threshold)
    if (hub_neg && spokes_orth) return("Hub-and-Spoke")
  }
  "Complex"
}

#' Detect Coalition Structure in Block Matrix
#'
#' Checks whether factions can be split into two blocs with positive
#' within-bloc ties and negative between-bloc ties.
#'
#' @param B Block density matrix.
#' @param k Number of factions.
#' @param threshold Tie threshold.
#' @return Logical; TRUE if a valid coalition partition exists.
#' @keywords internal
.detect_coalition <- function(B, k, threshold) {
  # Try all ways to split k factions into two non-empty groups.
  # For efficiency, only check partitions where group 1 has size 2..k/2
  # (faction 1 always in group 1 to avoid duplicates).
  for (size in 2:max(2, k %/% 2)) {
    combos <- utils::combn(2:k, size - 1)
    for (col in seq_len(ncol(combos))) {
      bloc_a <- c(1L, combos[, col])
      bloc_b <- setdiff(seq_len(k), bloc_a)
      # Check within-bloc A: all positive
      if (length(bloc_a) >= 2) {
        within_a <- B[bloc_a, bloc_a]
        off_a <- within_a[upper.tri(within_a)]
        if (!all(off_a > threshold)) next
      }
      # Check within-bloc B: all positive
      if (length(bloc_b) >= 2) {
        within_b <- B[bloc_b, bloc_b]
        off_b <- within_b[upper.tri(within_b)]
        if (!all(off_b > threshold)) next
      }
      # Check between-bloc: all negative
      between <- B[bloc_a, bloc_b, drop = FALSE]
      if (all(between < -threshold)) return(TRUE)
    }
  }
  FALSE
}
