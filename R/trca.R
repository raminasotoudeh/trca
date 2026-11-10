# CLAIM: C1, C2, C3, C4 — Signed Bipartite Spectral Embedding avoids both
#   Compression Error (preserves sign) and Dilution Error (no item averaging)
# METHOD: E8 — tRCA (signed bipartite spectral embedding): jointly embeds respondents and items via signed bipartite graph

# --- Internal helpers --------------------------------------------------

#' Construct the Normalized Bipartite Adjacency Matrix
#'
#' Centers the N x M response matrix and applies signed normalized Laplacian
#' normalization: `X_norm[i,j] = X_c[i,j] / sqrt(d_i * d_j)`, where d_i and d_j
#' are absolute-weight degrees.
#'
#' @param data_mat Numeric matrix (N x M), respondents x items.
#' @param centering Centering method: `"mean"` (column means) or `"midpoint"`.
#' @param midpoint Scale midpoint for `centering = "midpoint"`. If `NULL`,
#'   auto-detected as `(min + max) / 2`.
#' @return A list with `X_c` (centered matrix), `X_norm` (normalized matrix),
#'   `d_resp` (respondent degrees), `d_item` (item degrees).
#' @keywords internal
.bipartite_normalize <- function(data_mat, centering = "mean", midpoint = NULL) {
  X_c <- data_mat

  if (centering == "midpoint") {
    if (is.null(midpoint)) {
      midpoint <- (min(X_c, na.rm = TRUE) + max(X_c, na.rm = TRUE)) / 2
    }
    X_c <- X_c - midpoint
  } else {
    col_means <- colMeans(X_c, na.rm = TRUE)
    X_c <- sweep(X_c, 2, col_means)
  }

  # NAs become 0 (no edge in bipartite graph)
  X_c[is.na(X_c)] <- 0

  # Absolute-weight degrees
  d_resp <- rowSums(abs(X_c))
  d_item <- colSums(abs(X_c))

  # D^{-1/2} with protection against zero degree
  d_resp_inv_sqrt <- ifelse(d_resp > 1e-10, 1 / sqrt(d_resp), 0)
  d_item_inv_sqrt <- ifelse(d_item > 1e-10, 1 / sqrt(d_item), 0)

  # Normalized bipartite adjacency: X_norm[i,j] = X_c[i,j] / sqrt(d_i * d_j)
  X_norm <- sweep(sweep(X_c, 1, d_resp_inv_sqrt, "*"), 2, d_item_inv_sqrt, "*")

  list(X_c = X_c, X_norm = X_norm, d_resp = d_resp, d_item = d_item)
}

#' Pre-Clustering Item Filter
#'
#' Screens items BEFORE k-means clustering to remove those that carry
#' little signal about group structure. Two criteria are available:
#'
#' - **Variance-based** (`"variance"`): Items with low response variance
#'   across respondents. Simple but crude — high variance can be noise.
#'
#' - **Embedding-based** (`"embedding"`, recommended): Runs a first-pass SVD
#'   on the normalized bipartite adjacency, then measures each item's L2
#'   norm in the embedding space. Items near the origin contribute nothing
#'   to the spectral decomposition that drives clustering. This is computed
#'   after SVD but before k-means, so there is no circularity with the
#'   discovered groups. A second-pass SVD is then run on the filtered items.
#'
#' @param X_norm Normalized bipartite adjacency matrix (N x M).
#' @param method Filtering method: `"embedding"` (default) or `"variance"`.
#' @param threshold_rule How to set the cutoff: `"above_mean"` (default)
#'   or `"above_median"`.
#' @param max_k Maximum number of SVD dimensions for embedding filter.
#' @return A list with `keep` (logical vector), `scores` (numeric vector of
#'   item scores), `threshold` (the cutoff used), `n_dropped` (count),
#'   `criterion` (name of the score used).
#' @keywords internal
.filter_items <- function(X_norm, method = "embedding",
                          threshold_rule = "above_mean", max_k = 10) {
  m <- ncol(X_norm)
  n <- nrow(X_norm)

  if (method == "embedding") {
    # First-pass SVD to get item embedding norms
    n_sv <- min(max_k, n - 1, m - 1)
    svd_pre <- svd(X_norm, nu = 0, nv = n_sv)  # only need V (item vectors)
    V <- svd_pre$v[, seq_len(n_sv), drop = FALSE]
    scores <- sqrt(rowSums(V^2))
    criterion <- "embedding_norm"
  } else {
    # Variance of centered columns
    scores <- apply(X_norm, 2, var, na.rm = TRUE)
    criterion <- "variance"
  }

  threshold <- switch(threshold_rule,
    above_mean   = mean(scores),
    above_median = stats::median(scores),
    mean(scores)
  )

  keep <- scores >= threshold

  # Always keep at least 3 items
  if (sum(keep) < 3) {
    top3 <- order(-scores)[1:min(3, length(scores))]
    keep[top3] <- TRUE
  }

  list(keep = keep, scores = scores, threshold = threshold,
       n_dropped = sum(!keep), criterion = criterion)
}

#' Count Signal Dimensions by Parallel Analysis
#'
#' Counts the leading principal components of the rescaled, column-centered
#' data whose singular values exceed the 95th percentile of a null in which
#' each column is permuted independently (Horn 1965). k factions separated in
#' item space span k - 1 such dimensions. The count is taken before the signed
#' degree normalization, which rescales every respondent to unit size and
#' would hide differences in intensity.
#'
#' @param data_mat Numeric matrix (N x M), already rescaled. Missing values
#'   are set to 0 after centering, as in `.bipartite_normalize()`.
#' @param max_d Maximum number of components to test.
#' @param n_perm Number of permutations.
#' @return A list with `n_dims` (number of components above the null) and
#'   `scores` (N x `n_dims` principal component scores).
#' @keywords internal
.signal_dims <- function(data_mat, max_d, n_perm = 20) {
  X_c <- sweep(data_mat, 2, colMeans(data_mat, na.rm = TRUE))
  X_c[is.na(X_c)] <- 0  # missing = no edge, as in .bipartite_normalize()
  max_d <- max(1, min(max_d, nrow(X_c) - 1, ncol(X_c)))
  sv <- svd(X_c, nu = max_d, nv = 0)
  obs <- sv$d[seq_len(max_d)]
  null <- vapply(seq_len(n_perm), function(i) {
    svd(apply(X_c, 2, sample), nu = 0, nv = 0)$d[seq_len(max_d)]
  }, numeric(max_d))
  q95 <- apply(matrix(null, nrow = max_d), 1, stats::quantile, probs = 0.95)
  above <- obs > q95
  n_dims <- if (all(above)) max_d else which(!above)[1] - 1
  list(
    n_dims = as.integer(n_dims),
    scores = sv$u[, seq_len(n_dims), drop = FALSE] %*%
      diag(obs[seq_len(n_dims)], n_dims)
  )
}

#' Test Whether the Data Contain More Than One Faction
#'
#' Spectral clustering cannot compare k = 1 with k >= 2, and the signed
#' degree normalization rescales every respondent to unit size. Under
#' consensus with varying intensity, that normalization snaps respondents
#' above and below the mean intensity onto opposite poles, mimicking a
#' bipolar split. This test therefore works on the principal component
#' scores from `.signal_dims()`, where intensity is preserved. With no signal
#' dimensions there is one faction. Otherwise the gap statistic (Tibshirani,
#' Walther & Hastie 2001) chooses one faction when
#' `gap(1) >= gap(2) - SE(2)` (the standard one-standard-error rule).
#'
#' Factions separated in item space produce separated lumps of scores;
#' consensus produces one continuous spread and returns `TRUE`.
#'
#' @param signal Output of `.signal_dims()`.
#' @param B Number of reference datasets for the gap statistic.
#' @param nstart Random starts for k-means inside the gap statistic.
#' @param iter.max Maximum iterations for k-means.
#' @return A list with `one_faction` (logical), `n_dims` (significant
#'   components), `gap` (gap at k = 1 and 2, or `NULL`), and `se` (their
#'   simulation standard errors, or `NULL`).
#' @keywords internal
.test_one_faction <- function(signal, B = 50, nstart = 10, iter.max = 100) {
  if (signal$n_dims == 0) {
    return(list(one_faction = TRUE, n_dims = 0L, gap = NULL, se = NULL))
  }
  km <- function(x, k) {
    list(cluster = kmeans(x, k, nstart = nstart, iter.max = iter.max)$cluster)
  }
  tab <- cluster::clusGap(signal$scores, km, K.max = 2, B = B,
                          verbose = FALSE)$Tab
  list(
    one_faction = unname(tab[1, "gap"] >= tab[2, "gap"] - tab[2, "SE.sim"]),
    n_dims = signal$n_dims,
    gap = unname(tab[, "gap"]),
    se = unname(tab[, "SE.sim"])
  )
}


#' Choose k by Silhouette in the Signal Dimensions
#'
#' Runs k-means for each candidate k in a row-normalized respondent
#' embedding with as many dimensions as there are signal dimensions (k
#' factions span k - 1), and returns the k with the highest average
#' silhouette width. Extra dimensions are mostly noise, and with few items
#' they would dominate the row-normalized directions. Shared by [trca()] and
#' [trca_select_k()] so both recommend the same k.
#'
#' @param u Left singular vectors of the normalized bipartite adjacency.
#' @param n_dims Number of signal dimensions from `.signal_dims()`.
#' @param k_values Candidate k values (all >= 2).
#' @param nstart,iter.max Passed to [stats::kmeans()].
#' @param seed Optional seed, set once before the search.
#' @return A list with `k` (selected), `silhouette` (data frame of k and
#'   average silhouette width; `NA` where k-means could not run), and
#'   `embed_dims` (dimensions used).
#' @keywords internal
.silhouette_select <- function(u, n_dims, k_values, nstart, iter.max,
                               seed = NULL) {
  embed_dims <- min(max(n_dims, min(k_values) - 1, 1), ncol(u))
  raw <- u[, seq_len(embed_dims), drop = FALSE]
  rn <- sqrt(rowSums(raw^2))
  rn[rn < 1e-10] <- 1e-10
  emb <- raw / rn
  # In few dimensions row-normalized points can collapse (in 1-D to +/-1)
  n_distinct <- nrow(unique(round(emb, 8)))
  avg_sil <- rep(NA_real_, length(k_values))
  if (!is.null(seed)) set.seed(seed)
  for (i in seq_along(k_values)) {
    if (k_values[i] > n_distinct) next
    km <- kmeans(emb, centers = k_values[i], nstart = nstart, iter.max = iter.max)
    sil <- cluster::silhouette(km$cluster, dist(emb))
    avg_sil[i] <- mean(sil[, "sil_width"])
  }
  k <- if (all(is.na(avg_sil))) min(k_values) else k_values[which.max(avg_sil)]
  list(
    k = as.integer(k),
    silhouette = data.frame(k = k_values, avg_silhouette = avg_sil),
    embed_dims = as.integer(embed_dims)
  )
}


# --- Main function -----------------------------------------------------

#' Topological Relational Class Analysis (tRCA)
#'
#' Fits tRCA. Jointly embeds respondents and items in a shared spectral space
#' via the Signed Normalized Laplacian of the respondent-item bipartite graph,
#' then clusters only the respondent rows. This avoids two errors present in
#' correlation-based approaches such as RCA:
#'
#' 1. **Compression Error**: abs(W) destroys sign information.
#' 2. **Dilution Error**: correlating respondents across
#'    ALL items averages discriminating and non-discriminating items together,
#'    burying item-specific opposition in a sea of agreement.
#'
#' tRCA works with the raw N x M centered response matrix directly. The SVD of
#' the normalized bipartite adjacency yields eigenvalues and eigenvectors of the
#' Signed Normalized Laplacian without forming the (N+M) x (N+M) matrix.
#'
#' Spectral clustering on its own cannot choose k = 1, and the signed degree
#' normalization makes consensus with varying intensity look like a bipolar
#' split (respondents above and below the mean intensity land on opposite
#' poles). So when k is chosen automatically and `min_k = 1`, `trca()` first
#' tests for a single faction on the rescaled, column-centered data:
#' parallel analysis (Horn 1965) counts principal components above a
#' column-permutation null, and the gap statistic (Tibshirani, Walther &
#' Hastie 2001) on those components decides between one and two groups with
#' the one-standard-error rule. The test is conservative: in simulations it
#' keeps every structured scenario split, and it recognizes consensus in
#' roughly 85% of samples. The rest fall through to the usual k >= 2 search.
#'
#' The silhouette search for k >= 2 runs in an embedding with as many
#' dimensions as the parallel analysis finds signal dimensions (k factions
#' span k - 1), rather than in all `max_k` dimensions. With few items the
#' extra dimensions are mostly noise, and the row-normalized directions then
#' cluster on that noise. In simulations across 6 to 42 items, this raised the
#' share of datasets with the correct k from 89% to 98%. The final clustering
#' always uses a `k`-dimensional embedding, as with a user-supplied `k`.
#'
#' When `filter_items` is enabled, items are screened BEFORE clustering.
#' The recommended method (`"embedding"`) runs a first-pass SVD on all items,
#' measures each item's L2 norm in the embedding space (how much it
#' contributes to the spectral decomposition), drops items with below-mean
#' norms, then re-runs the SVD on the remaining items. This is computed
#' after SVD but before k-means, so there is no circularity with the
#' discovered groups.
#'
#' @param data Numeric matrix or data frame of respondents (rows) x items
#'   (columns). Data frame columns may be numeric, logical, ordered factors
#'   (their integer codes are used), or haven-labelled numerics; unordered
#'   factors and character columns are rejected. Missing values are allowed
#'   and are treated as absent edges in the bipartite graph (set to 0 after
#'   centering). A respondent or item with no observed responses is rejected.
#' @param k Optional number of clusters. If `NULL` (default), selected
#'   automatically: when `min_k = 1`, a one-faction test runs first (see
#'   Details); if it finds more than one faction, k is chosen via `method`.
#' @param method k-selection/clustering method: `"silhouette"` (default) or
#'   `"kmeans"` (uses eigengap).
#' @param centering How to center the response matrix:
#'   - `"mean"` (default): Column-mean centering. Each item's mean response
#'     becomes zero, so positive = above average, negative = below average.
#'   - `"midpoint"`: Center on the scale midpoint (e.g., 3 on a 1-5 Likert
#'     scale). Positive = agree, negative = disagree. More interpretable
#'     when the scale has a meaningful neutral point.
#' @param midpoint Scale midpoint for `centering = "midpoint"`. If `NULL`
#'   (default), auto-detected as `(min + max) / 2` from the data.
#' @param rescale Logical. If `TRUE` (default), rescale each item to \[0, 1\]
#'   before centering. This equalizes item contributions to the degree
#'   normalization when items are on different scales (e.g., mixing 1-3 and
#'   1-5 Likert items). No-op when all items share the same scale.
#' @param filter_items Item pre-screening method. `FALSE` (default) to use
#'   all items. `TRUE` or `"embedding"` (recommended) to run a first-pass SVD,
#'   drop items whose embedding norm is below the mean, then re-run SVD.
#'   `"variance"` to drop items with below-mean variance instead.
#'   All filtering happens before k-means clustering — no circularity.
#' @param max_k Maximum k to consider. Default `10`.
#' @param min_k Minimum k. Default `1`, which allows a consensus (one
#'   faction) solution. Set `min_k = 2` to skip the one-faction test and
#'   always split the sample.
#' @param nstart Random starts for k-means. Default `50`.
#' @param iter.max Maximum iterations for k-means. Default `100`.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#' @param verbose Logical. If `TRUE` (default), prints which items are dropped
#'   when `filter_items` is enabled.
#'
#' @return A list with components:
#'   \item{clusters}{Integer vector of cluster assignments (length N).}
#'   \item{k}{Number of clusters used.}
#'   \item{k_auto}{Logical: was k selected automatically?}
#'   \item{k_method}{How k was chosen: `"user"`, `"one_faction_test"` (k = 1),
#'     `"eigengap"`, or `"silhouette"`.}
#'   \item{signal_dims}{Number of signal dimensions found by parallel
#'     analysis, which sets the silhouette search's embedding dimension.
#'     `NULL` if `k` was supplied.}
#'   \item{one_faction_test}{Result of the one-faction test: `one_faction`,
#'     `n_dims` (significant components), and `gap`/`se` at k = 1 and 2.
#'     `NULL` if the test did not run.}
#'   \item{centering}{Centering method used.}
#'   \item{respondent_embedding}{N x k matrix of respondent coordinates.}
#'   \item{item_embedding}{M_kept x k matrix of item coordinates (kept items only).}
#'   \item{eigenvalues}{Bipartite Laplacian eigenvalues (1 - singular_values).}
#'   \item{singular_values}{Singular values of the normalized bipartite adjacency.}
#'   \item{eigengap}{Eigengap diagnostics (always computed).}
#'   \item{X_centered}{The centered N x M_kept data matrix (kept items only).}
#'   \item{items_kept}{Character vector of item names retained after filtering
#'     (or column indices if no names). `NULL` if `filter_items = FALSE`.}
#'   \item{items_dropped}{Character vector of item names dropped.
#'     `NULL` if `filter_items = FALSE`.}
#'   \item{item_scores}{Named numeric vector of pre-clustering item scores
#'     (embedding norms or variances) for ALL original items.
#'     `NULL` if `filter_items = FALSE`.}
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, verbose = FALSE)
#' fit
#' table(fit$clusters, sim$truth)
#' @export
trca <- function(data, k = NULL, max_k = 10, min_k = 1,
                         centering = c("mean", "midpoint"),
                         midpoint = NULL,
                         rescale = TRUE,
                         filter_items = FALSE,
                         method = c("silhouette", "kmeans"),
                         nstart = 50, 
                         iter.max = 100,
                         seed = NULL,
                         verbose = TRUE) {
  centering <- match.arg(centering)
  method <- match.arg(method)
  if (min_k < 1) stop("`min_k` must be at least 1.")
  data_mat <- .as_response_matrix(data)
  n <- nrow(data_mat)
  m_orig <- ncol(data_mat)
  if (m_orig < 3) {
    stop(sprintf("tRCA needs at least 3 items (got %d).", m_orig), call. = FALSE)
  }
  k <- .check_k(k, n)

  # Item names for reporting
  item_names_orig <- colnames(data_mat)
  if (is.null(item_names_orig)) {
    item_names_orig <- paste0("item_", seq_len(m_orig))
  }

  # Rescale each item to [0, 1] to equalize contributions across scales
  if (rescale) {
    col_min <- apply(data_mat, 2, min, na.rm = TRUE)
    col_range <- apply(data_mat, 2, max, na.rm = TRUE) - col_min
    col_range[col_range < 1e-10] <- 1  # protect against constant columns
    data_mat <- sweep(sweep(data_mat, 2, col_min), 2, col_range, "/")
  }

  # 0. Pre-clustering item filtering (after SVD, before k-means)
  items_kept <- NULL
  items_dropped <- NULL
  item_scores <- NULL

  if (!identical(filter_items, FALSE)) {
    # Determine filter method
    if (isTRUE(filter_items) || filter_items == "embedding") {
      filter_method <- "embedding"
    } else if (filter_items == "variance") {
      filter_method <- "variance"
    } else {
      filter_method <- "embedding"  # default
    }

    # Center and normalize all items for first-pass screening
    bip_pre <- .bipartite_normalize(data_mat, centering = centering, midpoint = midpoint)

    filt <- .filter_items(bip_pre$X_norm, method = filter_method, max_k = max_k)

    item_scores <- filt$scores
    names(item_scores) <- item_names_orig

    items_kept <- item_names_orig[filt$keep]
    items_dropped <- item_names_orig[!filt$keep]

    if (verbose && filt$n_dropped > 0) {
      dropped_scores <- item_scores[items_dropped]
      dropped_info <- sprintf("  %s (%s = %.4f)", items_dropped,
                              filt$criterion, dropped_scores)
      message(sprintf(
        "tRCA item filter (%s): dropping %d of %d items (%s < %.4f):\n%s\n  Keeping %d items: %s",
        filt$criterion, filt$n_dropped, m_orig,
        filt$criterion, filt$threshold,
        paste(dropped_info, collapse = "\n"),
        sum(filt$keep), paste(items_kept, collapse = ", ")
      ))
    } else if (verbose) {
      message(sprintf(
        "tRCA item filter (%s): all %d items above threshold (%.4f), none dropped.",
        filt$criterion, m_orig, filt$threshold
      ))
    }

    # Filter the data matrix — second-pass SVD will be on kept items only
    data_mat <- data_mat[, filt$keep, drop = FALSE]
  }

  m <- ncol(data_mat)

  # Cap max_k at m - 1 (SVD of N x M matrix yields at most min(N,M)-1 useful vectors)
  max_k <- min(max_k, m - 1, n - 1)
  if (!is.null(k) && k > m - 1) {
    warning(sprintf(
      "Requested k=%d exceeds number of items after filtering (%d). Capping at k=%d.",
      k, m, m - 1))
    k <- m - 1
  }

  # 1. Center and normalize (on filtered data)
  bip <- .bipartite_normalize(data_mat, centering = centering, midpoint = midpoint)

  # 2. SVD of normalized bipartite adjacency
  # The SVD gives us eigenvalues/vectors of the Signed Normalized Laplacian:
  #   L_norm = I - D^{-1/2} A D^{-1/2}
  # where A is the (N+M)x(N+M) bipartite adjacency matrix.
  # Eigenvalues of L_norm: 1 - s_i (smallest) and 1 + s_i (largest)
  # Eigenvectors for eigenvalue 1 - s_i: [u_i; v_i] / sqrt(2)
  # So respondent embedding = U, item embedding = V.
  n_sv <- min(max_k, n - 1, m - 1)
  svd_res <- svd(bip$X_norm, nu = n_sv, nv = n_sv)

  singular_values <- svd_res$d[seq_len(n_sv)]
  eigenvalues <- 1 - singular_values  # smallest eigenvalues of bipartite Laplacian

  # 3. Eigengap on bipartite Laplacian eigenvalues (always computed)
  egap <- eigengap_select_k(eigenvalues, max_k = max_k, min_k = max(min_k, 2))

  k_auto <- is.null(k)

  # 3b. Signal dimensions (parallel analysis) and the one-faction test,
  # which the spectral search below can't do because it never returns k = 1
  signal <- NULL
  one_faction <- NULL
  if (k_auto) {
    if (!is.null(seed)) set.seed(seed)
    signal <- .signal_dims(data_mat, max_d = max_k)
    if (min_k == 1) {
      one_faction <- .test_one_faction(signal, iter.max = iter.max)
    }
  }
  search_min_k <- max(min_k, 2)

  # Helper: embed respondents at dimension d, row-normalize
  embed_resp <- function(d) {
    raw <- svd_res$u[, seq_len(d), drop = FALSE]
    rn <- sqrt(rowSums(raw^2))
    rn[rn < 1e-10] <- 1e-10
    raw / rn
  }

  # Helper: item embedding at dimension d (no normalization — items stay in native scale)
  embed_item <- function(d) {
    svd_res$v[, seq_len(d), drop = FALSE]
  }

  # 4. k-selection and clustering
  if (isTRUE(one_faction$one_faction)) {
    k <- 1L
    k_method <- "one_faction_test"
  } else if (method == "silhouette") {
    if (!requireNamespace("cluster", quietly = TRUE)) {
      stop("Package 'cluster' is required for method = \"silhouette\"")
    }
    if (k_auto) {
      if (max_k < search_min_k) {
        k <- search_min_k
      } else {
        k <- .silhouette_select(svd_res$u, signal$n_dims,
                                search_min_k:max_k, nstart, iter.max,
                                seed)$k
      }
      k_method <- "silhouette"
    } else {
      k_method <- "user"
    }
  } else {
    if (k_auto) {
      k <- egap$k
      k_method <- "eigengap"
    } else {
      k_method <- "user"
    }
  }

  # 5. Final embedding and clustering
  resp_emb <- embed_resp(k)
  item_emb <- embed_item(k)
  if (!is.null(seed)) set.seed(seed)
  km <- kmeans(resp_emb, centers = k, nstart = nstart, iter.max = iter.max)
  clusters <- km$cluster

  # Preserve item names
  if (!is.null(colnames(data_mat))) {
    rownames(item_emb) <- colnames(data_mat)
  }

  result <- list(
    clusters    = clusters,
    k           = k,
    k_auto      = k_auto,
    k_method    = k_method,
    centering   = centering,
    rescale     = rescale,
    respondent_embedding = resp_emb,
    item_embedding       = item_emb,
    eigenvalues          = eigenvalues,
    singular_values      = singular_values,
    eigengap             = egap,
    signal_dims          = signal$n_dims,
    one_faction_test     = one_faction,
    X_centered           = bip$X_c,
    items_kept           = items_kept,
    items_dropped        = items_dropped,
    item_scores          = item_scores
  )
  class(result) <- "trca"
  result
}


#' tRCA Block Density Matrix (Profile-Based)
#'
#' Computes a k x k block density matrix from the cluster mean profiles in
#' item space, rather than from person-to-person correlations. Each entry
#' `B[u,v]` is the Pearson correlation between the mean response profile of
#' cluster u and cluster v.
#'
#' This avoids the Dilution Error: each item contributes one observation to
#' the profile correlation, so the topology reflects the pattern of agreement
#' and disagreement across items rather than averaging over person-pairs.
#'
#' @param data Numeric matrix of respondents (rows) x items (columns).
#'   Raw (uncentered) data — centering is applied internally.
#' @param clusters Integer vector of cluster assignments.
#' @param centering Centering method: `"mean"` or `"midpoint"`. Default `"mean"`.
#' @param midpoint Scale midpoint for `centering = "midpoint"`.
#'
#' @return A k x k block density matrix with row/column names as faction labels.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' trca_block_density(sim$data, fit$clusters)
#' @export
trca_block_density <- function(data, clusters, centering = "mean",
                               midpoint = NULL) {
  data_mat <- .as_response_matrix(data)
  .check_clusters(clusters, nrow(data_mat))

  # Center for profile computation
  if (centering == "midpoint") {
    if (is.null(midpoint)) {
      midpoint <- (min(data_mat, na.rm = TRUE) + max(data_mat, na.rm = TRUE)) / 2
    }
    X_c <- data_mat - midpoint
  } else {
    col_means <- colMeans(data_mat, na.rm = TRUE)
    X_c <- sweep(data_mat, 2, col_means)
  }

  ids <- sort(unique(clusters))
  k <- length(ids)

  # Compute mean profile per cluster
  profiles <- matrix(NA, nrow = k, ncol = ncol(X_c))
  for (i in seq_len(k)) {
    mask <- clusters == ids[i]
    profiles[i, ] <- colMeans(X_c[mask, , drop = FALSE], na.rm = TRUE)
  }

  # Block density: correlation between cluster profiles
  B <- cor(t(profiles))

  # Handle k=1 edge case (single cluster, cor returns 1x1)
  if (k == 1) {
    B <- matrix(1, 1, 1)
  }

  # Replace NaN with 0 (occurs when a faction's profile has zero variance,
  # i.e., its mean is identical across all items after centering)
  B[is.nan(B)] <- 0

  labels <- paste0("Faction_", ids)
  rownames(B) <- colnames(B) <- labels

  # Set diagonal to the within-cluster coherence (mean pairwise correlation
  # of members).
  # For now, use 1.0 (profile correlates perfectly with itself).
  # The off-diagonals carry the topology information.
  B
}


#' tRCA Item Diagnostics
#'
#' Reports two measures of item importance, clearly distinguished by
#' when they are computed:
#'
#' - **`item_var`** (PRE-CLUSTERING): Variance of each item across all
#'   respondents. This is the measure used by `filter_items` in
#'   [trca()] to screen items before any clustering occurs.
#'   Items with low variance have too little response variation to
#'   carry signal about group structure.
#'
#' - **`embedding_norm`** (PRE-CLUSTERING): L2 norm of the item's
#'   position in the SVD embedding space. Computed from the bipartite
#'   graph decomposition before k-means, so still pre-clustering.
#'   Items far from the origin contribute more to respondent separation.
#'
#' - **`eta_sq`** (POST-CLUSTERING): Between-cluster variance / total
#'   variance for each item, using the discovered cluster assignments.
#'   Useful for interpretation but NOT used for filtering decisions
#'   (that would be circular).
#'
#' @param x Result from [trca()].
#' @param data Original data matrix (for item names and raw values).
#'   Should match the columns used in the tRCA analysis (i.e., filtered
#'   data if `filter_items` was used).
#' @param item_names Optional character vector of item names.
#'
#' @return A data frame with columns:
#'   \item{item}{Item name or index.}
#'   \item{item_var}{Pre-clustering item variance (used for filtering).}
#'   \item{embedding_norm}{Pre-clustering L2 norm in SVD embedding.}
#'   \item{eta_sq}{Post-clustering between/total variance ratio.}
#'   \item{dim_1, dim_2, ...}{Item's coordinates in each embedding dimension.}
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' head(trca_item_diagnostics(fit, data = sim$data))
#' @export
trca_item_diagnostics <- function(x, data = NULL, item_names = NULL) {
  item_emb <- x$item_embedding
  k <- x$k
  m <- nrow(item_emb)

  # Item names
  if (!is.null(item_names)) {
    names_vec <- item_names
  } else if (!is.null(rownames(item_emb))) {
    names_vec <- rownames(item_emb)
  } else {
    names_vec <- paste0("item_", seq_len(m))
  }

  # PRE-CLUSTERING: Embedding norm (from SVD, before k-means)
  emb_norm <- sqrt(rowSums(item_emb^2))

  # PRE-CLUSTERING: Item variance across respondents
  item_var <- rep(NA_real_, m)
  if (!is.null(data)) {
    data_mat <- .as_response_matrix(data)
    for (j in seq_len(m)) {
      item_var[j] <- var(data_mat[, j], na.rm = TRUE)
    }
  } else if (!is.null(x$item_scores)) {
    # Use pre-computed scores from filtering step (only for kept items)
    kept <- x$items_kept
    if (!is.null(kept)) {
      item_var <- x$item_scores[kept]
    }
  }

  # POST-CLUSTERING: Eta-squared (for interpretation, NOT for filtering)
  eta_sq <- rep(NA_real_, m)
  if (!is.null(data)) {
    data_mat <- .as_response_matrix(data)
    clusters <- x$clusters
    for (j in seq_len(m)) {
      resp <- data_mat[, j]
      valid <- !is.na(resp)
      if (sum(valid) < 2) next
      grand_mean <- mean(resp[valid])
      ss_total <- sum((resp[valid] - grand_mean)^2)
      if (ss_total < 1e-10) next
      ss_between <- 0
      for (cid in unique(clusters)) {
        mask <- valid & (clusters == cid)
        if (sum(mask) == 0) next
        grp_mean <- mean(resp[mask])
        ss_between <- ss_between + sum(mask) * (grp_mean - grand_mean)^2
      }
      eta_sq[j] <- ss_between / ss_total
    }
  }

  # Build result
  result <- data.frame(
    item = names_vec,
    item_var = item_var,
    embedding_norm = emb_norm,
    eta_sq = eta_sq,
    stringsAsFactors = FALSE
  )

  # Add embedding dimensions
  for (d in seq_len(ncol(item_emb))) {
    result[[paste0("dim_", d)]] <- item_emb[, d]
  }

  result[order(-result$item_var), ]
}


# --- k-Selection Diagnostics -----------------------------------------------

#' Select Number of Factions
#'
#' Runs the tRCA embedding pipeline once, then evaluates spectral
#' k-means clustering for each candidate k. Reports the k that [trca()]
#' would choose automatically, plus three further diagnostics:
#'
#' 0. **Automatic choice** (`k_auto`): the one-faction test, then the
#'    average silhouette width in the signal dimensions, exactly as in
#'    [trca()] with the same `nstart` and `seed`. If the one-faction test
#'    finds no factional structure, `k_auto` is 1 even though `k_range`
#'    starts at 2.
#'
#' 1. **WCSS** (Within-Cluster Sum of Squares) measured in the
#'    **original item space**. Spectral-space WCSS always decreases
#'    with k because the row-normalized embedding creates exploitable
#'    structure at every scale. Original-space WCSS shows diminishing
#'    returns, producing an interpretable elbow plot.
#'
#' 2. **Eigengap** spectrum of the bipartite Laplacian. A large gap
#'    between consecutive eigenvalues indicates a natural number of
#'    clusters.
#'
#' 3. **Spectral BIC**: `n * log(WCSS / n) + (k * d + k - 1) * log(n)`, with
#'    WCSS from k-means in a fixed `d = max(k_range)`-dimensional
#'    embedding. Each extra cluster costs `(d + 1) * log(n)`, so the
#'    criterion depends on `k_range` and favors too few factions in small
#'    samples: with 180 respondents in three simulated factions it picks
#'    k = 2. It is kept because the tRCA paper's ANES analysis reports it
#'    (N = 3,292, where it agrees with `k_auto` at k = 6), but `k_auto` is the
#'    recommended choice.
#'
#' @param data Numeric matrix of respondents (rows) x items (columns).
#' @param k_range Integer vector of candidate k values. Default `2:15`.
#' @param centering Centering method: `"mean"` (default) or `"midpoint"`.
#' @param midpoint Scale midpoint for `centering = "midpoint"`.
#' @param rescale Logical. Rescale items to \[0, 1\]? Default `TRUE`.
#' @param filter_items Item filtering method (passed to internal pipeline).
#'   Default `FALSE` (no filtering).
#' @param nstart Number of random starts for k-means. Default `25`.
#' @param iter.max Maximum iterations for k-means. Default `100`.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#' @param verbose Print progress? Default `TRUE`.
#'
#' @return An object of class `"trca_k_select"` with components:
#'   \item{k_auto}{k that [trca()] would choose automatically (recommended).}
#'   \item{silhouette}{Data frame of k and average silhouette width in the
#'     signal dimensions.}
#'   \item{signal_dims}{Number of signal dimensions from parallel analysis.}
#'   \item{one_faction_test}{Result of the one-faction test (see [trca()]).}
#'   \item{metrics}{Data frame with columns k, wcss, and bic.}
#'   \item{k_bic}{k that minimizes spectral BIC.}
#'   \item{eigenvalues}{Eigenvalues of the bipartite Laplacian.}
#'   \item{singular_values}{Singular values from the SVD.}
#'   \item{eigengap_k}{k from eigengap heuristic.}
#'   \item{eigengaps}{Vector of gaps between consecutive eigenvalues.}
#'
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' ks <- trca_select_k(sim$data, k_range = 2:6, verbose = FALSE)
#' ks
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(ks)
#' }
#' @export
trca_select_k <- function(data, k_range = 2:15, centering = "mean",
                     midpoint = NULL, rescale = TRUE,
                     filter_items = FALSE,
                     nstart = 25,
                     iter.max = 100,
                     seed = NULL,
                     verbose = TRUE) {
  centering <- match.arg(centering, c("mean", "midpoint"))
  data_mat <- .as_response_matrix(data)
  n <- nrow(data_mat)
  m <- ncol(data_mat)

  # Rescale to [0, 1]
  if (rescale) {
    col_min <- apply(data_mat, 2, min, na.rm = TRUE)
    col_range <- apply(data_mat, 2, max, na.rm = TRUE) - col_min
    col_range[col_range < 1e-10] <- 1
    data_mat <- sweep(sweep(data_mat, 2, col_min), 2, col_range, "/")
  }

  # Optional item filtering
  if (!identical(filter_items, FALSE)) {
    filter_method <- if (isTRUE(filter_items) || filter_items == "embedding") {
      "embedding"
    } else {
      "variance"
    }
    bip_pre <- .bipartite_normalize(data_mat, centering = centering,
                                     midpoint = midpoint)
    filt <- .filter_items(bip_pre$X_norm, method = filter_method)
    if (verbose && filt$n_dropped > 0) {
      message(sprintf("trca_select_k: dropping %d of %d items via %s filter",
                      filt$n_dropped, m, filter_method))
    }
    data_mat <- data_mat[, filt$keep, drop = FALSE]
    m <- ncol(data_mat)
  }

  # Cap k_range at feasible values
  k_max_feasible <- min(m - 1, n - 1)
  k_range <- k_range[k_range <= k_max_feasible & k_range >= 2]
  if (length(k_range) == 0) stop("No feasible k values in k_range")

  d_max <- max(k_range)

  # Normalize and SVD (once)
  bip <- .bipartite_normalize(data_mat, centering = centering,
                               midpoint = midpoint)
  svd_res <- svd(bip$X_norm, nu = d_max, nv = d_max)

  singular_values <- svd_res$d[seq_len(d_max)]
  eigenvalues <- 1 - singular_values

  # Eigengap
  egap <- eigengap_select_k(eigenvalues, max_k = d_max, min_k = min(k_range))

  # Prepare original-space data for WCSS (impute NAs with column means)
  orig_mat <- data_mat
  for (j in seq_len(m)) {
    na_idx <- is.na(orig_mat[, j])
    if (any(na_idx)) orig_mat[na_idx, j] <- mean(orig_mat[, j], na.rm = TRUE)
  }

  # Fixed d_max-dimensional embedding for BIC (single space across all k)
  raw_emb_full <- svd_res$u[, seq_len(d_max), drop = FALSE]
  rn_full <- sqrt(rowSums(raw_emb_full^2))
  rn_full[rn_full < 1e-10] <- 1e-10
  emb_full <- raw_emb_full / rn_full

  # Evaluate each k
  metrics <- vector("list", length(k_range))

  for (i in seq_along(k_range)) {
    kk <- k_range[i]

    # k-means in d=k spectral embedding (standard spectral clustering)
    raw_emb_k <- svd_res$u[, seq_len(kk), drop = FALSE]
    rn_k <- sqrt(rowSums(raw_emb_k^2))
    rn_k[rn_k < 1e-10] <- 1e-10
    emb_k <- raw_emb_k / rn_k

    if (!is.null(seed)) set.seed(seed)
    km <- kmeans(emb_k, centers = kk, nstart = nstart, iter.max = iter.max)

    # WCSS in original item space
    wcss_orig <- 0
    for (cl in seq_len(kk)) {
      idx <- which(km$cluster == cl)
      if (length(idx) > 1) {
        wcss_orig <- wcss_orig + sum(scale(orig_mat[idx, , drop = FALSE],
                                           scale = FALSE)^2)
      }
    }

    # Spectral BIC: k-means in fixed d_max space, BIC with d_max penalty
    if (!is.null(seed)) set.seed(seed)
    km_full <- kmeans(emb_full, centers = kk, nstart = nstart, iter.max = iter.max)
    wcss_spec <- km_full$tot.withinss
    n_params <- kk * d_max + kk - 1
    bic_val <- n * log(wcss_spec / n) + n_params * log(n)

    if (verbose) message(sprintf("  k = %2d: WCSS = %.1f  BIC = %.1f",
                                 kk, wcss_orig, bic_val))

    metrics[[i]] <- data.frame(k = kk, wcss = wcss_orig, bic = bic_val)
  }

  metrics <- do.call(rbind, metrics)

  # Automatic choice, as in trca(): one-faction test, then silhouette in the
  # signal dimensions. Computed after the metrics so they are unchanged.
  if (!is.null(seed)) set.seed(seed)
  signal <- .signal_dims(data_mat, max_d = d_max)
  one_faction <- .test_one_faction(signal, iter.max = iter.max)
  sel <- .silhouette_select(svd_res$u, signal$n_dims, k_range, nstart,
                            iter.max, seed)
  k_auto <- if (one_faction$one_faction) 1L else sel$k

  result <- list(
    k_auto = k_auto,
    silhouette = sel$silhouette,
    signal_dims = signal$n_dims,
    one_faction_test = one_faction,
    metrics = metrics,
    k_bic = metrics$k[which.min(metrics$bic)],
    eigenvalues = eigenvalues,
    singular_values = singular_values,
    eigengap_k = egap$k,
    eigengaps = egap$gaps,
    k_range = k_range,
    n = n, d_max = d_max, n_items = m
  )
  class(result) <- "trca_k_select"
  result
}

#' @export
print.trca_k_select <- function(x, ...) {
  cat("tRCA k-Selection Diagnostics\n")
  cat("============================\n")
  cat(sprintf("k range evaluated: %d to %d  (n = %d)\n\n",
              min(x$k_range), max(x$k_range), x$n))
  cat("Recommendation (as in trca()):\n")
  if (x$k_auto == 1) {
    cat("  k = 1: the one-faction test finds no factional structure\n")
  } else {
    cat(sprintf("  k = %d: highest silhouette in %d signal dimension%s\n",
                x$k_auto, x$signal_dims, if (x$signal_dims == 1) "" else "s"))
  }
  cat("Other diagnostics:\n")
  cat(sprintf("  Eigengap heuristic:   k = %d\n", x$eigengap_k))
  cat(sprintf("  Spectral BIC minimum: k = %d (favors small k in small samples)\n",
              x$k_bic))
  cat("  WCSS elbow:           visual inspection\n")
  cat("\nMetrics:\n")
  m <- merge(x$metrics, x$silhouette, by = "k", sort = TRUE)
  m$wcss <- round(m$wcss, 1)
  m$bic <- round(m$bic, 1)
  m$avg_silhouette <- round(m$avg_silhouette, 3)
  print(m, row.names = FALSE)
  invisible(x)
}

#' Plot k-selection diagnostics
#'
#' Plots the k-selection metrics computed by [trca_select_k()].
#'
#' @param x An `trca_k_select` object from [trca_select_k()].
#' @param metrics Character vector of which metrics to plot. Any combination
#'   of `"silhouette"`, `"wcss"`, `"bic"`, and `"eigengap"`. Default: all
#'   four.
#' @param ... Unused.
#' @return A `ggplot` object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' ks <- trca_select_k(sim$data, k_range = 2:6, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(ks, metrics = "eigengap")
#' }
#' @export
plot.trca_k_select <- function(x, metrics = c("silhouette", "wcss", "bic", "eigengap"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required")
  }

  metrics <- match.arg(metrics, c("silhouette", "wcss", "bic", "eigengap"),
                       several.ok = TRUE)
  m <- x$metrics

  panels <- list()

  if ("silhouette" %in% metrics) {
    sil <- x$silhouette[!is.na(x$silhouette$avg_silhouette), ]
    panels[[length(panels) + 1]] <- data.frame(
      k = sil$k, value = sil$avg_silhouette,
      metric = "Average silhouette (signal dimensions)",
      stringsAsFactors = FALSE
    )
  }

  if ("wcss" %in% metrics) {
    panels[[length(panels) + 1]] <- data.frame(
      k = m$k, value = m$wcss,
      metric = "Within-Cluster SS",
      stringsAsFactors = FALSE
    )
  }

  if ("bic" %in% metrics) {
    panels[[length(panels) + 1]] <- data.frame(
      k = m$k, value = m$bic,
      metric = "Spectral BIC",
      stringsAsFactors = FALSE
    )
  }

  if ("eigengap" %in% metrics) {
    n_gaps <- min(length(x$eigengaps), max(x$k_range) - 1)
    gap_df <- data.frame(
      k = seq(2, n_gaps + 1),
      value = x$eigengaps[seq_len(n_gaps)],
      metric = "Eigengap",
      stringsAsFactors = FALSE
    )
    gap_df <- gap_df[gap_df$k >= min(x$k_range) & gap_df$k <= max(x$k_range), ]
    panels[[length(panels) + 1]] <- gap_df
  }

  df <- do.call(rbind, panels)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$k, y = .data$value)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ .data$metric, scales = "free_y", ncol = 1) +
    ggplot2::scale_x_continuous(breaks = m$k) +
    ggplot2::labs(x = "Number of factions (k)", y = NULL,
                  title = "k-Selection Diagnostics") +
    ggplot2::theme_minimal() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"))

  p
}
