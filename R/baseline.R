# CLAIM: C1, C2 — Standard RCA introduces Compression Error
# METHOD: E4 — Standard RCA baseline for comparison

#' Standard RCA Baseline
#'
#' Implements the standard RCA pipeline with the absolute-value
#' transformation for direct comparison with tRCA. This is the method
#' that introduces the "Compression Error" by computing |r_ij| before
#' clustering.
#'
#' @param data Numeric matrix of respondents (rows) x items (columns).
#' @param k Optional number of clusters. If `NULL`, uses eigengap on the
#'   unsigned Laplacian.
#' @param nstart Number of random starts for k-means. Default 50.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#'
#' @return A list with components:
#'   \item{clusters}{Integer vector of cluster assignments.}
#'   \item{k}{Number of clusters used.}
#'   \item{W_abs}{Absolute-value correlation matrix.}
#'   \item{embedding}{Spectral embedding from unsigned Laplacian.}
#' @examples
#' sim <- sim_bipolar(n_per_faction = 40)
#' fit <- rca_baseline(sim$data)
#' table(fit$clusters, sim$truth)
#' @export
rca_baseline <- function(data, k = NULL, nstart = 50, seed = NULL) {
  data_mat <- .as_response_matrix(data)
  k <- .check_k(k, nrow(data_mat))

  # 1. The RCA "Compression Error" Step
  # Calculate raw correlations and immediately take the absolute value
  W_raw <- cor(t(data_mat), use = "pairwise.complete.obs")
  W_abs <- abs(W_raw)
  diag(W_abs) <- 0
  
  # 2. Standard (Unsigned) Normalized Laplacian
  D_vec <- rowSums(W_abs)
  
  # Handle isolated nodes gracefully
  D_inv_sqrt <- rep(0, length(D_vec))
  valid_nodes <- D_vec > 1e-10
  D_inv_sqrt[valid_nodes] <- 1 / sqrt(D_vec[valid_nodes])
  
  D_inv_sqrt_mat <- diag(D_inv_sqrt)
  I <- diag(nrow(W_abs))
  
  # L = I - D^{-1/2} |W| D^{-1/2}
  L_unsigned <- I - (D_inv_sqrt_mat %*% W_abs %*% D_inv_sqrt_mat)
  
  # 3. Spectral Decomposition
  decomp <- eigen(L_unsigned, symmetric = TRUE)
  n <- nrow(W_abs)
  eigenvalues <- rev(decomp$values)
  
  # 4. K-Selection (Eigengap on the Unsigned Laplacian)
  if (is.null(k)) {
    # We can reuse the eigengap function since standard Laplacians 
    # also look for gaps in the smallest eigenvalues
    egap <- eigengap_select_k(eigenvalues)
    k <- egap$k
  }
  
  # 5. Embedding & Hypersphere Projection
  raw_vectors <- decomp$vectors[, n:(n - k + 1), drop = FALSE]
  row_norms <- sqrt(rowSums(raw_vectors^2))
  row_norms[row_norms < 1e-10] <- 1e-10
  embedding <- raw_vectors / row_norms
  
  # 6. K-Means
  if (!is.null(seed)) set.seed(seed)
  km <- kmeans(embedding, centers = k, nstart = nstart)
  
  list(
    clusters = km$cluster,
    k = k,
    W_abs = W_abs,
    embedding = embedding
  )
}

#' RCA Baseline with Leiden Community Detection
#'
#' Implements the RCA pipeline using Leiden community detection on |W|
#' instead of spectral clustering. This is closer to how RCA is used
#' in practice (modularity-based algorithms like Leiden/Louvain or
#' Newman's leading eigenvector). Provides a stronger baseline than
#' spectral clustering on the unsigned Laplacian.
#'
#' @param data Numeric matrix of respondents (rows) x items (columns).
#' @param resolution Resolution parameter for Leiden. Higher values
#'   produce more communities. Default 1.0.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#'
#' @return A list with components:
#'   \item{clusters}{Integer vector of cluster assignments.}
#'   \item{k}{Number of clusters found.}
#'   \item{W_abs}{Absolute-value correlation matrix.}
#'   \item{method}{"leiden"}
#' @examples
#' sim <- sim_bipolar(n_per_faction = 40)
#' fit <- rca_leiden(sim$data)
#' table(fit$clusters, sim$truth)
#' @export
rca_leiden <- function(data, resolution = 1.0, seed = NULL) {
  data_mat <- .as_response_matrix(data)

  # 1. The RCA "Compression Error" Step
  W_raw <- cor(t(data_mat), use = "pairwise.complete.obs")
  W_abs <- abs(W_raw)
  diag(W_abs) <- 0

  # 2. Build graph from |W| and run Leiden
  g <- igraph::graph_from_adjacency_matrix(W_abs, mode = "undirected",
                                            weighted = TRUE)
  if (!is.null(seed)) set.seed(seed)
  membership <- igraph::cluster_leiden(g, resolution_parameter = resolution,
                                       objective_function = "modularity")$membership

  list(
    clusters = membership,
    k = length(unique(membership)),
    W_abs = W_abs,
    method = "leiden"
  )
}