# Tests for tRCA (Signed Bipartite Spectral Embedding)

# --- trca basic functionality ---

test_that("trca recovers bipolar structure", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_s3_class(res, "trca")
  expect_equal(res$k, 2)
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

test_that("trca recovers triadic impasse", {
  sim <- sim_triadic_impasse(n_per_faction = 80, items_per_block = 7, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_equal(res$k, 3)
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

test_that("trca recovers hub-and-spoke with forced k", {
  sim <- sim_hub_and_spoke(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, k = 3, verbose = FALSE, seed = 42)
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

test_that("trca recovers multipolar structure (k=4)", {
  sim <- sim_multipolar(k = 4, n_per_faction = 80, items_per_block = 5, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_equal(res$k, 4)
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

test_that("trca recovers coalition structure (k=4)", {
  sim <- sim_coalition(k = 4, n_per_faction = 80, items_per_block = 5, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

# --- centering modes ---

test_that("trca supports both centering modes", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)

  res_mean <- trca(sim$data, centering = "mean", verbose = FALSE, seed = 42)
  expect_equal(res_mean$centering, "mean")
  ari_mean <- adjusted_rand_index(res_mean$clusters, sim$truth)
  expect_gt(ari_mean, 0.9)

  res_mid <- trca(sim$data, centering = "midpoint", verbose = FALSE, seed = 42)
  expect_equal(res_mid$centering, "midpoint")
  ari_mid <- adjusted_rand_index(res_mid$clusters, sim$truth)
  expect_gt(ari_mid, 0.9)
})

# --- k-selection methods ---

test_that("trca silhouette selects correct k", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, method = "silhouette", verbose = FALSE, seed = 42)
  expect_equal(res$k, 2)
  expect_equal(res$k_method, "silhouette")
  expect_true(res$k_auto)
})

test_that("trca kmeans/eigengap selects correct k", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, method = "kmeans", verbose = FALSE, seed = 42)
  expect_equal(res$k, 2)
  expect_equal(res$k_method, "eigengap")
  expect_true(res$k_auto)
})

test_that("trca respects user-specified k", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, k = 3, verbose = FALSE, seed = 42)
  expect_equal(res$k, 3)
  expect_equal(res$k_method, "user")
  expect_false(res$k_auto)
})

# --- item filtering ---

test_that("trca with embedding filter keeps all items on clean data", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, filter_items = TRUE, verbose = FALSE, seed = 42)
  # On clean bipolar data, most items should be kept
  expect_true(!is.null(res$items_kept))
  expect_true(!is.null(res$item_scores))
  # Should still recover structure
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

test_that("trca with variance filter works", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, filter_items = "variance", verbose = FALSE, seed = 42)
  expect_true(!is.null(res$items_kept))
  ari <- adjusted_rand_index(res$clusters, sim$truth)
  expect_gt(ari, 0.9)
})

test_that("trca without filtering has NULL items_kept", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, filter_items = FALSE, verbose = FALSE, seed = 42)
  expect_null(res$items_kept)
  expect_null(res$items_dropped)
  expect_null(res$item_scores)
})

test_that("trca caps k when items are heavily filtered", {
  # Create data with very few items so filtering can push below k
  sim <- sim_bipolar(n_per_faction = 40, p = 5, seed = 42)
  # Should not error even if filter removes items
  expect_no_error(
    res <- trca(sim$data, filter_items = TRUE, verbose = FALSE, seed = 42)
  )
})

# --- return structure ---

test_that("trca returns all expected components", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_true(all(c("clusters", "k", "k_auto", "k_method", "centering",
                     "respondent_embedding", "item_embedding", "eigenvalues",
                     "singular_values", "eigengap", "X_centered") %in% names(res)))
  expect_equal(length(res$clusters), 160)
  expect_equal(nrow(res$respondent_embedding), 160)
  expect_equal(ncol(res$respondent_embedding), res$k)
  expect_equal(nrow(res$item_embedding), 20)
  expect_equal(ncol(res$item_embedding), res$k)
})

# --- trca_block_density ---

test_that("trca_block_density produces correct shape", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  B <- trca_block_density(sim$data, res$clusters)
  expect_equal(dim(B), c(2, 2))
  expect_true(all(diag(B) == 1))
  expect_true(!is.null(rownames(B)))
})

test_that("trca_block_density with midpoint centering avoids artifact", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  # With mean centering at k=2, profile cor is always -1 (artifact)
  B_mean <- trca_block_density(sim$data, res$clusters, centering = "mean")
  # With midpoint centering, value should reflect true opposition
  B_mid <- trca_block_density(sim$data, res$clusters, centering = "midpoint")
  # Both should show negative off-diagonal (bipolar)
  expect_lt(B_mid[1, 2], 0)
})

test_that("trca_block_density shows agreement when consensus is forced to split", {
  sim <- sim_consensus(n = 80, p = 20, seed = 42)
  res <- trca(sim$data, min_k = 2, verbose = FALSE, seed = 42)
  B <- trca_block_density(sim$data, res$clusters, centering = "midpoint")
  off_diag <- B[upper.tri(B)]
  expect_gt(length(off_diag), 0)
  expect_true(all(off_diag > 0))
})

# --- trca_item_diagnostics ---

test_that("trca_item_diagnostics returns correct structure", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  diag_df <- trca_item_diagnostics(res, data = sim$data)
  expect_s3_class(diag_df, "data.frame")
  expect_true(all(c("item", "item_var", "embedding_norm", "eta_sq") %in%
                    names(diag_df)))
  expect_equal(nrow(diag_df), 20)
  # All eta_sq should be between 0 and 1
  expect_true(all(diag_df$eta_sq >= 0 & diag_df$eta_sq <= 1, na.rm = TRUE))
})

test_that("trca_item_diagnostics embedding norms are positive", {
  sim <- sim_triadic_impasse(n_per_faction = 80, items_per_block = 7, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  diag_df <- trca_item_diagnostics(res)
  expect_true(all(diag_df$embedding_norm > 0))
})

# --- S3 class assignment ---

test_that("trca returns object with class 'trca'", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_s3_class(res, "trca")
})

test_that("topology() returns object with class 'trca_topology'", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_s3_class(topology(res, data = sim$data), "trca_topology")
})

# --- print/summary methods ---

test_that("print.trca produces output", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_output(print(res), "tRCA clustering")
})

test_that("print.trca_topology produces output", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_output(print(topology(res, data = sim$data)), "Topology map")
})

test_that("summary.trca produces output", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  expect_output(summary(res), "tRCA Clustering Summary")
})

# --- convenience accessors ---

test_that("topology() works for trca objects", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  topo <- topology(res)
  expect_s3_class(topo, "trca_topology")
})

test_that("block_matrix() works for trca objects", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  B <- block_matrix(res)
  expect_equal(dim(B), c(res$k, res$k))
})

test_that("item_importance() works for trca objects", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- trca(sim$data, verbose = FALSE, seed = 42)
  imp <- item_importance(res, data = sim$data)
  expect_s3_class(imp, "data.frame")
  expect_true("embedding_norm" %in% names(imp))
})

# --- check_data ---

test_that("check_data reports correct dimensions", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  result <- check_data(sim$data, verbose = FALSE)
  expect_equal(result$n, 80)
  expect_equal(result$m, 10)
  expect_equal(result$zero_var_rows, 0)
  expect_equal(result$zero_var_cols, 0)
})

test_that("check_data detects zero-variance rows", {
  mat <- matrix(rnorm(200), 20, 10)
  mat[1, ] <- 5  # constant row
  mat[2, ] <- 3  # constant row
  result <- check_data(mat, verbose = FALSE)
  expect_equal(result$zero_var_rows, 2)
})

test_that("check_data detects missing values", {
  mat <- matrix(rnorm(200), 20, 10)
  mat[1, 1] <- NA
  mat[5, 3] <- NA
  result <- check_data(mat, verbose = FALSE)
  expect_equal(result$pct_missing, 100 * 2 / 200)
})

# --- compare_methods ---

test_that("compare_methods runs and returns correct structure", {
  sim <- sim_bipolar(n_per_faction = 40, p = 10, seed = 42)
  comp <- compare_methods(sim$data, verbose = FALSE, seed = 42)
  expect_s3_class(comp, "trca_comparison")
  expect_true(all(c("trca", "rca") %in% names(comp$results)))
  expect_equal(nrow(comp$summary), 2)
  expect_output(print(comp), "Method comparison")
})

test_that("trca validates min_k and skips the one-faction test when k is given", {
  sim <- sim_bipolar(n_per_faction = 30, p = 10, seed = 1)
  expect_error(trca(sim$data, min_k = 0, verbose = FALSE), "min_k")
  res <- trca(sim$data, k = 2, seed = 1, verbose = FALSE)
  expect_null(res$one_faction_test)
  expect_equal(res$k_method, "user")
})

# --- consistency of defaults ---

test_that("block_matrix() and topology() agree and follow the fit's centering", {
  sim <- sim_triadic_impasse(n_per_faction = 40, items_per_block = 4, seed = 1)
  for (cen in c("mean", "midpoint")) {
    fit <- trca(sim$data, k = 3, centering = cen, seed = 1, verbose = FALSE)
    expect_equal(block_matrix(fit), topology(fit)$B)
    expect_equal(block_matrix(fit, data = sim$data),
                 topology(fit, data = sim$data)$B)
  }
  expect_error(block_matrix(fit, centering = "mean"), "Pass `data`")
  expect_equal(dim(block_matrix(fit, data = sim$data, centering = "mean")),
               c(3, 3))
})

test_that("compare_methods() uses trca()'s defaults", {
  sim <- sim_triadic_impasse(n_per_faction = 40, items_per_block = 4, seed = 1)
  comp <- compare_methods(sim$data, methods = "trca", seed = 1, verbose = FALSE)
  fit <- trca(sim$data, seed = 1, verbose = FALSE)
  expect_equal(comp$results$trca$clusters, fit$clusters)
})

# --- trca_select_k ---

test_that("trca_select_k() recommends the same k as trca()", {
  sim <- sim_triadic_impasse(n_per_faction = 60, items_per_block = 4, seed = 1)
  ks <- trca_select_k(sim$data, k_range = 2:8, nstart = 50, seed = 1,
                      verbose = FALSE)
  fit <- trca(sim$data, seed = 1, verbose = FALSE)
  expect_equal(ks$k_auto, 3)
  expect_equal(ks$k_auto, fit$k)
  expect_equal(ks$signal_dims, 2)
  expect_named(ks$metrics, c("k", "wcss", "bic"))
  expect_equal(ks$silhouette$k, 2:8)
  out <- capture.output(print(ks))
  expect_true(any(grepl("k = 3: highest silhouette", out)))
})

test_that("trca_select_k() reports k = 1 on consensus data", {
  sim <- sim_consensus(n = 80, p = 20, seed = 42)
  ks <- trca_select_k(sim$data, k_range = 2:6, seed = 42, verbose = FALSE)
  expect_equal(ks$k_auto, 1)
  expect_true(ks$one_faction_test$one_faction)
})
