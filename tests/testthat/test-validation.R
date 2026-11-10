# Validation target tests (D1-D4)
# These verify that tRCA recovers the correct topology for each scenario
# and that RCA fails where expected.

fit_both <- function(sim, k = NULL) {
  f <- trca(sim$data, k = k, seed = 42, verbose = FALSE)
  r <- rca_baseline(sim$data, k = k, seed = 42)
  list(
    trca = f,
    trca_ari = adjusted_rand_index(f$clusters, sim$truth),
    trca_topo = topology(f, data = sim$data),
    rca = r,
    rca_ari = adjusted_rand_index(r$clusters, sim$truth)
  )
}

test_that("D3: tRCA recovers Bipolar Opposition", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- fit_both(sim)
  expect_equal(res$trca$k, 2)
  expect_gt(res$trca_ari, 0.9)
  expect_equal(res$trca_topo$topologies[[1]]$type, "Bipolar Opposition")
  # Block matrix should have negative off-diagonal
  expect_lt(res$trca_topo$B[1, 2], 0)
})

test_that("D3: RCA fails on bipolar (compression error)", {
  sim <- sim_bipolar(n_per_faction = 80, p = 20, seed = 42)
  res <- fit_both(sim)
  # RCA's |r| makes opposing factions (r ≈ -1) look identical to aligned
  # factions (r ≈ +1), so it cannot distinguish the two groups.
  expect_lt(res$rca_ari, 0.5)
})

test_that("D4: Consensus with a single faction is classified as Consensus", {
  sim <- sim_consensus(n = 80, p = 20, seed = 42)
  res <- trca(sim$data, k = 1, seed = 42, verbose = FALSE)
  topo <- topology(res, data = sim$data)
  expect_equal(topo$topologies[[1]]$type, "Consensus")
})

test_that("D4: automatic k recognizes consensus as one faction", {
  sim <- sim_consensus(n = 80, p = 20, seed = 42)
  res <- trca(sim$data, seed = 42, verbose = FALSE)
  expect_equal(res$k, 1)
  expect_equal(res$k_method, "one_faction_test")
  expect_true(res$one_faction_test$one_faction)
  topo <- topology(res, data = sim$data)
  expect_equal(topo$topologies[[1]]$type, "Consensus")
})

test_that("D4: min_k = 2 skips the one-faction test", {
  sim <- sim_consensus(n = 80, p = 20, seed = 42)
  res <- trca(sim$data, min_k = 2, seed = 42, verbose = FALSE)
  expect_gte(res$k, 2)
  expect_null(res$one_faction_test)
})

test_that("one-faction test keeps structured scenarios split", {
  sims <- list(
    sim_bipolar(n_per_faction = 80, p = 20, seed = 42),
    sim_triadic_impasse(n_per_faction = 80, items_per_block = 7, seed = 42),
    sim_hub_and_spoke(n_per_faction = 80, p = 20, seed = 42),
    sim_multipolar(seed = 42),
    sim_coalition(seed = 42)
  )
  for (sim in sims) {
    res <- trca(sim$data, seed = 42, verbose = FALSE)
    expect_false(res$one_faction_test$one_faction)
    expect_equal(res$k, length(unique(sim$truth)))
  }
})

test_that("D1: tRCA recovers Triadic Impasse", {
  sim <- sim_triadic_impasse(n_per_faction = 80, items_per_block = 7, seed = 42)
  res <- fit_both(sim)
  expect_equal(res$trca$k, 3)
  expect_gt(res$trca_ari, 0.9)
  expect_equal(res$trca_topo$topologies[[1]]$type, "Triadic Impasse")
  off_diag <- res$trca_topo$B[upper.tri(res$trca_topo$B)]
  expect_true(all(off_diag < 0))
})

test_that("D1: RCA fails on triadic impasse (compression error)", {
  sim <- sim_triadic_impasse(n_per_faction = 80, items_per_block = 7, seed = 42)
  res <- fit_both(sim)
  # With automatic k, RCA should get the wrong k or a poor partition
  rca_correct <- res$rca$k == 3 && res$rca_ari > 0.9
  expect_false(rca_correct)
})

test_that("D2: tRCA recovers Hub-and-Spoke", {
  sim <- sim_hub_and_spoke(n_per_faction = 80, p = 20, seed = 42)
  res <- fit_both(sim, k = 3)
  expect_gt(res$trca_ari, 0.9)
  expect_equal(res$trca_topo$topologies[[1]]$type, "Hub-and-Spoke")
})

test_that("automatic k is correct with few items", {
  # With few items, most of the max_k embedding dimensions are noise; the
  # silhouette search must run in the signal dimensions only.
  sims <- list(
    sim_bipolar(n_per_faction = 30, p = 8, seed = 1),
    sim_bipolar(n_per_faction = 60, p = 10, noise_sd = 1, seed = 2),
    sim_triadic_impasse(n_per_faction = 60, items_per_block = 2, seed = 3),
    sim_multipolar(k = 4, n_per_faction = 60, items_per_block = 3, seed = 4)
  )
  for (sim in sims) {
    res <- trca(sim$data, seed = 42, verbose = FALSE)
    true_k <- length(unique(sim$truth))
    expect_equal(res$k, true_k)
    expect_equal(res$signal_dims, true_k - 1)
    expect_gt(adjusted_rand_index(res$clusters, sim$truth), 0.9)
  }
})

test_that("signal_dims is NULL when k is supplied", {
  sim <- sim_bipolar(n_per_faction = 30, p = 10, seed = 1)
  expect_null(trca(sim$data, k = 2, seed = 1, verbose = FALSE)$signal_dims)
})
