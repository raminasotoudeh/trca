# CLAIM: C2, C3, C4 — Simulated datasets for validating tRCA topology recovery
# METHOD: D1–D4 — Profile-based data-generating processes

# --- Internal helper ---------------------------------------------------

#' Generate respondents from a profile-based DGP
#'
#' Each respondent draws an intensity from Uniform(0.5, 2.0) and produces
#' responses as `intensity * signal * profile + noise`.
#'
#' @param n Number of respondents.
#' @param profile Numeric vector of item loadings (length p).
#' @param signal Signal multiplier. Default 2.0.
#' @param noise_sd Standard deviation of Gaussian noise. Default 0.3.
#' @return An n x p numeric matrix of simulated responses.
#' @keywords internal
gen_group <- function(n, profile, signal = 2.0, noise_sd = 0.3) {
  intensities <- runif(n, 0.5, 2.0)
  t(sapply(intensities, function(a) {
    a * signal * profile + rnorm(length(profile), 0, noise_sd)
  }))
}

# --- Exported generators -----------------------------------------------

#' Simulate Bipolar Opposition Data
#'
#' Generates a respondent x item matrix with two factions that hold
#' mirror-image positions on all attitude dimensions (D3).
#'
#' @param n_per_faction Number of respondents per faction. Default 80.
#' @param p Number of attitude items. Default 20.
#' @param signal Signal strength multiplier. Default 2.0.
#' @param noise_sd Noise standard deviation. Default 0.3.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#'
#' @return A list with:
#'   \item{data}{Numeric matrix (2 * n_per_faction) x p.}
#'   \item{truth}{Integer vector of ground truth labels (1 or 2).}
#'   \item{profiles}{List of profile vectors used for each faction.}
#'   \item{scenario}{"bipolar"}
#' @examples
#' sim <- sim_bipolar(n_per_faction = 20)
#' dim(sim$data)
#' table(sim$truth)
#' @export
sim_bipolar <- function(n_per_faction = 80, p = 20, signal = 2.0,
                        noise_sd = 0.3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  prof <- rep(c(1, -1), length.out = p)
  X <- rbind(
    gen_group(n_per_faction, prof, signal, noise_sd),
    gen_group(n_per_faction, -prof, signal, noise_sd)
  )
  list(
    data = X,
    truth = rep(1:2, each = n_per_faction),
    profiles = list(faction_1 = prof, faction_2 = -prof),
    scenario = "bipolar"
  )
}

#' Simulate Consensus Data
#'
#' Generates a respondent x item matrix with a single faction where all
#' respondents share the same attitudinal direction (D4).
#'
#' @inheritParams sim_bipolar
#' @param n Total number of respondents. Default 80.
#'
#' @return A list with:
#'   \item{data}{Numeric matrix n x p.}
#'   \item{truth}{Integer vector of 1s (single faction).}
#'   \item{profiles}{List with the single profile vector.}
#'   \item{scenario}{"consensus"}
#' @examples
#' sim <- sim_consensus(n = 40)
#' dim(sim$data)
#' @export
sim_consensus <- function(n = 80, p = 20, signal = 2.0,
                          noise_sd = 0.3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  prof <- rep(c(1, -1), length.out = p)
  X <- gen_group(n, prof, signal, noise_sd)
  list(
    data = X,
    truth = rep(1L, n),
    profiles = list(faction_1 = prof),
    scenario = "consensus"
  )
}

#' Simulate Triadic Impasse Data
#'
#' Generates a respondent x item matrix with three factions forming a
#' frustrated triangle — each faction opposes the other two on different
#' item dimensions (D1).
#'
#' Items are divided into three blocks of equal size. Each faction is
#' positive (+1) on its own block and negative (-1) on the other two,
#' producing all-negative inter-faction correlations.
#'
#' @inheritParams sim_bipolar
#' @param items_per_block Number of items per block. Default 7 (total p = 21).
#'
#' @return A list with:
#'   \item{data}{Numeric matrix (3 * n_per_faction) x (3 * items_per_block).}
#'   \item{truth}{Integer vector of ground truth labels (1, 2, or 3).}
#'   \item{profiles}{List of profile vectors for each faction.}
#'   \item{scenario}{"triadic_impasse"}
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 20)
#' dim(sim$data)
#' table(sim$truth)
#' @export
sim_triadic_impasse <- function(n_per_faction = 80, items_per_block = 7,
                                signal = 2.0, noise_sd = 0.3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  b <- items_per_block
  pA <- c(rep(1, b), rep(-1, b), rep(-1, b))
  pB <- c(rep(-1, b), rep(1, b), rep(-1, b))
  pC <- c(rep(-1, b), rep(-1, b), rep(1, b))
  X <- rbind(
    gen_group(n_per_faction, pA, signal, noise_sd),
    gen_group(n_per_faction, pB, signal, noise_sd),
    gen_group(n_per_faction, pC, signal, noise_sd)
  )
  list(
    data = X,
    truth = rep(1:3, each = n_per_faction),
    profiles = list(faction_1 = pA, faction_2 = pB, faction_3 = pC),
    scenario = "triadic_impasse"
  )
}

#' Simulate Hub-and-Spoke Data
#'
#' Generates a respondent x item matrix with a center faction and two
#' peripheral factions that oppose the center on orthogonal dimensions (D2).
#'
#' Items are split into two halves (dim1 and dim2). The center is positive
#' on both. The left faction opposes on dim1 (zero on dim2). The right
#' faction opposes on dim2 (zero on dim1). This produces a block matrix
#' where the center is negative to both peripherals, but peripherals are
#' orthogonal (near-zero) to each other.
#'
#' @inheritParams sim_bipolar
#'
#' @return A list with:
#'   \item{data}{Numeric matrix (3 * n_per_faction) x p.}
#'   \item{truth}{Integer vector of ground truth labels (1, 2, or 3).}
#'   \item{profiles}{List of profile vectors for center, left, right.}
#'   \item{scenario}{"hub_and_spoke"}
#' @examples
#' sim <- sim_hub_and_spoke(n_per_faction = 20)
#' dim(sim$data)
#' table(sim$truth)
#' @export
sim_hub_and_spoke <- function(n_per_faction = 80, p = 20, signal = 2.0,
                              noise_sd = 0.3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  half <- p %/% 2
  dim1 <- rep(c(1, -1), length.out = half)
  dim2 <- rep(c(1, -1), length.out = half)
  p_center <- c(dim1, dim2)
  p_left   <- c(-dim1, rep(0, half))
  p_right  <- c(rep(0, half), -dim2)
  X <- rbind(
    gen_group(n_per_faction, p_center, signal, noise_sd),
    gen_group(n_per_faction, p_left, signal, noise_sd),
    gen_group(n_per_faction, p_right, signal, noise_sd)
  )
  list(
    data = X,
    truth = rep(1:3, each = n_per_faction),
    profiles = list(center = p_center, left = p_left, right = p_right),
    scenario = "hub_and_spoke"
  )
}

#' Simulate Multipolar Opposition Data
#'
#' Generalizes the triadic impasse to k mutually opposing factions.
#' Items are divided into k blocks; each faction is positive (+1) on its
#' own block and negative (`-1/(k-1)`) on all others. The continuous
#' negative loading ensures that pairwise profile dot products remain
#' negative for any k, producing a block matrix where ALL off-diagonals
#' are negative (frustrated k-polar structure).
#'
#' When k=2 this reduces to bipolar opposition; when k=3 it is equivalent
#' to the triadic impasse.
#'
#' @param k Number of factions. Default 4.
#' @param n_per_faction Number of respondents per faction. Default 80.
#' @param items_per_block Number of items per block. Default 5
#'   (total p = k * items_per_block).
#' @param signal Signal strength multiplier. Default 2.0.
#' @param noise_sd Noise standard deviation. Default 0.3.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#'
#' @return A list with:
#'   \item{data}{Numeric matrix (k * n_per_faction) x (k * items_per_block).}
#'   \item{truth}{Integer vector of ground truth labels (1 to k).}
#'   \item{profiles}{List of profile vectors for each faction.}
#'   \item{scenario}{Character string `"multipolar_k<k>"`.}
#' @examples
#' sim <- sim_multipolar(k = 4, n_per_faction = 20)
#' dim(sim$data)
#' table(sim$truth)
#' @export
sim_multipolar <- function(k = 4, n_per_faction = 80, items_per_block = 5,
                           signal = 2.0, noise_sd = 0.3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  b <- items_per_block
  p <- k * b

  # Centred simplex profiles: faction i is +1 on its block,
  # -1/(k-1) on all other blocks. This ensures every pair of profiles
  # has a negative dot product regardless of k.
  neg_val <- -1 / (k - 1)
  profiles <- list()
  data_blocks <- list()
  for (i in seq_len(k)) {
    prof <- rep(neg_val, p)
    prof[((i - 1) * b + 1):(i * b)] <- 1
    profiles[[paste0("faction_", i)]] <- prof
    data_blocks[[i]] <- gen_group(n_per_faction, prof, signal, noise_sd)
  }

  X <- do.call(rbind, data_blocks)
  list(
    data = X,
    truth = rep(seq_len(k), each = n_per_faction),
    profiles = profiles,
    scenario = paste0("multipolar_k", k)
  )
}

#' Simulate Two-Bloc Coalition Data
#'
#' Creates k factions organized into two opposing blocs of k/2 factions each.
#' Factions within a bloc share a primary ideological direction but differ on
#' secondary "identity" dimensions. Factions across blocs oppose on the primary
#' dimension.
#'
#' The block matrix shows positive within-bloc correlations and negative
#' between-bloc correlations — like bipolar opposition with internal
#' sub-structure.
#'
#' @param k Number of factions (must be even). Default 4.
#' @param n_per_faction Number of respondents per faction. Default 80.
#' @param items_per_block Number of items per identity block. Default 5.
#' @param signal Signal strength multiplier. Default 2.0.
#' @param noise_sd Noise standard deviation. Default 0.3.
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#'
#' @return A list with:
#'   \item{data}{Numeric matrix (k * n_per_faction) x p.}
#'   \item{truth}{Integer vector of ground truth labels (1 to k).}
#'   \item{profiles}{List of profile vectors for each faction.}
#'   \item{scenario}{Character string `"coalition_k<k>"`.}
#' @examples
#' sim <- sim_coalition(k = 4, n_per_faction = 20)
#' dim(sim$data)
#' table(sim$truth)
#' @export
sim_coalition <- function(k = 4, n_per_faction = 80, items_per_block = 5,
                          signal = 2.0, noise_sd = 0.3, seed = NULL) {
  if (k %% 2 != 0) stop("k must be even for coalition structure")
  if (!is.null(seed)) set.seed(seed)
  b <- items_per_block
  half_k <- k %/% 2

  # Items layout:
  # [primary_items | identity_block_1 | identity_block_2 | ... | identity_block_k]
  # Primary items: b items that define bloc membership (+1 vs -1)
  # Identity items: b items per faction, unique to each faction
  p_primary <- b
  p_identity <- k * b
  p <- p_primary + p_identity

  primary_A <- rep(c(1, -1), length.out = p_primary)   # Bloc A direction
  primary_B <- -primary_A                               # Bloc B direction

  profiles <- list()
  data_blocks <- list()
  for (i in seq_len(k)) {
    # Primary dimension: first half_k factions are Bloc A, rest are Bloc B
    if (i <= half_k) {
      primary <- primary_A
    } else {
      primary <- primary_B
    }

    # Identity dimension: +1 on own block, 0 elsewhere
    identity <- rep(0, p_identity)
    identity[((i - 1) * b + 1):(i * b)] <- 1

    prof <- c(primary, identity)
    profiles[[paste0("faction_", i)]] <- prof
    data_blocks[[i]] <- gen_group(n_per_faction, prof, signal, noise_sd)
  }

  X <- do.call(rbind, data_blocks)
  list(
    data = X,
    truth = rep(seq_len(k), each = n_per_faction),
    profiles = profiles,
    scenario = paste0("coalition_k", k)
  )
}
