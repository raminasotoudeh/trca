# Input validation shared by all functions that take respondent x item data

likert_data <- function() {
  sim <- sim_bipolar(n_per_faction = 30, p = 8, seed = 1)
  X <- round(pmin(pmax(sim$data + 3, 1), 5))
  colnames(X) <- paste0("q", seq_len(ncol(X)))
  list(X = X, df = as.data.frame(X), truth = sim$truth)
}

# A haven-labelled column without depending on haven
fake_labelled <- function(x, labels) {
  structure(as.double(x), labels = labels,
            class = c("haven_labelled", "vctrs_vctr", "double"))
}

test_that("data frames of numeric columns match the matrix", {
  d <- likert_data()
  expect_equal(unname(.as_response_matrix(d$df)), unname(d$X))
  expect_equal(colnames(.as_response_matrix(d$df)), colnames(d$X))
})

test_that("unordered factors and character columns are rejected by name", {
  d <- likert_data()
  f <- d$df
  f$q2 <- factor(f$q2)
  f$q5 <- as.character(f$q5)
  expect_error(.as_response_matrix(f), "Not numeric: q2, q5")
  expect_error(trca(f, verbose = FALSE), "must contain only numeric columns")
  expect_error(rca_baseline(f), "must contain only numeric columns")
  expect_error(.as_response_matrix(matrix(letters[1:6], 2)), "numeric matrix")
  expect_error(.as_response_matrix(1:10), "matrix or data frame")
})

test_that("ordered factors use their integer codes", {
  d <- likert_data()
  o <- d$df
  o$q1 <- factor(o$q1, levels = 1:5, ordered = TRUE)
  expect_equal(.as_response_matrix(o), .as_response_matrix(d$df))
})

test_that("haven-labelled columns keep their codes", {
  d <- likert_data()
  h <- d$df
  h$q1 <- fake_labelled(h$q1, c(Disagree = 1, Agree = 5))
  expect_equal(.as_response_matrix(h), .as_response_matrix(d$df))
  fit <- trca(h, k = 2, seed = 1, verbose = FALSE)
  expect_equal(fit$k, 2)
})

test_that("check_data flags negative codes in labelled columns", {
  d <- likert_data()
  h <- d$df
  q3 <- h$q3
  q3[1:3] <- -9
  h$q3 <- fake_labelled(q3, c(Refused = -9, Agree = 5))
  res <- suppressMessages(check_data(h))
  expect_equal(res$negative_coded, "q3")
  expect_match(paste(capture_messages(check_data(h)), collapse = ""), "missing-data codes")
  expect_length(suppressMessages(check_data(d$df))$negative_coded, 0)
})

test_that("missing values are treated as absent edges", {
  d <- likert_data()
  X <- d$X
  set.seed(2)
  X[cbind(sample(nrow(X), 15), sample(ncol(X), 15, replace = TRUE))] <- NA
  fit <- trca(X, k = 2, seed = 1, verbose = FALSE)
  expect_false(anyNA(fit$clusters))
  expect_gt(adjusted_rand_index(fit$clusters, d$truth), 0.9)
  auto <- trca(X, seed = 1, verbose = FALSE)
  expect_false(anyNA(auto$clusters))
  B <- trca_block_density(X, fit$clusters)
  expect_false(anyNA(B))
})

test_that("respondents or items with no responses are rejected", {
  d <- likert_data()
  X <- d$X
  X[4, ] <- NA
  expect_error(trca(X, verbose = FALSE), "1 respondent\\(s\\) with no observed responses \\(rows 4\\)")
  X <- d$X
  X[, "q6"] <- NA
  expect_error(trca(X, verbose = FALSE), "item\\(s\\) with no observed responses \\(q6\\)")
})

test_that("infinite values and tiny inputs are rejected", {
  d <- likert_data()
  X <- d$X
  X[1, 1] <- Inf
  expect_error(trca(X, verbose = FALSE), "infinite")
  expect_error(trca(d$X[, 1:2], verbose = FALSE), "at least 3 items")
  expect_error(trca(d$X[1, , drop = FALSE], verbose = FALSE), "at least 2 respondents")
})

test_that("zero-variance items do not break the fit", {
  d <- likert_data()
  X <- d$X
  X[, 3] <- 4
  fit <- trca(X, k = 2, seed = 1, verbose = FALSE)
  expect_false(anyNA(fit$respondent_embedding))
  expect_gt(adjusted_rand_index(fit$clusters, d$truth), 0.9)
  expect_match(paste(capture_messages(check_data(X)), collapse = ""), "zero variance")
})

test_that("k is validated", {
  d <- likert_data()
  expect_error(trca(d$X, k = 0, verbose = FALSE), "positive whole number")
  expect_error(trca(d$X, k = 2.5, verbose = FALSE), "positive whole number")
  expect_error(trca(d$X[1:3, ], k = 5, verbose = FALSE), "cannot exceed")
  expect_error(rca_baseline(d$X[1:3, ], k = 5), "cannot exceed")
})

test_that("cluster labels must match the data", {
  d <- likert_data()
  expect_error(trca_block_density(d$X, d$truth[-1]), "length 59")
  cl <- d$truth
  cl[1] <- NA
  expect_error(trca_block_density(d$X, cl), "missing values")
})
