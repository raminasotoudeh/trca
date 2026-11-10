# --- Input validation --------------------------------------------------

#' Coerce Survey Responses to a Numeric Matrix
#'
#' Shared input check for every function that takes respondent x item data.
#' Accepts a numeric matrix or a data frame whose columns are numeric,
#' logical, ordered factors (converted to their integer codes), or
#' haven-labelled numerics (labels dropped, codes kept). Unordered factors
#' and character columns are rejected because their numeric codes carry no
#' order. Missing values are allowed (tRCA treats them as absent edges), but
#' a respondent or item with no observed responses is rejected.
#'
#' @param data Respondent x item data.
#' @param arg Argument name used in error messages.
#' @return A numeric matrix with the original row and column names.
#' @keywords internal
.as_response_matrix <- function(data, arg = "data") {
  if (is.data.frame(data)) {
    cols <- names(data)
    if (is.null(cols)) cols <- paste0("V", seq_along(data))
    bad <- character(0)
    out <- vector("list", length(data))
    for (j in seq_along(data)) {
      col <- data[[j]]
      if (inherits(col, "haven_labelled")) {
        col <- as.vector(unclass(col))
        attributes(col) <- NULL
      }
      if (is.ordered(col)) {
        out[[j]] <- as.integer(col)
      } else if (is.numeric(col) || is.logical(col)) {
        out[[j]] <- as.numeric(col)
      } else {
        bad <- c(bad, cols[j])
      }
    }
    if (length(bad) > 0) {
      stop(sprintf(
        "`%s` must contain only numeric columns. Not numeric: %s.\n%s",
        arg, paste(bad, collapse = ", "),
        "Convert Likert items with as.integer(), or use an ordered factor."
      ), call. = FALSE)
    }
    data_mat <- matrix(unlist(out, use.names = FALSE), nrow = nrow(data),
                       dimnames = list(rownames(data), names(data)))
    if (is.null(names(data))) colnames(data_mat) <- NULL
  } else if (is.matrix(data)) {
    if (!(is.numeric(data) || is.logical(data))) {
      stop(sprintf("`%s` must be a numeric matrix, not a %s matrix.",
                   arg, typeof(data)), call. = FALSE)
    }
    data_mat <- data
    storage.mode(data_mat) <- "double"
  } else {
    stop(sprintf(
      "`%s` must be a numeric matrix or data frame (respondents x items).",
      arg), call. = FALSE)
  }

  if (nrow(data_mat) < 2 || ncol(data_mat) < 2) {
    stop(sprintf("`%s` needs at least 2 respondents and 2 items (got %d x %d).",
                 arg, nrow(data_mat), ncol(data_mat)), call. = FALSE)
  }
  if (any(is.infinite(data_mat))) {
    stop(sprintf("`%s` contains infinite values.", arg), call. = FALSE)
  }
  empty_rows <- which(rowSums(!is.na(data_mat)) == 0)
  if (length(empty_rows) > 0) {
    stop(sprintf(
      "`%s` has %d respondent(s) with no observed responses (rows %s). Remove them first.",
      arg, length(empty_rows), .format_ids(empty_rows)), call. = FALSE)
  }
  empty_cols <- which(colSums(!is.na(data_mat)) == 0)
  if (length(empty_cols) > 0) {
    ids <- if (is.null(colnames(data_mat))) empty_cols else colnames(data_mat)[empty_cols]
    stop(sprintf(
      "`%s` has %d item(s) with no observed responses (%s). Remove them first.",
      arg, length(empty_cols), .format_ids(ids)), call. = FALSE)
  }
  data_mat
}

#' Check a Number of Clusters
#'
#' @param k Requested number of clusters, or `NULL`.
#' @param n Number of respondents.
#' @return `k` as an integer, or `NULL`.
#' @keywords internal
.check_k <- function(k, n) {
  if (is.null(k)) return(NULL)
  if (!is.numeric(k) || length(k) != 1 || is.na(k) || k != round(k) || k < 1) {
    stop("`k` must be a single positive whole number.", call. = FALSE)
  }
  if (k > n) {
    stop(sprintf("`k` (%d) cannot exceed the number of respondents (%d).", k, n),
         call. = FALSE)
  }
  as.integer(k)
}

#' Check Cluster Labels Against Data
#'
#' @param clusters Cluster assignment vector.
#' @param n Number of respondents in the data.
#' @keywords internal
.check_clusters <- function(clusters, n) {
  if (length(clusters) != n) {
    stop(sprintf(
      "`clusters` has length %d but the data have %d respondents.",
      length(clusters), n), call. = FALSE)
  }
  if (anyNA(clusters)) {
    stop("`clusters` must not contain missing values.", call. = FALSE)
  }
  invisible(clusters)
}

# Shorten a list of row/column identifiers for error messages
.format_ids <- function(ids, max_show = 5) {
  shown <- paste(utils::head(ids, max_show), collapse = ", ")
  if (length(ids) > max_show) paste0(shown, ", ...") else shown
}
