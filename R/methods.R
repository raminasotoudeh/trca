# S3 methods for trca, trca_topology and trca_comparison objects
# Provides print, summary, plot, and convenience accessors

# ---- print methods --------------------------------------------------------

#' @export
print.trca <- function(x, ...) {
  n <- length(x$clusters)
  m <- ncol(x$X_centered)
  sizes <- table(x$clusters)
  cat(sprintf("tRCA clustering: %d respondents x %d items into %d faction%s\n",
              n, m, x$k, if (x$k == 1) "" else "s"))
  cat(sprintf("  k selection: %s", x$k_method))
  if (x$k_auto) cat(" (auto)") else cat(" (user-specified)")
  cat("\n")
  cat(sprintf("  Centering:   %s\n", x$centering))
  if (!is.null(x$items_dropped) && length(x$items_dropped) > 0) {
    cat(sprintf("  Items:       %d kept, %d dropped (embedding filter)\n",
                length(x$items_kept), length(x$items_dropped)))
  }
  cat(sprintf("  Faction sizes: %s\n",
              paste(sprintf("%d (n=%d)", as.integer(names(sizes)), sizes),
                    collapse = ", ")))
  invisible(x)
}

#' @export
print.trca_topology <- function(x, ...) {
  k <- nrow(x$B)
  cat(sprintf("Topology map: %d faction%s, %d game(s)\n", k,
              if (k == 1) "" else "s", x$games$n_games))
  for (g in names(x$topologies)) {
    topo <- x$topologies[[g]]
    cat(sprintf("  %s: %s (%s)\n", g, topo$type,
                paste(topo$factions, collapse = ", ")))
  }
  cat("\nBlock density matrix:\n")
  print(round(x$B, 3))
  invisible(x)
}

# ---- summary methods ------------------------------------------------------

#' @export
summary.trca <- function(object, ...) {
  x <- object
  n <- length(x$clusters)
  m <- ncol(x$X_centered)
  sizes <- table(x$clusters)
  cat(sprintf("tRCA Clustering Summary\n"))
  cat(sprintf("=======================\n"))
  cat(sprintf("Respondents: %d\n", n))
  cat(sprintf("Items:       %d", m))
  if (!is.null(x$items_dropped) && length(x$items_dropped) > 0) {
    m_orig <- m + length(x$items_dropped)
    cat(sprintf(" (of %d; %d dropped by embedding filter)", m_orig,
                length(x$items_dropped)))
  }
  cat("\n")
  cat(sprintf("Factions:    %d (k selection: %s)\n", x$k, x$k_method))
  cat(sprintf("Centering:   %s\n", x$centering))
  cat("\nFaction sizes:\n")
  for (i in seq_along(sizes)) {
    cat(sprintf("  Faction %s: %d (%.1f%%)\n",
                names(sizes)[i], sizes[i], 100 * sizes[i] / n))
  }
  cat(sprintf("\nEigengap suggests k = %d (gap = %.4f)\n",
              x$eigengap$k, x$eigengap$gaps[x$eigengap$gap_position]))
  if (!is.null(x$items_dropped) && length(x$items_dropped) > 0) {
    cat(sprintf("\nDropped items: %s\n",
                paste(x$items_dropped, collapse = ", ")))
  }
  invisible(x)
}

#' @export
summary.trca_topology <- function(object, ...) {
  x <- object
  k <- nrow(x$B)
  cat(sprintf("Topology Summary\n"))
  cat(sprintf("================\n"))
  cat(sprintf("Factions: %d\n", k))
  cat(sprintf("Games:    %d\n", x$games$n_games))
  for (g in names(x$topologies)) {
    topo <- x$topologies[[g]]
    cat(sprintf("\n%s: %s\n", g, topo$type))
    cat(sprintf("  Factions: %s\n", paste(topo$factions, collapse = ", ")))
    cat("  Block sub-matrix:\n")
    sub_B <- round(topo$B, 3)
    for (i in seq_len(nrow(sub_B))) {
      cat(sprintf("    %s\n", paste(sprintf("%7.3f", sub_B[i, ]), collapse = " ")))
    }
  }
  invisible(x)
}

# ---- plot methods ----------------------------------------------------------

#' Plot tRCA Clustering Result
#'
#' Creates a biplot showing respondents and items in the shared spectral
#' embedding space. Respondents are colored by cluster; items are labeled
#' points showing their position in the embedding.
#'
#' @param x A `trca` object from [trca()].
#' @param dims Which embedding dimensions to plot. Default `c(1, 2)`.
#' @param show_items Logical. Show item positions? Default `TRUE`.
#' @param item_labels Logical. Label item points? Default `TRUE`.
#' @param ... Additional arguments (ignored).
#'
#' @return A ggplot2 object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(fit)
#' }
#' @export
plot.trca <- function(x, dims = c(1, 2), show_items = TRUE,
                      item_labels = TRUE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot.trca()")
  }
  resp_emb <- x$respondent_embedding
  item_emb <- x$item_embedding
  if (max(dims) > ncol(resp_emb)) {
    stop(sprintf("Requested dim %d but embedding has only %d dimensions",
                 max(dims), ncol(resp_emb)))
  }

  df_resp <- data.frame(
    dim1 = resp_emb[, dims[1]],
    dim2 = resp_emb[, dims[2]],
    faction = factor(x$clusters),
    type = "respondent"
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(data = df_resp,
                        ggplot2::aes(x = .data$dim1, y = .data$dim2,
                                     color = .data$faction),
                        alpha = 0.5, size = 1.2) +
    ggplot2::labs(
      x = sprintf("Dimension %d", dims[1]),
      y = sprintf("Dimension %d", dims[2]),
      title = sprintf("tRCA Biplot (k = %d)", x$k),
      color = "Faction"
    ) +
    ggplot2::theme_minimal()

  if (show_items && !is.null(item_emb)) {
    item_names <- rownames(item_emb)
    if (is.null(item_names)) item_names <- paste0("item_", seq_len(nrow(item_emb)))
    df_item <- data.frame(
      dim1 = item_emb[, dims[1]],
      dim2 = item_emb[, dims[2]],
      label = item_names
    )
    # Scale items to fit within respondent range
    resp_range <- max(abs(c(df_resp$dim1, df_resp$dim2)))
    item_range <- max(abs(c(df_item$dim1, df_item$dim2)))
    if (item_range > 1e-10) {
      scale_factor <- resp_range / item_range * 0.8
      df_item$dim1 <- df_item$dim1 * scale_factor
      df_item$dim2 <- df_item$dim2 * scale_factor
    }
    p <- p + ggplot2::geom_point(data = df_item,
                                 ggplot2::aes(x = .data$dim1, y = .data$dim2),
                                 shape = 17, size = 3, color = "black")
    if (item_labels) {
      p <- p + ggplot2::geom_text(data = df_item,
                                  ggplot2::aes(x = .data$dim1, y = .data$dim2,
                                               label = .data$label),
                                  size = 3, hjust = -0.2, vjust = -0.5,
                                  color = "black")
    }
  }

  p
}

#' Plot Block Density Matrix
#'
#' Heatmap of the k x k block density matrix. By default uses a diverging
#' color scale (blue = opposition, red = alignment, white = orthogonal).
#' With `absolute = TRUE`, uses a sequential scale showing tie strength
#' regardless of sign — i.e., who is "playing the same game."
#'
#' @param B A k x k block density matrix, or an `trca_topology` object.
#' @param title Plot title. Default `"Block Density Matrix"`.
#' @param absolute Logical. If `TRUE`, plot `|B|` with a sequential palette
#'   (white to dark) showing engagement strength regardless of direction.
#'   Default `FALSE`.
#' @param order Logical. If `TRUE` (default), reorder rows and columns using
#'   Ward hierarchical clustering on `1 - |B|` so factions playing the same
#'   game are adjacent. Set to `FALSE` to keep original faction order.
#'
#' @return A ggplot2 object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' B <- block_matrix(fit, data = sim$data)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_block_matrix(B)
#' }
#' @export
plot_block_matrix <- function(B, title = NULL, absolute = FALSE, order = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_block_matrix()")
  }
  if (inherits(B, "trca_topology")) B <- B$B

  if (is.null(title)) {
    title <- if (absolute) "Relationality Matrix (|B|)" else "Block Density Matrix"
  }

  k <- nrow(B)
  labels <- rownames(B)
  if (is.null(labels)) labels <- paste0("F", seq_len(k))

  # Reorder by Ward clustering on |B| dissimilarity
  if (order && k > 2) {
    d <- stats::as.dist(1 - abs(B))
    hc <- stats::hclust(d, method = "ward.D2")
    ord <- hc$order
    B <- B[ord, ord]
    labels <- labels[ord]
  }

  plot_mat <- if (absolute) abs(B) else B

  df <- expand.grid(row = labels, col = labels, stringsAsFactors = FALSE)
  df$value <- as.vector(plot_mat)
  df$row <- factor(df$row, levels = rev(labels))
  df$col <- factor(df$col, levels = labels)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$col, y = .data$row,
                                         fill = .data$value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", .data$value)),
                       size = 3.5)

  if (absolute) {
    p <- p +
      ggplot2::scale_fill_gradient(low = "white", high = "#2D004B",
                                    limits = c(0, 1),
                                    name = "|Density|")
  } else {
    p <- p +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white",
                                    high = "#B2182B", midpoint = 0,
                                    limits = c(-1, 1),
                                    name = "Density")
  }

  p <- p +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  p
}

#' Plot Faction Relationship Network
#'
#' Network graph showing factions as nodes and inter-faction ties as edges.
#' Uses a signed stress layout (from `graphlayouts`) that pushes opposing
#' factions apart and pulls aligned factions together. Falls back to circle
#' layout if `graphlayouts` is not installed.
#'
#' Positive ties (alignment) are shown as solid lines; negative ties
#' (opposition) as dashed lines. In color mode (default), uses red/blue.
#' In black-and-white mode (`bw = TRUE`), edges are grey with opacity
#' proportional to tie strength.
#'
#' @param x A k x k block density matrix, or an `trca_topology` object.
#' @param layout Layout algorithm: `"stress"` (default, signed stress layout),
#'   `"circle"`, or `"fr"` (Fruchterman-Reingold).
#' @param show_values Logical; label edges with block density values?
#'   Default `TRUE`.
#' @param threshold Only draw edges where `|B[i,j]| > threshold`. Default `0`.
#' @param title Plot title.
#' @param node_size Size of faction nodes. Default `18`.
#' @param bw Logical. If `TRUE`, use greyscale with opacity for strength
#'   and linetype for sign. Suitable for print journals. Default `FALSE`.
#' @param faction_labels Optional character vector of custom node labels
#'   (length k). Overrides the names from the block matrix.
#' @param jitter Amount of layout jitter as a fraction of the coordinate
#'   range. Default `0.05`. Set to `0` to disable.
#'
#' @return A ggplot2 object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' topo <- topology(fit, data = sim$data)
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("graphlayouts", quietly = TRUE)) {
#'   plot_faction_network(topo)
#' }
#' @export
plot_faction_network <- function(x, layout = "stress", show_values = TRUE,
                                 threshold = 0, title = "Faction Relationships",
                                 node_size = 18, bw = FALSE,
                                 faction_labels = NULL, jitter = 0.20) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_faction_network()")
  }
  if (inherits(x, "trca_topology")) x <- x$B

  B <- as.matrix(x)
  k <- nrow(B)
  labels <- rownames(B)
  if (is.null(labels)) labels <- paste0("F", seq_len(k))

  if (!is.null(faction_labels)) {
    if (length(faction_labels) != k) {
      stop(sprintf("faction_labels must have length %d, got %d",
                   k, length(faction_labels)))
    }
    labels <- faction_labels
  }

  # Build igraph object with signed weights
  g <- igraph::graph_from_adjacency_matrix(
    B, mode = "undirected", weighted = TRUE, diag = FALSE
  )

  # Compute layout
  if (layout == "stress" &&
      requireNamespace("graphlayouts", quietly = TRUE)) {
    coords <- .signed_stress_layout(g)
  } else {
    if (layout == "stress") {
      message("Package 'graphlayouts' not found, falling back to circle layout")
    }
    # For non-stress layouts, use abs(B) graph
    g_abs <- igraph::graph_from_adjacency_matrix(
      abs(B), mode = "undirected", weighted = TRUE, diag = FALSE
    )
    if (layout == "fr") {
      coords <- igraph::layout_with_fr(g_abs)
    } else {
      coords <- igraph::layout_in_circle(g_abs)
    }
  }

  # Apply jitter to prevent node overlap
  if (jitter > 0 && nrow(coords) > 1) {
    xr <- diff(range(coords[, 1]))
    yr <- diff(range(coords[, 2]))
    if (xr > 1e-10) {
      coords[, 1] <- coords[, 1] + stats::runif(k, -jitter * xr, jitter * xr)
    }
    if (yr > 1e-10) {
      coords[, 2] <- coords[, 2] + stats::runif(k, -jitter * yr, jitter * yr)
    }
  }

  node_df <- data.frame(
    x = coords[, 1], y = coords[, 2], label = labels,
    stringsAsFactors = FALSE
  )

  # Build edge list from upper triangle
  edge_rows <- list()
  idx <- 0L
  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      if (abs(B[i, j]) <= threshold) next
      idx <- idx + 1L
      edge_rows[[idx]] <- data.frame(
        x = coords[i, 1], y = coords[i, 2],
        xend = coords[j, 1], yend = coords[j, 2],
        xmid = (coords[i, 1] + coords[j, 1]) / 2,
        ymid = (coords[i, 2] + coords[j, 2]) / 2,
        weight = B[i, j],
        abs_weight = abs(B[i, j]),
        sign = if (B[i, j] >= 0) "Alignment (+)" else "Opposition (-)",
        lbl = sprintf("%.2f", B[i, j]),
        stringsAsFactors = FALSE
      )
    }
  }

  p <- ggplot2::ggplot()

  if (length(edge_rows) > 0) {
    edge_df <- do.call(rbind, edge_rows)

    if (bw) {
      p <- p +
        ggplot2::geom_segment(
          data = edge_df,
          ggplot2::aes(
            x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            linetype = .data$sign, linewidth = .data$abs_weight,
            alpha = .data$abs_weight
          ),
          color = "grey30"
        ) +
        ggplot2::scale_linetype_manual(
          values = c("Alignment (+)" = "solid", "Opposition (-)" = "dashed"),
          name = "Tie"
        ) +
        ggplot2::scale_linewidth_continuous(
          range = c(0.5, 3), name = "|Density|"
        ) +
        ggplot2::scale_alpha_continuous(range = c(0.3, 1.0), guide = "none")
    } else {
      # Continuous diverging color gradient: blue (opposition) → white → red (alignment)
      p <- p +
        ggplot2::geom_segment(
          data = edge_df,
          ggplot2::aes(
            x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend,
            color = .data$weight, linewidth = .data$abs_weight
          )
        ) +
        ggplot2::scale_color_gradient2(
          low = "#2166AC", mid = "white", high = "#B2182B",
          midpoint = 0, limits = c(-1, 1), name = "Density"
        ) +
        ggplot2::scale_linewidth_continuous(
          range = c(0.5, 3), guide = "none"
        )
    }

    if (show_values) {
      p <- p + ggplot2::geom_label(
        data = edge_df,
        ggplot2::aes(x = .data$xmid, y = .data$ymid, label = .data$lbl),
        size = 3, fill = "white", label.size = 0.2, alpha = 0.85
      )
    }
  }

  p <- p +
    ggplot2::geom_point(
      data = node_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      size = node_size, shape = 21, fill = "grey95",
      color = "grey55", stroke = 0.8
    ) +
    ggplot2::geom_text(
      data = node_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      size = 4, fontface = "bold"
    ) +
    ggplot2::labs(title = title) +
    ggplot2::coord_fixed(
      xlim = range(node_df$x) + c(-1, 1) * diff(range(node_df$x)) * 0.15,
      ylim = range(node_df$y) + c(-1, 1) * diff(range(node_df$y)) * 0.15,
      clip = "off"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
    )

  p
}

#' Signed Stress Layout for Faction Networks
#'
#' Computes a stress-minimizing layout where positive edges pull nodes
#' together and negative edges push them apart. Requires `graphlayouts`.
#'
#' @param g An igraph graph with signed `weight` edge attribute.
#' @param eps Small constant to avoid division by zero. Default `1e-6`.
#' @param far_quantile Quantile of positive-edge lengths used to set the
#'   baseline separation for negative edges. Default `0.9`.
#' @param far_multiplier Multiplier for the far baseline. Default `3`.
#' @param iter Maximum stress iterations. Default `500`.
#' @param tol Convergence tolerance. Default `1e-4`.
#' @param bbox Bounding box size. Default `30`.
#' @return A k x 2 coordinate matrix.
#' @keywords internal
.signed_stress_layout <- function(g, eps = 1e-6, far_quantile = 0.9,
                                  far_multiplier = 3, iter = 500,
                                  tol = 1e-4, bbox = 30) {
  w <- igraph::edge_attr(g, "weight")
  if (is.null(w) || length(w) == 0) {
    return(igraph::layout_in_circle(g))
  }

  w_pos <- w[w > 0]
  if (length(w_pos) == 0) {
    return(igraph::layout_in_circle(g))
  }

  # Positive weights -> short distances; negative -> far distances
  len_pos <- 1 / (w_pos + eps)
  D0 <- far_multiplier * as.numeric(
    stats::quantile(len_pos, probs = far_quantile, names = FALSE)
  )

  len <- ifelse(w > 0,
                1 / (w + eps),
                D0 + 1 / (abs(w) + eps))

  xy <- graphlayouts::layout_with_stress(g, weights = len,
                                          iter = iter, tol = tol, bbox = bbox)
  colnames(xy) <- c("x", "y")
  xy
}

#' Plot Item Diagnostics
#'
#' Bar chart showing item scores (embedding norms or variances) with the
#' filtering threshold marked. Items below the threshold are shown in grey.
#'
#' @param x A `trca` object from [trca()], or a data frame from
#'   [trca_item_diagnostics()].
#' @param score Which score to plot: `"embedding_norm"` (default) or
#'   `"item_var"` or `"eta_sq"`.
#' @param threshold Optional threshold line. If `x` is a `trca` object
#'   with item filtering, the threshold is extracted automatically.
#'
#' @return A ggplot2 object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, filter_items = TRUE, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_item_diagnostics(fit)
#' }
#' @export
plot_item_diagnostics <- function(x, score = "embedding_norm",
                                  threshold = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_item_diagnostics()")
  }
  if (inherits(x, "trca")) {
    if (is.null(x$item_scores)) {
      stop("No item scores available. Run trca() with filter_items = TRUE.")
    }
    scores <- x$item_scores
    df <- data.frame(
      item = names(scores),
      score = as.numeric(scores),
      kept = names(scores) %in% x$items_kept,
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(x)) {
    df <- data.frame(
      item = x$item,
      score = x[[score]],
      kept = TRUE,
      stringsAsFactors = FALSE
    )
  } else {
    stop("x must be an 'trca' object or a data frame from trca_item_diagnostics()")
  }

  df <- df[order(-df$score), ]
  df$item <- factor(df$item, levels = df$item)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$item, y = .data$score,
                                         fill = .data$kept)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = c("TRUE" = "#2166AC", "FALSE" = "grey70"),
                                labels = c("TRUE" = "Kept", "FALSE" = "Dropped"),
                                name = "Status") +
    ggplot2::labs(
      x = "Item", y = score,
      title = sprintf("Item Diagnostics: %s", score)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  if (!is.null(threshold)) {
    p <- p + ggplot2::geom_hline(yintercept = threshold, linetype = "dashed",
                                  color = "red")
  }

  p
}

#' Plot Faction Profiles
#'
#' Faceted barplot showing each faction's mean deviation from the overall
#' item mean. Bars are colored by direction (above/below the overall mean)
#' and annotated with significance stars from one-sample t-tests.
#'
#' @param data Numeric matrix of respondents (rows) x items (columns).
#' @param clusters Integer vector of cluster assignments (length N). If
#'   `data` is a `trca` object, clusters are extracted automatically.
#' @param topo Optional `trca_topology` object. If provided, faction labels
#'   are read from `rownames(topo$B)` (e.g., after [name_factions()]).
#' @param faction_labels Optional character vector of faction names (length k).
#'   Overrides names from `topo` if both are provided.
#' @param item_labels Optional character vector of item names (length M).
#' @param bw Logical. If `TRUE`, use greyscale fills. Default `FALSE`.
#' @param sig_level Significance level for stars. Default `0.05`.
#' @param text_size Optional font size for the item labels on the y axis.
#'   `NULL` (default) uses the theme default.
#' @param title Plot title.
#'
#' @return A ggplot2 object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_faction_profiles(sim$data, fit$clusters)
#' }
#' @export
plot_faction_profiles <- function(data, clusters = NULL,
                                  topo = NULL,
                                  faction_labels = NULL,
                                  item_labels = NULL,
                                  bw = FALSE,
                                  sig_level = 0.05,
                                  text_size = NULL,
                                  title = "Faction Profiles: Deviation from Overall Mean") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_faction_profiles()")
  }

  # Extract clusters from trca object if needed
  if (inherits(data, "trca")) {
    if (is.null(clusters)) clusters <- data$clusters
    data <- data$X_centered
  }
  if (is.null(clusters)) {
    stop("clusters must be provided (or pass an trca object as data)")
  }

  # Pull faction labels from topology object if available
  if (is.null(faction_labels) && !is.null(topo) && inherits(topo, "trca_topology")) {
    faction_labels <- rownames(topo$B)
  }

  data_mat <- .as_response_matrix(data)
  .check_clusters(clusters, nrow(data_mat))
  m <- ncol(data_mat)
  overall_means <- colMeans(data_mat, na.rm = TRUE)

  ids <- sort(unique(clusters))
  k <- length(ids)

  if (is.null(faction_labels)) {
    faction_labels <- paste0("Faction_", ids)
  }
  if (is.null(item_labels)) {
    item_labels <- colnames(data_mat)
    if (is.null(item_labels)) item_labels <- paste0("Item_", seq_len(m))
  }

  rows <- list()
  for (i in seq_along(ids)) {
    mask <- clusters == ids[i]
    faction_data <- data_mat[mask, , drop = FALSE]
    faction_means <- colMeans(faction_data, na.rm = TRUE)
    deviations <- faction_means - overall_means

    pvals <- numeric(m)
    for (j in seq_len(m)) {
      tt <- tryCatch(
        stats::t.test(faction_data[, j], mu = overall_means[j]),
        error = function(e) list(p.value = 1)
      )
      pvals[j] <- tt$p.value
    }

    stars <- ifelse(pvals < 0.001, "***",
             ifelse(pvals < 0.01, "**",
             ifelse(pvals < sig_level, "*", "")))

    rows[[i]] <- data.frame(
      faction = faction_labels[i],
      item = item_labels,
      deviation = deviations,
      pvalue = pvals,
      star = stars,
      direction = ifelse(deviations >= 0, "above", "below"),
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, rows)
  df$faction <- factor(df$faction, levels = faction_labels)
  df$item <- factor(df$item, levels = rev(item_labels))

  # Nudge stars outward from bar tip
  df$star_hjust <- ifelse(df$deviation >= 0, -0.3, 1.3)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$item, y = .data$deviation,
                                         fill = .data$direction)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$star, hjust = .data$star_hjust),
      size = 3.5, color = "black", vjust = 0.75
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::facet_wrap(~ .data$faction) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "grey50",
                        linewidth = 0.3) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.15)) +
    ggplot2::labs(title = title, x = NULL, y = "Deviation from overall mean") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   strip.text = ggplot2::element_text(face = "bold"),
                   axis.text.y = if (!is.null(text_size))
                     ggplot2::element_text(size = text_size))

  if (bw) {
    p <- p + ggplot2::scale_fill_manual(
      values = c("above" = "grey30", "below" = "grey75")
    )
  } else {
    p <- p + ggplot2::scale_fill_manual(
      values = c("above" = "#B2182B", "below" = "#2166AC")
    )
  }

  p
}

# ---- convenience accessors ------------------------------------------------

#' Extract Topology from a Clustering Result
#'
#' Computes the full topology map (block density matrix, games, topology
#' classification) from a `trca` clustering result.
#'
#' @param x A clustering result object.
#' @param tie_threshold Absolute block density below which two factions are
#'   treated as unrelated (orthogonal) when detecting games and classifying
#'   topology. Default `0.1`.
#' @param ... Unused.
#'
#' @return An `trca_topology` object.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' topo <- topology(fit, data = sim$data)
#' topo
#' summary(topo)
#' @export
topology <- function(x, ...) {
  UseMethod("topology")
}

#' @rdname topology
#' @param data Original data matrix (needed for profile-based block density).
#' @param centering Centering for profile-based B. Defaults to the centering
#'   used during clustering (stored in `x$centering`). Without `data`, the
#'   block matrix is computed from the fitted model's centered matrix, so a
#'   different centering requires `data`.
#' @export
topology.trca <- function(x, data = NULL, centering = x$centering,
                          tie_threshold = 0.1, ...) {
  B <- .fit_block_density(x, data, centering)
  .topology_from_blocks(B, tie_threshold = tie_threshold)
}

# Block density for a fitted model. Without `data`, the fit's own centered
# matrix is used as is, so `centering` must match the fit.
.fit_block_density <- function(x, data, centering) {
  centering <- match.arg(centering, c("mean", "midpoint"))
  if (!is.null(data)) {
    return(trca_block_density(data, x$clusters, centering = centering))
  }
  if (centering != x$centering) {
    stop(sprintf(paste0(
      "The model was fitted with centering = \"%s\". Pass `data` to compute ",
      "the block matrix with centering = \"%s\"."), x$centering, centering),
      call. = FALSE)
  }
  # X_centered is already centered; midpoint 0 leaves it unchanged
  trca_block_density(x$X_centered, x$clusters, centering = "midpoint",
                     midpoint = 0)
}

#' Build a topology object from a block density matrix
#'
#' Splits the factions into games with [detect_games()] and classifies the
#' topology of each game with [classify_topology()].
#'
#' @param B A k x k signed block density matrix with faction names.
#' @param tie_threshold Absolute block density below which two factions are
#'   treated as unrelated.
#' @return A `trca_topology` object.
#' @keywords internal
.topology_from_blocks <- function(B, tie_threshold = 0.1) {
  games <- detect_games(B, threshold = tie_threshold)
  topologies <- list()
  for (g in seq_len(games$n_games)) {
    factions <- names(games$membership)[games$membership == g]
    idx <- which(rownames(B) %in% factions)
    sub_B <- B[idx, idx, drop = FALSE]
    topo_type <- classify_topology(sub_B, threshold = tie_threshold)
    topologies[[paste0("Game_", g)]] <- list(
      type = topo_type, factions = factions, B = sub_B
    )
  }
  result <- list(B = B, games = games, topologies = topologies)
  class(result) <- "trca_topology"
  result
}

#' Extract Block Density Matrix
#'
#' Convenience function to extract or compute the block density matrix
#' from a clustering result.
#'
#' @param x A clustering result or topology object.
#' @param ... Additional arguments.
#'
#' @return A k x k block density matrix.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' block_matrix(fit, data = sim$data)
#' @export
block_matrix <- function(x, ...) {
  UseMethod("block_matrix")
}

#' @rdname block_matrix
#' @param data Original data matrix.
#' @param centering Centering for profile-based B. Defaults to the centering
#'   used during clustering (stored in `x$centering`), as in [topology()].
#'   A different centering requires `data`.
#' @export
block_matrix.trca <- function(x, data = NULL, centering = x$centering, ...) {
  .fit_block_density(x, data, centering)
}

#' @rdname block_matrix
#' @export
block_matrix.trca_topology <- function(x, ...) {
  x$B
}

#' Extract Item Importance Scores
#'
#' Returns item-level diagnostics from a tRCA clustering result.
#'
#' @param x A `trca` object from [trca()].
#' @param data Optional original data matrix for computing eta-squared.
#'
#' @return A data frame from [trca_item_diagnostics()].
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' head(item_importance(fit, data = sim$data))
#' @export
item_importance <- function(x, data = NULL) {
  if (!inherits(x, "trca")) {
    stop("item_importance() requires an 'trca' object from trca()")
  }
  trca_item_diagnostics(x, data = data)
}

# ---- name_factions --------------------------------------------------------

#' Rename Factions in a Topology Object
#'
#' Updates faction labels throughout the topology object: B matrix
#' row/column names, game membership names, meta-graph vertex names,
#' and per-game topology faction lists and sub-B matrices.
#'
#' @param x An `trca_topology` object.
#' @param labels A character vector of new names (length k, positional),
#'   or a named character vector mapping old names to new names
#'   (e.g., `c(Faction_1 = "Progressive", Faction_2 = "Traditional")`).
#'
#' @return A modified `trca_topology` object with updated names.
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' fit <- trca(sim$data, k = 3, verbose = FALSE)
#' topo <- topology(fit, data = sim$data)
#' topo <- name_factions(topo, c("Left", "Right", "Center"))
#' rownames(block_matrix(topo))
#' @export
name_factions <- function(x, labels) {
  if (!inherits(x, "trca_topology")) {
    stop("name_factions() requires an 'trca_topology' object")
  }
  k <- nrow(x$B)
  old_labels <- rownames(x$B)

  if (!is.null(names(labels))) {
    bad <- setdiff(names(labels), old_labels)
    if (length(bad) > 0) {
      stop(sprintf("Unknown faction names: %s", paste(bad, collapse = ", ")))
    }
    new_labels <- old_labels
    for (i in seq_along(labels)) {
      idx <- which(old_labels == names(labels)[i])
      new_labels[idx] <- labels[i]
    }
  } else {
    if (length(labels) != k) {
      stop(sprintf("labels must have length %d (number of factions), got %d",
                   k, length(labels)))
    }
    new_labels <- labels
  }

  name_map <- stats::setNames(new_labels, old_labels)

  rownames(x$B) <- colnames(x$B) <- new_labels
  names(x$games$membership) <- name_map[names(x$games$membership)]

  if (!is.null(x$games$meta_graph)) {
    igraph::V(x$games$meta_graph)$name <- name_map[igraph::V(x$games$meta_graph)$name]
  }

  for (g in names(x$topologies)) {
    topo <- x$topologies[[g]]
    topo$factions <- as.character(name_map[topo$factions])
    rownames(topo$B) <- colnames(topo$B) <- name_map[rownames(topo$B)]
    x$topologies[[g]] <- topo
  }

  x
}

# ---- compare_methods ------------------------------------------------------

#' Compare tRCA and RCA on the Same Data
#'
#' Runs tRCA and the RCA baseline on the same dataset and returns a
#' structured comparison. Handles data cleaning (missing values,
#' zero-variance rows) internally. For both methods, the topology is read
#' from the profile-based block density matrix of the recovered classes
#' ([trca_block_density()]), so the two are compared on the same footing.
#'
#' @param data Numeric matrix or data frame of respondents (rows) x items
#'   (columns). Missing values are removed via complete cases.
#' @param methods Character vector of methods to run. Default
#'   `c("trca", "rca")`.
#' @param k Optional fixed k. If `NULL`, each method auto-selects.
#' @param trca_centering Centering for tRCA and for the block density
#'   matrices. Default `"mean"`, as in [trca()].
#' @param trca_filter_items Item filter for tRCA. Default `FALSE`, as in
#'   [trca()].
#' @param seed Optional integer seed. If supplied, `set.seed(seed)` is
#'   called before each random step so results are reproducible. `NULL`
#'   (default) uses the current state of the random number generator.
#' @param verbose Logical. Show progress messages. Default `TRUE`.
#'
#' @return A list with class `"trca_comparison"` containing:
#'   \item{data}{Cleaned data matrix used for analysis.}
#'   \item{n}{Number of respondents after cleaning.}
#'   \item{m}{Number of items.}
#'   \item{results}{Named list of clustering results (trca/rca).}
#'   \item{topologies}{Named list of `trca_topology` objects per method.}
#'   \item{summary}{Data frame comparing k, topology across methods.}
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' cmp <- compare_methods(sim$data, k = 3, verbose = FALSE)
#' cmp
#' @export
compare_methods <- function(data, methods = c("trca", "rca"),
                            k = NULL,
                            trca_centering = "mean",
                            trca_filter_items = FALSE,
                            seed = NULL, verbose = TRUE) {
  methods <- match.arg(methods, c("trca", "rca"), several.ok = TRUE)
  data_mat <- .as_response_matrix(data)

  # Clean data: complete cases, remove zero-variance rows
  data_mat <- data_mat[stats::complete.cases(data_mat), ]
  row_vars <- apply(data_mat, 1, stats::var, na.rm = TRUE)
  data_mat <- data_mat[row_vars > 1e-10, ]
  n <- nrow(data_mat)
  m <- ncol(data_mat)

  if (verbose) message(sprintf("Data: %d respondents x %d items", n, m))

  results <- list()
  topologies <- list()
  summary_rows <- list()

  for (method in methods) {
    if (verbose) message(sprintf("  Running %s...", toupper(method)))
    res <- tryCatch({
      r <- if (method == "trca") {
        trca(data_mat, k = k, centering = trca_centering,
             filter_items = trca_filter_items, seed = seed, verbose = verbose)
      } else {
        rca_baseline(data_mat, k = k, seed = seed)
      }
      B <- trca_block_density(data_mat, r$clusters, centering = trca_centering)
      list(result = r, topo = .topology_from_blocks(B))
    }, error = function(e) {
      if (verbose) message(sprintf("    %s failed: %s", toupper(method), e$message))
      NULL
    })

    if (!is.null(res)) {
      results[[method]] <- res$result
      topologies[[method]] <- res$topo
      topo_type <- res$topo$topologies[[1]]$type
      summary_rows[[method]] <- data.frame(
        method = toupper(method),
        k = res$result$k,
        topology = topo_type,
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL

  result <- list(
    data = data_mat,
    n = n,
    m = m,
    results = results,
    topologies = topologies,
    summary = summary_df
  )
  class(result) <- "trca_comparison"
  result
}

#' @export
print.trca_comparison <- function(x, ...) {
  cat(sprintf("Method comparison: %d respondents x %d items\n", x$n, x$m))
  cat(sprintf("Methods: %s\n\n", paste(names(x$results), collapse = ", ")))
  print(x$summary, row.names = FALSE)
  invisible(x)
}

# ---- check_data ------------------------------------------------------------

#' Check Data Suitability for Relational Class Analysis
#'
#' Inspects a data matrix for common issues before running tRCA.
#' Reports missing values, zero-variance rows/columns, detected scale range,
#' and suggested centering method. It also flags haven-labelled columns that
#' contain negative values, which survey files often use as codes for
#' "refused" or "don't know" and which should be recoded to `NA` first.
#'
#' @param data Numeric matrix or data frame of respondents (rows) x items
#'   (columns).
#' @param verbose Logical. Print the report. Default `TRUE`.
#'
#' @return A list (invisibly) with diagnostic information:
#'   \item{n}{Number of respondents.}
#'   \item{m}{Number of items.}
#'   \item{n_complete}{Respondents with no missing values.}
#'   \item{pct_missing}{Percentage of missing cells.}
#'   \item{zero_var_rows}{Number of constant-response respondents.}
#'   \item{zero_var_cols}{Number of zero-variance items.}
#'   \item{scale_range}{Detected min-max range.}
#'   \item{midpoint}{Suggested scale midpoint.}
#'   \item{suggested_centering}{Suggested centering method.}
#'   \item{negative_coded}{Names of haven-labelled columns with negative
#'     values.}
#' @examples
#' sim <- sim_triadic_impasse(n_per_faction = 40)
#' check_data(sim$data)
#' @export
check_data <- function(data, verbose = TRUE) {
  data_mat <- .as_response_matrix(data)
  n <- nrow(data_mat)
  m <- ncol(data_mat)

  # Negative codes in labelled survey columns are usually missing-data codes
  negative_coded <- character(0)
  if (is.data.frame(data)) {
    labelled <- vapply(data, inherits, logical(1), what = "haven_labelled")
    has_neg <- vapply(data, function(col) {
      is.numeric(unclass(col)) && any(unclass(col) < 0, na.rm = TRUE)
    }, logical(1))
    negative_coded <- names(data)[labelled & has_neg]
  }

  n_missing <- sum(is.na(data_mat))
  pct_missing <- 100 * n_missing / (n * m)
  n_complete <- sum(stats::complete.cases(data_mat))

  row_vars <- apply(data_mat, 1, stats::var, na.rm = TRUE)
  zero_var_rows <- sum(row_vars < 1e-10, na.rm = TRUE)

  col_vars <- apply(data_mat, 2, stats::var, na.rm = TRUE)
  zero_var_cols <- sum(col_vars < 1e-10, na.rm = TRUE)

  scale_min <- min(data_mat, na.rm = TRUE)
  scale_max <- max(data_mat, na.rm = TRUE)
  midpoint <- (scale_min + scale_max) / 2

  # Detect Likert-like scales
  unique_vals <- sort(unique(as.vector(data_mat[!is.na(data_mat)])))
  is_likert <- length(unique_vals) <= 10 && all(unique_vals == round(unique_vals))
  suggested <- if (is_likert) "midpoint" else "mean"

  if (verbose) {
    message(sprintf("Data check: %d respondents x %d items", n, m))
    message(sprintf("  Complete cases: %d of %d (%.1f%%)",
                    n_complete, n, 100 * n_complete / n))
    if (n_missing > 0) {
      message(sprintf("  Missing cells:  %d (%.1f%%)", n_missing, pct_missing))
    }
    if (zero_var_rows > 0) {
      message(sprintf("  WARNING: %d respondent(s) with constant responses (will cause NaN correlations)",
                  zero_var_rows))
    }
    if (zero_var_cols > 0) {
      message(sprintf("  WARNING: %d item(s) with zero variance (carry no signal)",
                  zero_var_cols))
    }
    if (length(negative_coded) > 0) {
      message(sprintf(
        "  WARNING: %d labelled item(s) contain negative values, often missing-data codes; recode them to NA: %s",
        length(negative_coded), .format_ids(negative_coded)))
    }
    message(sprintf("  Scale range:    [%g, %g]", scale_min, scale_max))
    if (is_likert) {
      message(sprintf("  Detected:       Likert-like (%d levels)", length(unique_vals)))
      message(sprintf("  Midpoint:       %g", midpoint))
    }
    message(sprintf("  Suggested centering: %s", suggested))
  }

  invisible(list(
    n = n, m = m,
    n_complete = n_complete,
    pct_missing = pct_missing,
    zero_var_rows = zero_var_rows,
    zero_var_cols = zero_var_cols,
    scale_range = c(scale_min, scale_max),
    midpoint = midpoint,
    is_likert = is_likert,
    suggested_centering = suggested,
    negative_coded = negative_coded
  ))
}
