# METHOD: E1 — Eigengap diagnostic visualization

#' Plot Eigengap Diagnostic
#'
#' Plots the eigenvalue spectrum of the signed normalized bipartite Laplacian with
#' the selected gap highlighted. A clean gap indicates well-defined
#' factional structure; a muddy spectrum suggests the field does not
#' decompose neatly.
#'
#' @param eigengap Output from [eigengap_select_k()] or the `eigengap`
#'   component of a [trca()] result.
#' @param max_display Maximum number of eigenvalues to display. Default `20`.
#'
#' @return A ggplot2 object.
#' @examples
#' sim <- sim_bipolar(n_per_faction = 40)
#' fit <- trca(sim$data, verbose = FALSE)
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_eigengap(fit$eigengap)
#' }
#' @export
plot_eigengap <- function(eigengap, max_display = 20) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot_eigengap()")
  }
  n_show <- min(max_display, length(eigengap$eigenvalues))
  evals <- eigengap$eigenvalues[seq_len(n_show)]
  k <- eigengap$k
  gap_pos <- eigengap$gap_position
  df <- data.frame(index = seq_along(evals), eigenvalue = evals)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$index, y = .data$eigenvalue)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = gap_pos + 0.5, linetype = "dashed",
                        color = "red") +
    ggplot2::annotate("text", x = gap_pos + 0.5, y = max(evals) * 0.9,
                      label = paste0("k = ", k, " (gap at ", gap_pos, ")"),
                      hjust = -0.2, color = "red") +
    ggplot2::labs(x = "Index", y = "Eigenvalue",
                  title = "Eigengap Diagnostic",
                  subtitle = "Signed Normalized Laplacian spectrum") +
    ggplot2::theme_minimal()
  p
}
