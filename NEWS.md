# trca 0.1.0

First release.

* `trca()` fits Topological Relational Class Analysis: a signed spectral
  embedding of the respondent-item network, with respondents clustered by
  k-means.
* Automatic choice of k. A one-faction test (parallel analysis plus the gap
  statistic) can return a single faction (consensus). Otherwise the
  silhouette criterion is evaluated in the signal dimensions found by
  parallel analysis.
* `trca_select_k()` reports the k that `trca()` would choose, with its
  silhouette widths, alongside the eigengap, within-cluster sum of squares,
  and spectral BIC.
* `topology()`, `block_matrix()`, and `trca_block_density()` build the
  signed block density matrix (by default with the fit's own centering);
  `detect_games()` and `classify_topology()` identify games and classify them as Consensus, Bipolar Opposition, Triadic Impasse,
  Multipolar Opposition, Coalition, Hub-and-Spoke, or Complex.
* Relational Class Analysis baselines: `rca_baseline()` and `rca_leiden()`.
  `compare_methods()` compares tRCA with RCA.
* Simulators for canonical topologies: `sim_bipolar()`, `sim_consensus()`,
  `sim_triadic_impasse()`, `sim_hub_and_spoke()`, `sim_multipolar()`, and
  `sim_coalition()`.
* Plots: `plot_block_matrix()`, `plot_faction_network()`,
  `plot_faction_profiles()`, `plot_item_diagnostics()`, `plot_eigengap()`,
  and a `plot()` method for fitted models.
* Input checks: `check_data()`, and validation of data frames, ordered
  factors, haven-labelled columns, and missing values in every function
  that takes survey data.
