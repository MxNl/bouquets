#' Cluster Time Series by Directional Dynamics
#'
#' @description
#' Encodes each time series as a feature vector and clusters the series based
#' on similarity of those sequences. The feature representation is controlled
#' by `normalise` and should match the value used in [make_plot_bouquet()] so
#' that cluster assignments align visually with the rendered paths. The
#' resulting `cluster` column can be passed directly to [make_plot_bouquet()]
#' via `stem_colors`, `flower_colors`, `facet_by`, or `cluster_hull`.
#'
#' The returned object is both the original `data` tibble (with an added
#' cluster column) **and** a `cluster_bouquet` S3 object. Call
#' [summary.cluster_bouquet()] on it to print a structured report of cluster
#' quality, sizes, and member series.
#'
#' @param data A long-format data frame or tibble -- the same format expected
#'   by [make_plot_bouquet()].
#' @param time_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The time index
#'   column. Defaults to the first column.
#' @param series_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   identifying each individual time series. Defaults to the second column.
#' @param value_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The numeric
#'   value column. Defaults to the third column.
#' @param k Either a positive integer specifying the number of clusters, or
#'   `"auto"` (default) to select the optimal number by maximising the
#'   composite silhouette score across `k = 2, ..., k_max`.
#' @param k_max A single positive integer. Upper bound considered when
#'   `k = "auto"`. Capped at `n_series - 1`. Defaults to `8L`.
#' @param method The clustering algorithm. One of:
#'   \describe{
#'     \item{`"pca_hclust"`}{(Default) PCA projection followed by agglomerative
#'       hierarchical clustering with Ward's D2. Deterministic; no extra
#'       packages required. The `distance` argument does not apply.}
#'     \item{`"pca_kmeans"`}{PCA projection followed by k-means. Set `seed`
#'       for reproducibility. The `distance` argument does not apply.}
#'     \item{`"hclust"`}{Ward's D2 hierarchical clustering directly on the
#'       direction sequences using the `distance` metric. Best for short series.}
#'     \item{`"kmeans"`}{k-means on the direction sequences. Always uses
#'       Euclidean distance internally. Set `seed` for reproducibility.}
#'     \item{`"pam"`}{Partitioning Around Medoids. More robust to outliers.
#'       Requires the \pkg{cluster} package.}
#'   }
#' @param distance Distance metric for `"hclust"` and `"pam"`. One of
#'   `"euclidean"` (default), `"correlation"` (\eqn{1 - r}), or `"manhattan"`.
#'   Not used by PCA methods or `"kmeans"`.
#' @param resolution A non-negative number. Composite k-selection score:
#'   \eqn{\bar{s}(k) \times \min_c \bar{s}_c(k) \times k^{\text{resolution}}}.
#'   Higher values favour finer clusters. `0` = plain mean silhouette.
#'   Default `0.5`.
#' @param pca_variance Cumulative variance retained during PCA. Only used by
#'   `"pca_hclust"` and `"pca_kmeans"`. Default `0.90`.
#' @param cluster_col String. Name of the cluster column added to `data`.
#'   Default `"cluster"`.
#' @param normalise Logical. Controls the feature representation used for
#'   clustering. Should match the value passed to [make_plot_bouquet()] so
#'   that cluster assignments align visually with the rendered paths.
#'   \describe{
#'     \item{`FALSE` (default)}{Binary \eqn{\pm 1}/0 direction sequences.
#'       Clusters by directional pattern only.}
#'     \item{`TRUE`}{Same binary \eqn{\pm 1}/0 features. Per-series angle
#'       scaling in [make_plot_bouquet()] does not change the direction
#'       sequence, so clustering is identical to `FALSE`.}
#'   }
#' @param seed Integer or `NULL`. Random seed passed to [base::set.seed()]
#'   before k-means initialisation. Ensures reproducible results for
#'   `"pca_kmeans"` and `"kmeans"`. Default `NULL` = no seed (non-reproducible).
#' @param verbose Logical. Print k selection table, chosen k, silhouette score,
#'   and cluster sizes. Default `FALSE`.
#'
#' @return The original `data` tibble with one additional factor column (named
#'   by `cluster_col`, levels `"C1"`, `"C2"`, ... ordered by decreasing size)
#'   plus a `cluster_bouquet` S3 class. The object also carries a `bq_meta`
#'   attribute containing all clustering diagnostics; use
#'   [summary.cluster_bouquet()] to print them.
#'
#' @details
#' ## Feature representation
#' Each series of length \eqn{n} is represented as a vector of \eqn{n - 1}
#' signed steps \eqn{d_i \in \{-1, 0, +1\}}.
#'
#' ## Composite k-selection
#' Plain mean silhouette consistently selects k = 2 because two large blobs
#' score globally well even when finer structure exists. The composite score
#' adds a worst-cluster penalty and a resolution exponent.
#'
#' ## Pairing with `make_plot_bouquet()`
#' Pass the same `normalise` value to both functions. When a `cluster_bouquet`
#' object is piped into [make_plot_bouquet()], the `normalise` setting is
#' inherited automatically if `make_plot_bouquet()` is left at its default
#' (`normalise = FALSE`). A warning is emitted if an explicit mismatch is
#' detected.
#' ```r
#' data |>
#'   cluster_bouquet(
#'     time_col = date, series_col = id, value_col = gwl,
#'     normalise = TRUE
#'   ) |>
#'   make_plot_bouquet(
#'     time_col = date, series_col = id, value_col = gwl,
#'     stem_colors = cluster, flower_colors = cluster
#'     # normalise = TRUE is inherited automatically
#'   )
#' ```
#'
#' @seealso [make_plot_bouquet()] for visualising the result.
#'   [summary.cluster_bouquet()] for a structured quality report.
#'   [plot_cluster_quality()] to plot silhouette scores across k.
#'
#' @examples
#' set.seed(42)
#' n      <- 52L
#' weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
#' season <- sin(seq(0, 2 * pi, length.out = n))
#'
#' gw_long <- tibble::tibble(
#'   week    = rep(weeks, 6L),
#'   station = rep(paste0("S", 1:6), each = n),
#'   level_m = c(
#'     8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.2)),
#'     8.3 + 0.7 * season + cumsum(rnorm(n,  0.01, 0.2)),
#'     7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.2)),
#'     7.0 + 0.6 * season + cumsum(rnorm(n,  0.00, 0.2)),
#'     9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.2)),
#'     9.3 + 1.0 * season + cumsum(rnorm(n, -0.02, 0.2))
#'   )
#' )
#'
#' # Default: PCA + hclust, auto k, binary features
#' clustered <- cluster_bouquet(gw_long,
#'   time_col = week, series_col = station, value_col = level_m)
#'
#' # Inspect the result
#' summary(clustered)
#'
#' # Plot silhouette scores across k to inspect the elbow
#' \donttest{
#' plot_cluster_quality(clustered)
#' }
#'
#' @export
#' @importFrom dplyr all_of case_when filter group_by left_join mutate rename
#'   row_number select ungroup .data
#' @importFrom rlang ':='
#' @importFrom stats as.dist cor cutree dist hclust kmeans prcomp setNames
#' @importFrom tibble tibble
cluster_bouquet <- function(
    data,
    time_col     = 1,
    series_col   = 2,
    value_col    = 3,
    k            = "auto",
    k_max        = 8L,
    method       = c("pca_hclust", "pca_kmeans", "hclust", "kmeans", "pam"),
    distance     = c("euclidean", "correlation", "manhattan"),
    resolution   = 0.5,
    pca_variance = 0.90,
    cluster_col  = "cluster",
    normalise    = FALSE,
    seed         = NULL,
    verbose      = FALSE) {

  method   <- match.arg(method)
  distance <- match.arg(distance)

  # -- Validate inputs ----------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data frame or tibble.", call. = FALSE)

  if (!is.logical(normalise) || length(normalise) != 1L)
    stop("`normalise` must be TRUE or FALSE.", call. = FALSE)

  if (!identical(k, "auto")) {
    if (!is.numeric(k) || length(k) != 1L || k < 2L || k != round(k))
      stop('`k` must be a positive integer >= 2 or "auto".', call. = FALSE)
    k <- as.integer(k)
  }

  k_max <- as.integer(k_max)
  if (length(k_max) != 1L || k_max < 2L)
    stop("`k_max` must be a single integer >= 2.", call. = FALSE)

  if (!is.numeric(resolution) || length(resolution) != 1L || resolution < 0)
    stop("`resolution` must be a single non-negative number.", call. = FALSE)

  if (!is.numeric(pca_variance) || length(pca_variance) != 1L ||
      pca_variance <= 0 || pca_variance > 1)
    stop("`pca_variance` must be a single number in (0, 1].", call. = FALSE)

  if (!is.character(cluster_col) || length(cluster_col) != 1L ||
      !nzchar(cluster_col))
    stop("`cluster_col` must be a non-empty string.", call. = FALSE)

  if (!is.logical(verbose) || length(verbose) != 1L)
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L))
    stop("`seed` must be a single integer or NULL.", call. = FALSE)

  if (method == "pam" && !requireNamespace("cluster", quietly = TRUE))
    stop('method = "pam" requires the cluster package. ',
         'Install it with install.packages("cluster").', call. = FALSE)

  # Warn when distance is set but will have no effect
  distance_unused <- startsWith(method, "pca_") || method == "kmeans"
  if (distance_unused && distance != "euclidean")
    warning(sprintf('`distance = "%s"` has no effect for method = "%s".',
                    distance, method), call. = FALSE)

  if (cluster_col %in% names(data))
    warning(sprintf('Column "%s" already exists and will be overwritten.',
                    cluster_col), call. = FALSE)

  # -- Resolve column names -----------------------------------------------------
  time_col_name   <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_col_name <- names(dplyr::select(data, {{ series_col }}))[1L]
  value_col_name  <- names(dplyr::select(data, {{ value_col }}))[1L]

  if (!is.numeric(data[[value_col_name]]))
    stop(sprintf("Column '%s' must be numeric.", value_col_name), call. = FALSE)

  # -- Build feature matrix -----------------------------------------------------
  # normalise = FALSE / TRUE  ->  binary +1/-1/0 direction sequences
  # normalise = "magnitude"   ->  raw signed deltas, globally range-normalised
  dir_long <- data |>
    dplyr::rename(
      bq_t = dplyr::all_of(time_col_name),
      bq_s = dplyr::all_of(series_col_name),
      bq_v = dplyr::all_of(value_col_name)
    ) |>
    dplyr::group_by(.data$bq_s) |>
    dplyr::mutate(
      step  = dplyr::row_number(),
      delta = .data$bq_v - dplyr::lag(.data$bq_v),
      dir   = dplyr::case_when(
        is.na(.data$delta) ~  0L,
        .data$delta > 0    ~  1L,
        .data$delta < 0    ~ -1L,
        TRUE               ~  0L
      )
    ) |>
    dplyr::filter(.data$step > 1L) |>
    dplyr::select("bq_s", "step", "dir") |>
    dplyr::ungroup()

  series_names <- sort(unique(as.character(dir_long$bq_s)))
  n_series     <- length(series_names)
  n_steps      <- max(dir_long$step)
  n_features   <- n_steps - 1L

  if (n_series < 3L)
    stop("At least 3 series are required for meaningful clustering.", call. = FALSE)

  feat_mat <- matrix(0, nrow = n_series, ncol = n_features,
                     dimnames = list(series_names, NULL))

  # Binary +1/-1/0 direction sequences
  feat_mat <- matrix(0L, nrow = n_series, ncol = n_features,
                     dimnames = list(series_names, NULL))
  for (sn in series_names) {
    rows           <- dir_long[dir_long$bq_s == sn, ]
    feat_mat[sn, ] <- rows$dir[order(rows$step)]
  }

  # -- Distance matrix ----------------------------------------------------------
  dist_mat <- .bouquet_dist(feat_mat, distance)

  # -- PCA compression ----------------------------------------------------------
  feat_clust <- if (startsWith(method, "pca_")) {
    .pca_compress(feat_mat, pca_variance, verbose)
  } else {
    feat_mat
  }

  dist_clust <- if (startsWith(method, "pca_")) {
    stats::dist(feat_clust, method = "euclidean")
  } else {
    dist_mat
  }

  # -- Determine k --------------------------------------------------------------
  k_max_eff <- min(k_max, n_series - 1L)

  score_tbl <- NULL   # will be filled when k = "auto"

  if (identical(k, "auto")) {
    result <- .select_k_bouquet(
      feat_clust = feat_clust,
      dist_clust = dist_clust,
      dist_sil   = dist_mat,
      method     = method,
      k_max      = k_max_eff,
      res        = resolution,
      verbose    = verbose,
      seed       = seed
    )
    k         <- result$k
    score_tbl <- result$score_tbl
  } else {
    if (k > n_series - 1L)
      stop(sprintf("`k` (%d) must be less than the number of series (%d).",
                   k, n_series), call. = FALSE)
  }

  # -- Cluster ------------------------------------------------------------------
  if (!is.null(seed)) set.seed(seed)
  labels <- .run_clustering_bouquet(feat_clust, dist_clust, k, method)

  # -- Relabel C1, C2, ... by decreasing cluster size ---------------------------
  size_order    <- names(sort(table(labels), decreasing = TRUE))
  rank_map      <- stats::setNames(paste0("C", seq_along(size_order)), size_order)
  labels_factor <- factor(rank_map[as.character(labels)],
                          levels = paste0("C", seq_len(k)))

  # -- Silhouette diagnostics ---------------------------------------------------
  sil_obj  <- cluster::silhouette(as.integer(labels_factor), dist_mat)
  mean_sil <- mean(sil_obj[, "sil_width"])
  sil_tbl  <- data.frame(
    series  = series_names,
    cluster = as.character(labels_factor),
    sil     = sil_obj[, "sil_width"],
    stringsAsFactors = FALSE
  )

  sizes    <- table(labels_factor)
  members  <- split(series_names, labels_factor)

  # -- Verbose output ------------------------------------------------------------
  if (verbose) {
    cat(sprintf(
      "\ncluster_bouquet  |  method = %s  |  distance = %s\n",
      method, if (distance_unused) paste0(distance, " (unused)") else distance
    ))
    cat(sprintf("k = %d  |  mean silhouette = %.3f  |  resolution = %.2f\n",
                k, mean_sil, resolution))
    cat("Cluster sizes:\n")
    for (cl in names(sizes))
      cat(sprintf("  %s : %d series (%s)\n", cl, sizes[[cl]],
                  paste(members[[cl]], collapse = ", ")))
    cat("\n")
  }

  # -- Join cluster labels back onto data ---------------------------------------
  label_tbl            <- tibble::tibble(bq_s = series_names, .cl = labels_factor)
  names(label_tbl)[2L] <- cluster_col

  out <- data |>
    dplyr::rename(bq_s = dplyr::all_of(series_col_name)) |>
    dplyr::left_join(label_tbl, by = "bq_s") |>
    dplyr::rename(!!series_col_name := "bq_s")

  # -- Attach S3 class and metadata ---------------------------------------------
  attr(out, "bq_meta") <- list(
    k            = k,
    method       = method,
    distance     = if (distance_unused) paste0(distance, " (unused)") else distance,
    resolution   = resolution,
    mean_sil     = mean_sil,
    sizes        = sizes,
    members      = members,
    sil_tbl      = sil_tbl,
    score_tbl    = score_tbl,
    cluster_col  = cluster_col,
    normalise    = normalise,
    seed         = seed,
    n_series     = n_series
  )
  class(out) <- c("cluster_bouquet", class(out))
  out
}


# -- S3 methods -----------------------------------------------------------------

#' Summarise a cluster_bouquet result
#'
#' Prints a structured report of the clustering: method, k selection scores
#' (when k was chosen automatically), silhouette quality, cluster sizes, and
#' per-series silhouette widths.
#'
#' @param object A `cluster_bouquet` object returned by [cluster_bouquet()].
#' @param ... Unused; present for S3 compatibility.
#'
#' @return `object`, invisibly (so the result can still be piped).
#'
#' @examples
#' set.seed(42)
#' n <- 52L
#' gw <- tibble::tibble(
#'   week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 4L),
#'   station = rep(paste0("S", 1:4), each = n),
#'   level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)),
#'               cumsum(rnorm(n)), cumsum(rnorm(n)))
#' )
#' summary(cluster_bouquet(gw, week, station, level))
#'
#' @export
summary.cluster_bouquet <- function(object, ...) {
  m <- attr(object, "bq_meta")
  if (is.null(m)) {
    cat("No cluster_bouquet metadata found.\n")
    return(invisible(object))
  }

  cat("-- cluster_bouquet summary ----------------------------------------------\n")
  cat(sprintf("  Method    : %s\n", m$method))
  cat(sprintf("  Distance  : %s\n", m$distance))
  cat(sprintf("  Normalise : %s\n", if (isTRUE(m$normalise)) "TRUE" else "FALSE"))
  cat(sprintf("  Seed      : %s\n", if (is.null(m$seed)) "none" else m$seed))
  cat(sprintf("  Series    : %d   k = %d   Resolution = %.2f\n",
              m$n_series, m$k, m$resolution))
  cat(sprintf("  Mean silhouette : %.3f  ", m$mean_sil))
  cat(dplyr::case_when(
    m$mean_sil >= 0.50 ~ "(strong structure)\n",
    m$mean_sil >= 0.25 ~ "(reasonable structure)\n",
    TRUE               ~ "(weak structure -- consider more data or fewer clusters)\n"
  ))

  if (!is.null(m$score_tbl)) {
    cat("\n  Auto k selection (composite silhouette score):\n")
    for (i in seq_len(nrow(m$score_tbl))) {
      row <- m$score_tbl[i, ]
      cat(sprintf("    k = %d : %.4f%s\n", row$k, row$score,
                  if (row$k == m$k) "  <-- selected" else ""))
    }
  }

  cat("\n  Cluster sizes and members:\n")
  for (cl in names(m$members)) {
    sil_cl <- mean(m$sil_tbl$sil[m$sil_tbl$cluster == cl])
    cat(sprintf("    %s  (%d series, mean sil = %.3f)\n",
                cl, length(m$members[[cl]]), sil_cl))
    cat(sprintf("       %s\n", paste(m$members[[cl]], collapse = ", ")))
  }

  cat("\n  Per-series silhouette widths:\n")
  sil_sorted <- m$sil_tbl[order(m$sil_tbl$cluster, -m$sil_tbl$sil), ]
  for (i in seq_len(nrow(sil_sorted))) {
    r <- sil_sorted[i, ]
    bar <- strrep("|", max(0L, round(r$sil * 20)))
    cat(sprintf("    %-20s  %s  %6.3f  %s\n",
                r$series, r$cluster, r$sil, bar))
  }
  cat("------------------------------------------------------------------------\n")
  invisible(object)
}


#' Plot silhouette scores across k values for a cluster_bouquet result
#'
#' Draws a bar chart of the composite silhouette score (or plain mean
#' silhouette if `resolution = 0`) across all candidate k values evaluated
#' during automatic k selection. The selected k is highlighted. Use this to
#' inspect the elbow and decide whether a different k might be preferable.
#'
#' This function requires that [cluster_bouquet()] was called with
#' `k = "auto"` -- if a fixed k was supplied there are no scores to plot.
#'
#' @param x A `cluster_bouquet` object returned by [cluster_bouquet()].
#' @param dark_mode Logical. Match the dark background of [make_plot_bouquet()].
#'   Default `FALSE`.
#'
#' @return A [ggplot2::ggplot] object (invisibly), also printed to the device.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' n <- 52L
#' gw <- tibble::tibble(
#'   week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 6L),
#'   station = rep(paste0("S", 1:6), each = n),
#'   level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)),
#'               cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)))
#' )
#' result <- cluster_bouquet(gw, week, station, level)
#' plot_cluster_quality(result)
#' }
#'
#' @export
#' @importFrom ggplot2 aes element_blank element_rect element_text geom_bar
#'   geom_text ggplot labs margin scale_fill_manual theme theme_minimal
plot_cluster_quality <- function(x, dark_mode = FALSE) {
  if (!inherits(x, "cluster_bouquet"))
    stop("`x` must be a cluster_bouquet object returned by cluster_bouquet().",
         call. = FALSE)
  m <- attr(x, "bq_meta")
  if (is.null(m$score_tbl))
    stop(
      "No k-selection scores found. ",
      "plot_cluster_quality() requires cluster_bouquet() to have been called ",
      "with k = \"auto\".",
      call. = FALSE
    )

  df <- m$score_tbl
  df$selected <- df$k == m$k

  bg_col      <- if (dark_mode) "#1a1a2e" else "#f8f8f5"
  text_col    <- if (dark_mode) "white"   else "#1a1a2e"
  sub_col     <- if (dark_mode) "grey60"  else "grey40"
  bar_default <- if (dark_mode) "#3a7d2c" else "#52a85e"
  bar_sel     <- "#f472b6"

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = factor(.data$k),
    y    = .data$score,
    fill = .data$selected
  )) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$score)),
      vjust = -0.4, size = 3.2, color = sub_col
    ) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = bar_default, "TRUE" = bar_sel),
      guide  = "none"
    ) +
    ggplot2::labs(
      title    = "Cluster quality across k",
      subtitle = sprintf(
        "method = %s | distance = %s | resolution = %.2f | selected k = %d",
        m$method, m$distance, m$resolution, m$k
      ),
      x = "Number of clusters (k)",
      y = "Composite silhouette score"
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = bg_col, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_col, color = NA),
      plot.title       = ggplot2::element_text(color = text_col, size = 14,
                                               margin = ggplot2::margin(b = 4)),
      plot.subtitle    = ggplot2::element_text(color = sub_col, size = 9,
                                               margin = ggplot2::margin(b = 10)),
      axis.title       = ggplot2::element_text(color = sub_col, size = 10),
      axis.text        = ggplot2::element_text(color = sub_col, size = 10),
      panel.grid.major.x = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(20, 20, 15, 20)
    )

  print(p)
  invisible(p)
}


# -- Unexported helpers ---------------------------------------------------------

#' @noRd
.bouquet_dist <- function(feat_mat, distance) {
  if (distance == "correlation") {
    r   <- stats::cor(t(feat_mat))
    r[] <- pmax(-1, pmin(1, r))
    stats::as.dist(1 - r)
  } else {
    stats::dist(feat_mat,
                method = if (distance == "manhattan") "manhattan" else "euclidean")
  }
}

#' @noRd
.pca_compress <- function(feat_mat, pca_variance, verbose) {
  pca     <- stats::prcomp(feat_mat, center = TRUE, scale. = FALSE)
  cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  n_comp  <- max(2L, which(cum_var >= pca_variance)[1L])
  if (verbose)
    cat(sprintf("PCA: retaining %d / %d components (%.0f%% variance explained)\n",
                n_comp, ncol(feat_mat), cum_var[n_comp] * 100))
  pca$x[, seq_len(n_comp), drop = FALSE]
}

#' @noRd
.run_clustering_bouquet <- function(feat_clust, dist_clust, k, method) {
  switch(method,
    pca_hclust = ,
    hclust     = stats::cutree(stats::hclust(dist_clust, method = "ward.D2"), k = k),
    pca_kmeans = ,
    kmeans     = stats::kmeans(feat_clust, centers = k,
                               nstart = 25L, iter.max = 100L)$cluster,
    pam        = cluster::pam(dist_clust, k = k, diss = TRUE)$clustering
  )
}

#' @noRd
.composite_score <- function(labels, dist_sil, resolution) {
  if (length(unique(labels)) < 2L) return(-Inf)
  sil        <- cluster::silhouette(labels, dist_sil)[, "sil_width"]
  mean_sil   <- mean(sil)
  cl_means   <- tapply(sil, labels, mean)
  min_cl_sil <- min(cl_means)
  if (mean_sil <= 0 || min_cl_sil <= 0) return(mean_sil)
  mean_sil * min_cl_sil * length(unique(labels))^resolution
}

#' @noRd
.select_k_bouquet <- function(feat_clust, dist_clust, dist_sil,
                               method, k_max, res, verbose, seed) {
  scores <- numeric(k_max - 1L)
  for (ki in seq(2L, k_max)) {
    if (!is.null(seed)) set.seed(seed)
    labels          <- .run_clustering_bouquet(feat_clust, dist_clust, ki, method)
    scores[ki - 1L] <- .composite_score(as.integer(labels), dist_sil, res)
  }
  best_k    <- which.max(scores) + 1L
  score_tbl <- data.frame(k = seq(2L, k_max), score = scores)

  if (verbose) {
    cat("\nAuto k selection (composite silhouette score):\n")
    for (ki in seq(2L, k_max))
      cat(sprintf("  k = %d : %.4f%s\n", ki, scores[ki - 1L],
                  if (ki == best_k) " <-- selected" else ""))
  }

  list(k = best_k, score_tbl = score_tbl)
}