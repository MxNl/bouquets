#' Cluster Time Series by Directional Dynamics
#'
#' @description
#' Encodes each time series as a feature vector and clusters the series based
#' on similarity. The feature representation is controlled by `method` and
#' `normalise`, and should match the value used in [make_plot_bouquet()] so
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
#' @param method The clustering algorithm and feature representation. One of:
#'   \describe{
#'     \item{\code{"coords_hclust"}}{(Default) Clusters on the actual (x, y)
#'       path coordinates. Paths that look similar in the plot cluster together.
#'       Uses Ward's D2 hierarchical clustering. Deterministic.}
#'     \item{\code{"coords_kmeans"}}{As \code{"coords_hclust"} but k-means.
#'       Set `seed` for reproducibility.}
#'     \item{\code{"coords_pam"}}{As \code{"coords_hclust"} but Partitioning
#'       Around Medoids. More robust to outliers. Requires \pkg{cluster}.}
#'     \item{\code{"heading_hclust"}}{Clusters on the cumulative heading angle
#'       sequence. Captures turning patterns independently of absolute position;
#'       rotation-invariant. Uses Ward's D2.}
#'     \item{\code{"heading_kmeans"}}{As \code{"heading_hclust"} but k-means.}
#'     \item{\code{"heading_pam"}}{As \code{"heading_hclust"} but PAM. Requires
#'       \pkg{cluster}.}
#'     \item{\code{"area_hclust"}}{Pairwise area distance: the area of the
#'       polygon enclosed by two paths from the shared origin, computed via the
#'       cross-product sum \eqn{\frac{1}{2}|\sum_i (x^{(1)}_i y^{(2)}_i -
#'       x^{(2)}_i y^{(1)}_i)|}. Paths spatially close and similarly shaped
#'       get small distances. Uses Ward's D2.}
#'     \item{\code{"area_pam"}}{As \code{"area_hclust"} but PAM. Requires
#'       \pkg{cluster}.}
#'   }
#' @param resolution A non-negative number. Composite k-selection score:
#'   \eqn{\bar{s}(k) \times \min_c \bar{s}_c(k) \times k^{\text{resolution}}}.
#'   Higher values favour finer clusters. `0` = plain mean silhouette.
#'   Default `0.5`.
#' @param cluster_col String. Name of the cluster column added to `data`.
#'   Default `"cluster"`.
#' @param normalise Logical. Controls path building for clustering. Should
#'   match the value passed to [make_plot_bouquet()] so that cluster
#'   assignments align visually with the rendered paths. \code{FALSE}
#'   (default) uses a single global \eqn{\theta}; \code{TRUE} normalises
#'   per series. See [make_plot_bouquet()] for full details.
#' @param ceiling_pct A single number in `(0, 1]`. Passed to path building for
#'   path-geometry methods. Should match the value used in
#'   [make_plot_bouquet()]. Default `0.80`.
#' @param launch_deg Initial heading in degrees for path building. Only used
#'   by path-geometry methods. Should match [make_plot_bouquet()]. Default
#'   `90`.
#' @param seed Integer or `NULL`. Random seed passed to [base::set.seed()]
#'   before k-means initialisation. Ensures reproducible results for
#'   `"coords_kmeans"` and `"heading_kmeans"`.
#'   Default `NULL` = no seed (non-reproducible).
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
#' ## Feature representations
#' **Path coordinates** (\code{"coords_*"}): each series is encoded as
#' \eqn{2(n-1)} values \eqn{(x_1, \ldots, x_{n-1}, y_1, \ldots, y_{n-1})}.
#' Directly reflects the visual plot -- paths close together cluster together.
#' Most intuitive for visual interpretation. This is the default.
#'
#' **Heading sequence** (\code{"heading_*"}): cumulative heading angle in
#' degrees at each step. Captures the turning pattern independently of
#' absolute position; rotation-invariant.
#'
#' **Area distance** (\code{"area_*"}): pairwise distance is the area of the
#' polygon enclosed by two paths (shoelace formula). Captures both shape
#' difference and spatial separation in a single scalar with a clean
#' geometric interpretation.
#'
#' ## Composite k-selection
#' Plain mean silhouette consistently selects k = 2. The composite score adds
#' a worst-cluster penalty and a resolution exponent to favour finer structure.
#'
#' ## Pairing with `make_plot_bouquet()`
#' Pass the same `normalise`, `ceiling_pct`, and `launch_deg` to both
#' functions. When a `cluster_bouquet` object is piped into
#' [make_plot_bouquet()], `normalise` is inherited automatically.
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
#' # Default: path-coordinate clustering (coords_hclust), auto k
#' clustered <- cluster_bouquet(gw_long,
#'   time_col = week, series_col = station, value_col = level_m)
#' summary(clustered)
#'
#' # Path-coordinate clustering -- most visually intuitive
#' \donttest{
#' cluster_bouquet(gw_long,
#'   time_col = week, series_col = station, value_col = level_m,
#'   method = "coords_hclust")
#'
#' # Area-based clustering
#' cluster_bouquet(gw_long,
#'   time_col = week, series_col = station, value_col = level_m,
#'   method = "area_hclust")
#' }
#'
#' @export
#' @importFrom dplyr all_of case_when filter group_by left_join mutate rename
#'   row_number select ungroup .data
#' @importFrom rlang ':='
#' @importFrom stats as.dist cutree dist hclust kmeans setNames
#' @importFrom tibble tibble
cluster_bouquet <- function(
    data,
    time_col     = 1,
    series_col   = 2,
    value_col    = 3,
    k            = "auto",
    k_max        = 8L,
    method       = c("coords_hclust", "coords_kmeans", "coords_pam",
                     "heading_hclust", "heading_kmeans", "heading_pam",
                     "area_hclust", "area_pam"),
    resolution   = 0.5,
    cluster_col  = "cluster",
    normalise    = FALSE,
    ceiling_pct  = 0.80,
    launch_deg   = 90,
    seed         = NULL,
    verbose      = FALSE) {

  method <- match.arg(method)

  # Convenience groups
  area_methods <- c("area_hclust", "area_pam")
  pam_methods  <- c("coords_pam", "heading_pam", "area_pam")

  # -- Validate inputs ----------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data frame or tibble.", call. = FALSE)

  if (!is.logical(normalise) || length(normalise) != 1L)
    stop("`normalise` must be TRUE or FALSE.", call. = FALSE)

  if (!is.numeric(ceiling_pct) || length(ceiling_pct) != 1L ||
      ceiling_pct <= 0 || ceiling_pct > 1)
    stop("`ceiling_pct` must be a single number in (0, 1].", call. = FALSE)

  if (!is.numeric(launch_deg) || length(launch_deg) != 1L)
    stop("`launch_deg` must be a single number.", call. = FALSE)

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

  if (!is.character(cluster_col) || length(cluster_col) != 1L ||
      !nzchar(cluster_col))
    stop("`cluster_col` must be a non-empty string.", call. = FALSE)

  if (!is.logical(verbose) || length(verbose) != 1L)
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)

  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1L))
    stop("`seed` must be a single integer or NULL.", call. = FALSE)

  if (method %in% pam_methods && !requireNamespace("cluster", quietly = TRUE))
    stop(sprintf('method = "%s" requires the cluster package. ', method),
         'Install it with install.packages("cluster").', call. = FALSE)

  if (cluster_col %in% names(data))
    warning(sprintf('Column "%s" already exists and will be overwritten.',
                    cluster_col), call. = FALSE)

  # -- Resolve column names -----------------------------------------------------
  time_col_name   <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_col_name <- names(dplyr::select(data, {{ series_col }}))[1L]
  value_col_name  <- names(dplyr::select(data, {{ value_col }}))[1L]

  if (!is.numeric(data[[value_col_name]]))
    stop(sprintf("Column '%s' must be numeric.", value_col_name), call. = FALSE)

  # Resolve series names early (needed for matrix row names in all branches)
  series_names <- sort(unique(as.character(data[[series_col_name]])))
  n_series     <- length(series_names)

  if (n_series < 3L)
    stop("At least 3 series are required for meaningful clustering.", call. = FALSE)

  # -- Build feature / distance matrix -----------------------------------------
  # All methods work from the actual path geometry, so we build paths first.
  paths     <- .build_bouquet_paths(data, time_col_name, series_col_name,
                                     value_col_name, ceiling_pct, launch_deg,
                                     verbose = FALSE, normalise = normalise)
  path_data <- paths$path_data
  n_steps   <- max(path_data$step)
  n_feat    <- n_steps - 1L   # exclude origin (step = 1, always (0,0))

  # Pre-sort each series' non-origin path rows once
  path_list <- lapply(series_names, function(sn) {
    rows <- path_data[path_data$series == sn & path_data$step > 1L, ]
    rows[order(rows$step), ]
  })
  names(path_list) <- series_names

  if (method %in% area_methods) {
    # Option C: pairwise area distance via shoelace cross-product sum
    # dist(s1,s2) = 0.5 * |sum_i( x1_i*y2_i - x2_i*y1_i )|
    dist_mat   <- .area_dist_bouquet(path_list, series_names)
    feat_mat   <- NULL
    feat_clust <- NULL
    dist_clust <- dist_mat

  } else if (startsWith(method, "coords_")) {
    # Option A: flattened (x, y) coordinates -- 2*(n-1) features
    feat_mat <- matrix(0, nrow = n_series, ncol = 2L * n_feat,
                       dimnames = list(series_names, NULL))
    for (sn in series_names) {
      rows <- path_list[[sn]]
      feat_mat[sn, ] <- c(rows$x, rows$y)
    }
    dist_mat   <- stats::dist(feat_mat, method = "euclidean")
    feat_clust <- feat_mat
    dist_clust <- dist_mat

  } else {
    # Option B: cumulative heading sequence -- (n-1) features
    feat_mat <- matrix(0, nrow = n_series, ncol = n_feat,
                       dimnames = list(series_names, NULL))
    for (sn in series_names) {
      rows <- path_list[[sn]]
      feat_mat[sn, ] <- rows$heading_deg
    }
    dist_mat   <- stats::dist(feat_mat, method = "euclidean")
    feat_clust <- feat_mat
    dist_clust <- dist_mat
  }

  # -- Determine k --------------------------------------------------------------
  k_max_eff <- min(k_max, n_series - 1L)
  score_tbl <- NULL

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

  sizes   <- table(labels_factor)
  members <- split(series_names, labels_factor)

  # -- Verbose output -----------------------------------------------------------
  if (verbose) {
    cat(sprintf("\ncluster_bouquet  |  method = %s\n", method))
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
    k           = k,
    method      = method,
    resolution  = resolution,
    mean_sil    = mean_sil,
    sizes       = sizes,
    members     = members,
    sil_tbl     = sil_tbl,
    score_tbl   = score_tbl,
    cluster_col = cluster_col,
    normalise   = normalise,
    seed        = seed,
    n_series    = n_series
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
        "method = %s | resolution = %.2f | selected k = %d",
        m$method, m$resolution, m$k
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
.run_clustering_bouquet <- function(feat_clust, dist_clust, k, method) {
  switch(method,
    coords_hclust  = ,
    heading_hclust = ,
    area_hclust    = stats::cutree(stats::hclust(dist_clust, method = "ward.D2"), k = k),
    coords_kmeans  = ,
    heading_kmeans = stats::kmeans(feat_clust, centers = k,
                                   nstart = 25L, iter.max = 100L)$cluster,
    coords_pam     = ,
    heading_pam    = ,
    area_pam       = cluster::pam(dist_clust, k = k, diss = TRUE)$clustering
  )
}

#' Pairwise area distance between bouquet paths (shoelace cross-product sum)
#' @noRd
.area_dist_bouquet <- function(path_list, series_names) {
  n    <- length(series_names)
  dmat <- matrix(0, nrow = n, ncol = n,
                 dimnames = list(series_names, series_names))
  for (i in seq_len(n - 1L)) {
    p1 <- path_list[[series_names[i]]]
    for (j in seq(i + 1L, n)) {
      p2        <- path_list[[series_names[j]]]
      area      <- 0.5 * abs(sum(p1$x * p2$y - p2$x * p1$y))
      dmat[i, j] <- area
      dmat[j, i] <- area
    }
  }
  stats::as.dist(dmat)
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