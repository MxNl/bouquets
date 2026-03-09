#' Cluster Time Series by Directional Dynamics
#'
#' @description
#' Encodes each time series as a sequence of signed directional steps
#' (\eqn{+1} increase, \eqn{-1} decrease, \eqn{0} no change) and clusters
#' the series based on the similarity of those sequences. The resulting
#' `cluster` column can be passed directly to [make_plot_bouquet()] via
#' `stem_colors`, `flower_colors`, or `facet_by`.
#'
#' @param data A long-format data frame or tibble — the same format expected
#'   by [make_plot_bouquet()].
#' @param time_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The time index
#'   column. Defaults to the first column.
#' @param series_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   identifying each individual time series. Defaults to the second column.
#' @param value_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The numeric
#'   value column. Defaults to the third column.
#' @param k Either a positive integer specifying the number of clusters, or
#'   `"auto"` (default) to select the optimal number by maximising the
#'   composite silhouette score across `k = 2, …, k_max`.
#' @param k_max A single positive integer. The upper bound considered when
#'   `k = "auto"`. Capped internally at `n_series - 1`. Defaults to `8L`.
#' @param method The clustering algorithm. One of:
#'   \describe{
#'     \item{`"pca_hclust"`}{(Default) Projects direction sequences onto their
#'       principal components retaining `pca_variance` of variance, then
#'       applies agglomerative hierarchical clustering with Ward's D2 linkage
#'       in that space. The PCA step removes noise dimensions that dilute
#'       cluster separation in long series. Deterministic; no extra packages
#'       required. The `distance` argument does not apply — distance is always
#'       Euclidean in PCA space.}
#'     \item{`"pca_kmeans"`}{PCA projection followed by k-means. Faster for
#'       large numbers of series; non-deterministic (set a seed for
#'       reproducibility). The `distance` argument does not apply.}
#'     \item{`"hclust"`}{Hierarchical clustering with Ward's D2 directly on
#'       the direction sequences, using the `distance` metric. Best for short
#'       series (< ~50 steps) where the raw sequence distance is meaningful.}
#'     \item{`"kmeans"`}{k-means directly on the direction sequences. The
#'       `distance` argument does not apply (k-means always uses Euclidean
#'       internally). Suitable for short series with many observations.}
#'     \item{`"pam"`}{Partitioning Around Medoids using the `distance` metric.
#'       More robust to outlier series than k-means because cluster centres are
#'       always real observations. Requires the \pkg{cluster} package.}
#'   }
#' @param distance The distance metric applied to the raw direction sequences.
#'   One of:
#'   \describe{
#'     \item{`"euclidean"`}{(Default) Penalises step-by-step disagreements
#'       between sequences.}
#'     \item{`"correlation"`}{Converts Pearson correlation to a distance as
#'       \eqn{1 - r}, measuring whether series *co-move* in their ups and
#'       downs regardless of their absolute patterns. Often the most
#'       meaningful metric for hydrological series.}
#'     \item{`"manhattan"`}{Sum of absolute step differences. Less sensitive
#'       to occasional large divergences than Euclidean.}
#'   }
#'   **Note**: only applied by `"hclust"` and `"pam"`. The PCA methods
#'   always use Euclidean distance in the reduced space; k-means has no
#'   distance argument by design.
#' @param resolution A non-negative number controlling the preference for
#'   more vs fewer clusters during automatic k selection. The composite score
#'   is:
#'
#'   \deqn{\text{score}(k) = \bar{s}(k) \times \min_c \bar{s}_c(k) \times
#'   k^{\text{resolution}}}
#'
#'   where \eqn{\bar{s}(k)} is the overall mean silhouette width and
#'   \eqn{\min_c \bar{s}_c(k)} is the mean silhouette of the *worst* cluster.
#'   Increasing `resolution` rewards finer groupings as long as all clusters
#'   remain well-defined. `resolution = 0` falls back to plain mean
#'   silhouette. Defaults to `0.5`.
#' @param pca_variance A number in `(0, 1]`. The cumulative proportion of
#'   variance to retain during PCA compression. Only used by `"pca_hclust"`
#'   and `"pca_kmeans"`. Defaults to `0.90`.
#' @param cluster_col A string. The name of the column added to `data`.
#'   Defaults to `"cluster"`.
#' @param verbose Logical. Whether to print the k selection table, chosen k,
#'   mean silhouette, and cluster sizes. Defaults to `TRUE`.
#'
#' @return The original `data` tibble with one additional factor column named
#'   by `cluster_col`. Levels are `"C1"`, `"C2"`, … ordered by decreasing
#'   cluster size.
#'
#' @details
#' ## Feature representation
#' Each series of length \eqn{n} is represented as a vector of \eqn{n - 1}
#' signed steps \eqn{d_i \in \{-1, 0, +1\}}. These form the rows of a
#' feature matrix \eqn{M \in \mathbb{R}^{n_\text{series} \times (n-1)}}.
#'
#' ## Which method and distance to choose
#' For typical hydrological series (weekly to daily, many years):
#' \itemize{
#'   \item Start with `"pca_hclust"` (default). PCA compression removes the
#'     noise that makes long raw sequences hard to separate.
#'   \item If you suspect the series differ mainly in *co-movement* rather
#'     than exact pattern, try `method = "hclust", distance = "correlation"`.
#'     This is often the most geologically meaningful choice.
#'   \item Use `"pam"` when you want cluster representatives to be real
#'     observed series that you can point to in the data.
#' }
#'
#' ## Composite k-selection score
#' Plain mean silhouette consistently selects k = 2 because two large blobs
#' almost always score well globally even when finer structure exists. The
#' composite score adds two corrections: the worst-cluster term
#' \eqn{\min_c \bar{s}_c(k)} kills solutions where one cluster is
#' ill-defined, and the resolution exponent \eqn{k^\text{resolution}} rewards
#' cleaner solutions at higher k.
#'
#' ## Pairing with `make_plot_bouquet()`
#' ```r
#' data |>
#'   cluster_bouquet(time_col = date, series_col = id, value_col = gwl) |>
#'   make_plot_bouquet(
#'     time_col      = date,
#'     series_col    = id,
#'     value_col     = gwl,
#'     stem_colors   = cluster,
#'     flower_colors = cluster,
#'     facet_by      = cluster
#'   )
#' ```
#'
#' @seealso [make_plot_bouquet()] for visualising the result.
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
#' # Default: PCA + hclust, auto k
#' clustered <- cluster_bouquet(gw_long,
#'   time_col = week, series_col = station, value_col = level_m)
#'
#' # Correlation distance with hclust — often best for hydrological series
#' clustered_corr <- cluster_bouquet(gw_long,
#'   time_col   = week,
#'   series_col = station,
#'   value_col  = level_m,
#'   method     = "hclust",
#'   distance   = "correlation"
#' )
#'
#' # Increase resolution to push toward finer, cleaner clusters
#' clustered_fine <- cluster_bouquet(gw_long,
#'   time_col   = week,
#'   series_col = station,
#'   value_col  = level_m,
#'   resolution = 1.5
#' )
#'
#' # Visualise — colour and facet by cluster
#' \donttest{
#' make_plot_bouquet(clustered,
#'   time_col      = week,
#'   series_col    = station,
#'   value_col     = level_m,
#'   stem_colors   = cluster,
#'   flower_colors = cluster,
#'   facet_by      = cluster,
#'   title         = "Groundwater Stations by Directional Cluster"
#' )
#' }
#'
#' @export
#'
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
    verbose      = TRUE) {

  method   <- match.arg(method)
  distance <- match.arg(distance)

  # ── Validate inputs ────────────────────────────────────────────────────────
  if (!is.data.frame(data))
    stop("`data` must be a data frame or tibble.", call. = FALSE)

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

  if (method == "pam" && !requireNamespace("cluster", quietly = TRUE))
    stop(
      'method = "pam" requires the cluster package. ',
      'Install it with install.packages("cluster").',
      call. = FALSE
    )

  # Warn when distance is set but will have no effect
  distance_unused <- startsWith(method, "pca_") || method == "kmeans"
  if (distance_unused && distance != "euclidean")
    warning(
      sprintf(
        '`distance = "%s"` has no effect for method = "%s". ',
        distance, method
      ),
      "Distance is always Euclidean for PCA methods; ",
      "k-means has no distance argument by design.",
      call. = FALSE
    )

  if (cluster_col %in% names(data))
    warning(
      sprintf('Column "%s" already exists and will be overwritten.', cluster_col),
      call. = FALSE
    )

  # ── Resolve column names ───────────────────────────────────────────────────
  time_col_name   <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_col_name <- names(dplyr::select(data, {{ series_col }}))[1L]
  value_col_name  <- names(dplyr::select(data, {{ value_col }}))[1L]

  if (!is.numeric(data[[value_col_name]]))
    stop(sprintf("Column '%s' must be numeric.", value_col_name), call. = FALSE)

  # ── Build direction-sequence feature matrix ────────────────────────────────
  # Each series → a row of signed steps (+1 / -1 / 0), length n_steps - 1.
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
    stop("At least 3 series are required for meaningful clustering.",
         call. = FALSE)

  # Build matrix without a tidyr dependency
  feat_mat <- matrix(
    0L,
    nrow     = n_series,
    ncol     = n_features,
    dimnames = list(series_names, NULL)
  )
  for (sn in series_names) {
    rows           <- dir_long[dir_long$bq_s == sn, ]
    feat_mat[sn, ] <- rows$dir[order(rows$step)]
  }

  # ── Distance matrix on raw features (used by hclust, pam, silhouette) ─────
  dist_mat <- .bouquet_dist(feat_mat, distance)

  # ── PCA compression (pca_* methods only) ──────────────────────────────────
  # Distance in PCA space is always Euclidean — the distance argument does
  # not apply after the feature space has been reorganised by PCA.
  feat_clust <- if (startsWith(method, "pca_")) {
    .pca_compress(feat_mat, pca_variance, verbose)
  } else {
    feat_mat
  }

  # Distance used for clustering (PCA methods use their own Euclidean dist)
  dist_clust <- if (startsWith(method, "pca_")) {
    stats::dist(feat_clust, method = "euclidean")
  } else {
    dist_mat
  }

  # ── Determine k ───────────────────────────────────────────────────────────
  k_max_eff <- min(k_max, n_series - 1L)

  if (identical(k, "auto")) {
    k <- .select_k_bouquet(
      feat_clust = feat_clust,
      dist_clust = dist_clust,
      dist_sil   = dist_mat,    # silhouette always in original feature space
      method     = method,
      k_max      = k_max_eff,
      res        = resolution,
      verbose    = verbose
    )
  } else {
    if (k > n_series - 1L)
      stop(
        sprintf("`k` (%d) must be less than the number of series (%d).",
                k, n_series),
        call. = FALSE
      )
  }

  # ── Cluster ───────────────────────────────────────────────────────────────
  labels <- .run_clustering_bouquet(feat_clust, dist_clust, k, method)

  # ── Relabel C1, C2, … by decreasing cluster size ─────────────────────────
  size_order    <- names(sort(table(labels), decreasing = TRUE))
  rank_map      <- stats::setNames(paste0("C", seq_along(size_order)),
                                   size_order)
  labels_factor <- factor(
    rank_map[as.character(labels)],
    levels = paste0("C", seq_len(k))
  )

  # ── Report ─────────────────────────────────────────────────────────────────
  if (verbose) {
    sil      <- cluster::silhouette(as.integer(labels_factor), dist_mat)
    mean_sil <- mean(sil[, "sil_width"])
    sizes    <- table(labels_factor)
    cat(sprintf(
      "\ncluster_bouquet  |  method = %s  |  distance = %s\n",
      method, if (distance_unused) paste0(distance, " (unused)") else distance
    ))
    cat(sprintf(
      "k = %d  |  mean silhouette = %.3f  |  resolution = %.2f\n",
      k, mean_sil, resolution
    ))
    cat("Cluster sizes:\n")
    for (cl in names(sizes))
      cat(sprintf("  %s : %d series\n", cl, sizes[[cl]]))
    cat("\n")
  }

  # ── Join cluster labels back onto data ────────────────────────────────────
  label_tbl            <- tibble::tibble(bq_s = series_names,
                                         .cl  = labels_factor)
  names(label_tbl)[2L] <- cluster_col

  data |>
    dplyr::rename(bq_s = dplyr::all_of(series_col_name)) |>
    dplyr::left_join(label_tbl, by = "bq_s") |>
    dplyr::rename(!!series_col_name := "bq_s")
}


# ── Unexported helpers ─────────────────────────────────────────────────────────

#' Build a distance matrix from the raw feature matrix
#'
#' For `"correlation"`, converts Pearson r to distance as `1 - r`.
#' @noRd
.bouquet_dist <- function(feat_mat, distance) {
  if (distance == "correlation") {
    r   <- stats::cor(t(feat_mat))
    r[] <- pmax(-1, pmin(1, r))   # guard against floating-point drift
    as.dist(1 - r)
  } else {
    stats::dist(feat_mat,
                method = if (distance == "manhattan") "manhattan" else "euclidean")
  }
}


#' Compress feature matrix via PCA
#'
#' Retains the minimum number of components (at least 2) needed to explain
#' `pca_variance` of total variance.
#' @noRd
.pca_compress <- function(feat_mat, pca_variance, verbose) {
  pca     <- stats::prcomp(feat_mat, center = TRUE, scale. = FALSE)
  cum_var <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  n_comp  <- max(2L, which(cum_var >= pca_variance)[1L])

  if (verbose)
    cat(sprintf(
      "PCA: retaining %d / %d components (%.0f%% variance explained)\n",
      n_comp, ncol(feat_mat), cum_var[n_comp] * 100
    ))

  pca$x[, seq_len(n_comp), drop = FALSE]
}


#' Run one clustering pass and return an integer label vector
#'
#' `feat_clust` is the feature matrix (raw or PCA-compressed).
#' `dist_clust` is the pre-computed distance for that feature space.
#' @noRd
.run_clustering_bouquet <- function(feat_clust, dist_clust, k, method) {
  switch(method,
    pca_hclust = ,
    hclust     = {
      stats::cutree(
        stats::hclust(dist_clust, method = "ward.D2"),
        k = k
      )
    },
    pca_kmeans = ,
    kmeans     = {
      stats::kmeans(feat_clust, centers = k,
                    nstart = 25L, iter.max = 100L)$cluster
    },
    pam = {
      cluster::pam(dist_clust, k = k, diss = TRUE)$clustering
    }
  )
}


#' Composite silhouette score that favours clean, well-separated clusters
#'
#' score(k) = mean_sil(k) * min_cluster_sil(k) * k^resolution
#'
#' The worst-cluster term kills solutions where any one cluster is
#' ill-defined. The resolution exponent rewards finer groupings as long as
#' all clusters remain well-separated. Silhouette is always computed in the
#' original (non-PCA) feature space so comparisons across k are consistent.
#' @noRd
.composite_score <- function(labels, dist_sil, resolution) {
  if (length(unique(labels)) < 2L) return(-Inf)

  sil        <- cluster::silhouette(labels, dist_sil)[, "sil_width"]
  mean_sil   <- mean(sil)
  cl_means   <- tapply(sil, labels, mean)
  min_cl_sil <- min(cl_means)

  # Avoid spurious positive scores from two negatives multiplying
  if (mean_sil <= 0 || min_cl_sil <= 0) return(mean_sil)

  mean_sil * min_cl_sil * length(unique(labels))^resolution
}


#' Select optimal k via composite silhouette score
#' @noRd
.select_k_bouquet <- function(feat_clust, dist_clust, dist_sil,
                               method, k_max, res, verbose) {
  scores <- numeric(k_max - 1L)

  for (ki in seq(2L, k_max)) {
    labels          <- .run_clustering_bouquet(feat_clust, dist_clust, ki, method)
    scores[ki - 1L] <- .composite_score(as.integer(labels), dist_sil, res)
  }

  best_k <- which.max(scores) + 1L

  if (verbose) {
    cat("\nAuto k selection (composite silhouette score):\n")
    for (ki in seq(2L, k_max))
      cat(sprintf("  k = %d : %.4f%s\n",
                  ki, scores[ki - 1L],
                  if (ki == best_k) " <-- selected" else ""))
  }

  best_k
}