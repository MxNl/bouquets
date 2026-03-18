library(testthat)
library(bouquets)
library(tibble)

# -- Shared test data ----------------------------------------------------------

make_gw <- function(n_stations = 6L, n = 52L, seed = 42L) {
  set.seed(seed)
  weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
  season <- sin(seq(0, 2 * pi, length.out = n))
  tibble::tibble(
    week    = rep(weeks, n_stations),
    station = rep(paste0("S", seq_len(n_stations)), each = n),
    lon     = rep(seq(7.0, 14.0, length.out = n_stations), each = n),
    lat     = rep(seq(48.0, 54.0, length.out = n_stations), each = n),
    level_m = as.vector(sapply(seq_len(n_stations), function(i)
      8.0 + 0.8 * season * (0.5 + i * 0.1) +
        cumsum(rnorm(n, (i - 3L) * 0.005, 0.2))
    ))
  )
}

gw <- make_gw()


# -- make_plot_bouquet() -------------------------------------------------------

test_that("make_plot_bouquet returns a ggplot for minimal call", {
  p <- make_plot_bouquet(gw, time_col = week, series_col = station,
                         value_col = level_m)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet verbose = FALSE produces no output", {
  out <- capture.output(suppressMessages(
    p <- make_plot_bouquet(gw, week, station, level_m, verbose = FALSE)
  ))
  expect_length(out, 0L)
})

test_that("make_plot_bouquet dark_mode works", {
  p <- make_plot_bouquet(gw, week, station, level_m, dark_mode = TRUE)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet show_rings and marker_every integer work", {
  p <- make_plot_bouquet(gw, week, station, level_m,
                         show_rings = TRUE, marker_every = 10L)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet marker_every keyword year works", {
  p <- make_plot_bouquet(gw, week, station, level_m, marker_every = "year")
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet marker_every keyword month works", {
  p <- make_plot_bouquet(gw, week, station, level_m, marker_every = "month")
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet marker_every bad keyword errors", {
  expect_error(
    make_plot_bouquet(gw, week, station, level_m, marker_every = "decade"),
    "year|quarter|month|week|day"
  )
})

test_that("make_plot_bouquet show_labels = TRUE works", {
  p <- make_plot_bouquet(gw, week, station, level_m, show_labels = TRUE)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet label_color overrides flower colour", {
  p <- make_plot_bouquet(gw, week, station, level_m,
                         show_labels = TRUE, label_color = "#333333")
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet label_color NULL uses flower colour", {
  p <- make_plot_bouquet(gw, week, station, level_m,
                         show_labels = TRUE, label_color = NULL)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet label_color bad value errors", {
  expect_error(
    make_plot_bouquet(gw, week, station, level_m,
                      show_labels = TRUE, label_color = 42L),
    "label_color"
  )
})

test_that("make_plot_bouquet highlight dims other series", {
  p <- make_plot_bouquet(gw, week, station, level_m, highlight = "S1")
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet highlight warns on unknown series", {
  expect_warning(
    make_plot_bouquet(gw, week, station, level_m, highlight = "UNKNOWN"),
    "not found"
  )
})

test_that("make_plot_bouquet highlight with show_labels renders", {
  p <- make_plot_bouquet(gw, week, station, level_m,
                         highlight = "S1", show_labels = TRUE)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet from/to filter works", {
  p <- make_plot_bouquet(gw, week, station, level_m,
                         from = as.Date("2023-04-01"),
                         to   = as.Date("2023-09-30"))
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet from/to that removes all data errors", {
  expect_error(
    make_plot_bouquet(gw, week, station, level_m,
                      from = as.Date("2099-01-01")),
    "No data remains"
  )
})

test_that("make_plot_bouquet normalise = TRUE works", {
  p <- make_plot_bouquet(gw, week, station, level_m, normalise = TRUE)
  expect_s3_class(p, "ggplot")
  expect_no_error(print(p))
})

test_that("make_plot_bouquet facet_by works", {
  gw2 <- gw
  gw2$region <- rep(c("A", "B"), each = nrow(gw) / 2L)
  p <- make_plot_bouquet(gw2, week, station, level_m, facet_by = region)
  expect_s3_class(p, "ggplot")
})

test_that("make_plot_bouquet stem_colors keyword greens works", {
  p <- make_plot_bouquet(gw, week, station, level_m, stem_colors = "greens")
  expect_s3_class(p, "ggplot")
})

test_that("make_plot_bouquet flower_colors keyword blossom works", {
  p <- make_plot_bouquet(gw, week, station, level_m,
                         flower_colors = "blossom")
  expect_s3_class(p, "ggplot")
})

test_that("make_plot_bouquet errors on bad ceiling_pct", {
  expect_error(make_plot_bouquet(gw, week, station, level_m,
                                 ceiling_pct = 1.5), "ceiling_pct")
  expect_error(make_plot_bouquet(gw, week, station, level_m,
                                 ceiling_pct = 0),   "ceiling_pct")
})

test_that("make_plot_bouquet errors on non-data-frame input", {
  expect_error(make_plot_bouquet(list(a = 1)), "data frame")
})


# -- cluster_bouquet() ---------------------------------------------------------

test_that("cluster_bouquet returns cluster_bouquet and tbl_df classes", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  expect_s3_class(cl, "cluster_bouquet")
  expect_true(is.data.frame(cl))
})

test_that("cluster_bouquet adds the cluster column", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  expect_true("cluster" %in% names(cl))
  expect_s3_class(cl$cluster, "factor")
})

test_that("cluster_bouquet cluster_col argument is respected", {
  cl <- cluster_bouquet(gw, week, station, level_m, cluster_col = "grp")
  expect_true("grp" %in% names(cl))
})

test_that("cluster_bouquet bq_meta attribute is present with required fields", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  m  <- attr(cl, "bq_meta")
  expect_type(m, "list")
  expect_true(all(c("k", "method", "mean_sil", "members",
                    "score_tbl", "sil_tbl") %in% names(m)))
})

test_that("cluster_bouquet default method is coords_hclust", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  expect_equal(attr(cl, "bq_meta")$method, "coords_hclust")
})

test_that("cluster_bouquet fixed k is respected", {
  cl <- cluster_bouquet(gw, week, station, level_m, k = 2L)
  expect_equal(attr(cl, "bq_meta")$k, 2L)
  expect_equal(nlevels(cl$cluster), 2L)
})

test_that("cluster_bouquet seed gives reproducible results for kmeans", {
  cl1 <- cluster_bouquet(gw, week, station, level_m,
                         method = "coords_kmeans", seed = 7L)
  cl2 <- cluster_bouquet(gw, week, station, level_m,
                         method = "coords_kmeans", seed = 7L)
  expect_identical(cl1$cluster, cl2$cluster)
})

test_that("cluster_bouquet verbose = FALSE produces no output", {
  out <- capture.output(suppressMessages(
    cl <- cluster_bouquet(gw, week, station, level_m, verbose = FALSE)
  ))
  expect_length(out, 0L)
})

test_that("cluster_bouquet all coords methods run without error", {
  for (m in c("coords_hclust", "coords_kmeans", "coords_pam")) {
    cl <- cluster_bouquet(gw, week, station, level_m, method = m, k = 2L,
                          seed = 1L)
    expect_s3_class(cl, "cluster_bouquet")
  }
})

test_that("cluster_bouquet all heading methods run without error", {
  for (m in c("heading_hclust", "heading_kmeans", "heading_pam")) {
    cl <- cluster_bouquet(gw, week, station, level_m, method = m, k = 2L,
                          seed = 1L)
    expect_s3_class(cl, "cluster_bouquet")
  }
})

test_that("cluster_bouquet all area methods run without error", {
  for (m in c("area_hclust", "area_pam")) {
    cl <- cluster_bouquet(gw, week, station, level_m, method = m, k = 2L)
    expect_s3_class(cl, "cluster_bouquet")
  }
})

test_that("cluster_bouquet normalise = TRUE is stored in bq_meta", {
  cl <- cluster_bouquet(gw, week, station, level_m, normalise = TRUE)
  expect_true(isTRUE(attr(cl, "bq_meta")$normalise))
})

test_that("cluster_bouquet ceiling_pct and launch_deg are accepted", {
  cl <- cluster_bouquet(gw, week, station, level_m,
                        ceiling_pct = 0.6, launch_deg = 0)
  expect_s3_class(cl, "cluster_bouquet")
})

test_that("cluster_bouquet errors with fewer than 3 series", {
  gw2 <- gw[gw$station %in% c("S1", "S2"), ]
  expect_error(cluster_bouquet(gw2, week, station, level_m),
               "At least 3 series")
})


# -- summary.cluster_bouquet() -------------------------------------------------

test_that("summary.cluster_bouquet prints without error and returns invisibly", {
  cl  <- cluster_bouquet(gw, week, station, level_m)
  out <- capture.output(res <- summary(cl))
  expect_true(length(out) > 5L)
  expect_identical(res, cl)
})

test_that("summary output contains key fields", {
  cl  <- cluster_bouquet(gw, week, station, level_m)
  out <- paste(capture.output(summary(cl)), collapse = "\n")
  expect_match(out, "Method")
  expect_match(out, "silhouette")
  expect_match(out, "Cluster sizes")
})


# -- plot_cluster_quality() ----------------------------------------------------

test_that("plot_cluster_quality returns a ggplot invisibly", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  p  <- plot_cluster_quality(cl)
  expect_s3_class(p, "ggplot")
})

test_that("plot_cluster_quality errors on fixed-k result", {
  cl <- cluster_bouquet(gw, week, station, level_m, k = 2L)
  expect_error(plot_cluster_quality(cl), "k = \"auto\"")
})

test_that("plot_cluster_quality dark_mode works", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  expect_s3_class(plot_cluster_quality(cl, dark_mode = TRUE), "ggplot")
})


# -- save_bouquet() ------------------------------------------------------------

test_that("save_bouquet writes a PNG file", {
  p    <- make_plot_bouquet(gw, week, station, level_m)
  tmp  <- tempfile(fileext = ".png")
  path <- save_bouquet(p, tmp, dpi = 72L)
  expect_true(file.exists(tmp))
  expect_equal(path, tmp)
  unlink(tmp)
})

test_that("save_bouquet errors on bad arguments", {
  p <- make_plot_bouquet(gw, week, station, level_m)
  expect_error(save_bouquet(p, "",      dpi = 72L), "non-empty")
  expect_error(save_bouquet(p, "a.png", width = -1, dpi = 72L), "positive")
})


# -- make_plot_bouquet_interactive() -------------------------------------------

test_that("make_plot_bouquet_interactive requires plotly", {
  skip_if(requireNamespace("plotly", quietly = TRUE),
          "plotly is installed; skipping missing-package guard test")
  expect_error(
    make_plot_bouquet_interactive(gw, week, station, level_m),
    "plotly"
  )
})


# -- Snapshot: verbose output --------------------------------------------------

test_that("make_plot_bouquet verbose output snapshot", {
  withr::local_seed(42L)
  out <- capture.output(
    make_plot_bouquet(gw, week, station, level_m, verbose = TRUE)
  )
  expect_true(any(grepl("Heading sweep", out)))
  expect_true(any(grepl("Binding series", out)))
  expect_true(any(grepl("theta", out)))
})

test_that("cluster_bouquet verbose output contains expected fields", {
  out <- capture.output(
    cluster_bouquet(gw, week, station, level_m, verbose = TRUE)
  )
  expect_true(any(grepl("cluster_bouquet", out, ignore.case = TRUE)))
  expect_true(any(grepl("silhouette|Cluster sizes", out)))
})


# -- bouquet_plot S3 class -----------------------------------------------------

test_that("make_plot_bouquet returns a bouquet_plot object", {
  p <- make_plot_bouquet(gw, week, station, level_m)
  expect_s3_class(p, "bouquet_plot")
})

test_that("print.bouquet_plot emits a diagnostic line and renders", {
  p   <- make_plot_bouquet(gw, week, station, level_m)
  out <- capture.output(print(p))
  expect_true(any(grepl("bouquet_plot", out)))
  expect_true(any(grepl("series|theta", out)))
})

test_that("format.bouquet_plot returns a string", {
  p <- make_plot_bouquet(gw, week, station, level_m)
  expect_type(format(p), "character")
  expect_match(format(p), "bouquet_plot")
})


# -- Integration: cluster -> plot with render ----------------------------------

test_that("cluster then make_plot_bouquet renders without error", {
  cl <- cluster_bouquet(gw, week, station, level_m)
  p  <- make_plot_bouquet(cl,
    time_col      = week,
    series_col    = station,
    value_col     = level_m,
    stem_colors   = cluster,
    flower_colors = cluster
  )
  expect_no_error(print(p))
})

test_that("cluster normalise = TRUE is inherited by make_plot_bouquet", {
  cl <- cluster_bouquet(gw, week, station, level_m, normalise = TRUE)
  expect_message(
    p <- make_plot_bouquet(cl, week, station, level_m),
    "inheriting normalise"
  )
  expect_s3_class(p, "bouquet_plot")
})

test_that("explicit normalise mismatch with cluster_bouquet warns", {
  # cluster built with default normalise = FALSE; user passes TRUE explicitly
  # -> stored=FALSE, passed=TRUE -> neither is the default-inherit case -> warn
  cl <- cluster_bouquet(gw, week, station, level_m)
  expect_warning(
    make_plot_bouquet(cl, week, station, level_m, normalise = TRUE),
    "normalise"
  )
})

test_that("area_hclust clustering integrates with make_plot_bouquet", {
  cl <- cluster_bouquet(gw, week, station, level_m, method = "area_hclust")
  p  <- make_plot_bouquet(cl, week, station, level_m,
                          stem_colors   = cluster,
                          flower_colors = cluster)
  expect_s3_class(p, "bouquet_plot")
  expect_no_error(print(p))
})