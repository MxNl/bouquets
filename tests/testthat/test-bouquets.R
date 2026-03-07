# ── Fixtures ──────────────────────────────────────────────────────────────────
# Must be defined before any test_that() block that uses them.

set.seed(42)
n_obs  <- 20L
weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n_obs)
season <- sin(seq(0, 2 * pi, length.out = n_obs))

gw_long <- tibble::tibble(
  week    = rep(weeks, 3L),
  station = rep(c("A", "B", "C"), each = n_obs),
  region  = rep(c("North", "North", "South"), each = n_obs),
  level_m = c(
    8.5 + cumsum(rnorm(n_obs, 0.00, 0.3)),
    7.2 + cumsum(rnorm(n_obs, 0.02, 0.3)),
    9.1 + cumsum(rnorm(n_obs, -0.01, 0.3))
  )
)

# Minimal wrapper so tests stay DRY. verbose=FALSE suppresses console output.
# Do NOT pass verbose here — tests that check verbose-dependent behaviour call
# make_plot_bouquet() directly to avoid the "matched by multiple args" error.
bouquet <- function(...) {
  make_plot_bouquet(
    gw_long,
    time_col   = week,
    series_col = station,
    value_col  = level_m,
    verbose    = FALSE,
    ...
  )
}

# Helper: find layers by geom class name
layers_of <- function(p, geom_class) {
  Filter(function(l) inherits(l$geom, geom_class), p$layers)
}

# Helper: extract resolved stem colour values from a plot's colour scale
stem_cols <- function(p) {
  sc <- Filter(function(s) "colour" %in% s$aesthetics, p$scales$scales)[[1L]]
  unname(sc$palette(dplyr::n_distinct(p$data$series)))
}

# Fixture for cluster_bouquet tests — 6 series, 52 weekly steps
set.seed(7)
n_cl   <- 52L
wks_cl <- seq(as.Date("2023-01-01"), by = "week", length.out = n_cl)
sea_cl <- sin(seq(0, 2 * pi, length.out = n_cl))

gw6 <- tibble::tibble(
  week    = rep(wks_cl, 6L),
  station = rep(paste0("S", 1:6), each = n_cl),
  level_m = c(
    8.5 + 0.8 * sea_cl + cumsum(rnorm(n_cl,  0.00, 0.2)),
    8.3 + 0.7 * sea_cl + cumsum(rnorm(n_cl,  0.01, 0.2)),
    7.2 + 0.5 * sea_cl + cumsum(rnorm(n_cl,  0.02, 0.2)),
    7.0 + 0.6 * sea_cl + cumsum(rnorm(n_cl,  0.00, 0.2)),
    9.1 + 1.1 * sea_cl + cumsum(rnorm(n_cl, -0.01, 0.2)),
    9.3 + 1.0 * sea_cl + cumsum(rnorm(n_cl, -0.02, 0.2))
  )
)


# ══════════════════════════════════════════════════════════════════════════════
# make_plot_bouquet()
# ══════════════════════════════════════════════════════════════════════════════

# ── Return type ───────────────────────────────────────────────────────────────

test_that("returns a ggplot object", {
  expect_s3_class(bouquet(), "ggplot")
})

test_that("returned object is visible (not invisible)", {
  result <- withVisible(bouquet())
  expect_true(result$visible)
})


# ── Input validation ──────────────────────────────────────────────────────────

test_that("non-data-frame input is rejected", {
  expect_error(
    make_plot_bouquet(list(a = 1:3), verbose = FALSE),
    "`data` must be a data frame"
  )
})

test_that("non-numeric value column is rejected", {
  bad <- dplyr::mutate(gw_long, level_m = as.character(level_m))
  expect_error(
    make_plot_bouquet(bad,
      time_col = week, series_col = station, value_col = level_m,
      verbose = FALSE
    ),
    "must be numeric"
  )
})

test_that("ceiling_pct out of range is rejected", {
  expect_error(bouquet(ceiling_pct = 0),   "`ceiling_pct`")
  expect_error(bouquet(ceiling_pct = 1.1), "`ceiling_pct`")
  expect_error(bouquet(ceiling_pct = "a"), "`ceiling_pct`")
})

test_that("non-scalar launch_deg is rejected", {
  expect_error(bouquet(launch_deg = c(45, 90)), "`launch_deg`")
})

test_that("invalid marker_every values are rejected", {
  expect_error(bouquet(marker_every = 0),   "`marker_every`")
  expect_error(bouquet(marker_every = -1),  "`marker_every`")
  expect_error(bouquet(marker_every = "5"), "`marker_every`")
})

test_that("non-logical flags are rejected", {
  expect_error(
    make_plot_bouquet(gw_long, time_col = week, series_col = station,
                      value_col = level_m, show_rings = "yes"),
    "`show_rings`"
  )
  expect_error(
    make_plot_bouquet(gw_long, time_col = week, series_col = station,
                      value_col = level_m, dark_mode = 1L),
    "`dark_mode`"
  )
  expect_error(
    make_plot_bouquet(gw_long, time_col = week, series_col = station,
                      value_col = level_m, verbose = NULL),
    "`verbose`"
  )
})

test_that("invalid hide_legend_after is rejected", {
  expect_error(bouquet(hide_legend_after = 0),   "`hide_legend_after`")
  expect_error(bouquet(hide_legend_after = "a"),  "`hide_legend_after`")
})

test_that("non-existent colour column name is caught early", {
  expect_error(
    make_plot_bouquet(gw_long,
      time_col    = week,
      series_col  = station,
      value_col   = level_m,
      stem_colors = no_such_col,
      verbose     = FALSE
    ),
    "not found in data"
  )
})

test_that("invalid stem_colors type is rejected", {
  expect_error(bouquet(stem_colors = 123), "hex colour vector")
})

test_that("invalid flower_colors type is rejected", {
  expect_error(bouquet(flower_colors = TRUE), "hex colour vector")
})

test_that("supplying only lon_col without lat_col is rejected", {
  gw_coords <- dplyr::mutate(gw_long, lon = 10, lat = 51)
  expect_error(
    make_plot_bouquet(gw_coords,
      time_col = week, series_col = station, value_col = level_m,
      lon_col  = lon,
      verbose  = FALSE
    ),
    "lon_col.*lat_col|lat_col.*lon_col"
  )
})

test_that("invalid map_width is rejected", {
  gw_coords <- dplyr::mutate(gw_long, lon = 10, lat = 51)
  expect_error(
    make_plot_bouquet(gw_coords,
      time_col  = week, series_col = station, value_col = level_m,
      lon_col   = lon, lat_col = lat,
      map_width = 1.5,
      verbose   = FALSE
    ),
    "`map_width`"
  )
})


# ── Theta / no-loop guarantee ─────────────────────────────────────────────────

test_that("heading never sweeps a full 360 degrees for any series", {
  n    <- 30L
  mono <- tibble::tibble(t = seq_len(n), id = "only", v = cumsum(rep(1, n)))
  pd   <- make_plot_bouquet(mono,
    time_col = t, series_col = id, value_col = v, verbose = FALSE
  )$data
  expect_lt(max(pd$heading_deg) - min(pd$heading_deg), 360)
})

test_that("ceiling_pct controls theta proportionally", {
  h_range <- function(p) diff(range(p$data$heading_deg))
  expect_gt(h_range(bouquet(ceiling_pct = 0.80)),
            h_range(bouquet(ceiling_pct = 0.50)))
})


# ── Path geometry ─────────────────────────────────────────────────────────────

test_that("all series start at the origin", {
  pd      <- bouquet()$data
  origins <- pd[pd$step == 1L, c("x", "y")]
  expect_true(all(origins$x == 0))
  expect_true(all(origins$y == 0))
})

test_that("path_data has one row per step per series", {
  pd <- bouquet()$data
  expect_equal(nrow(pd), max(pd$step) * dplyr::n_distinct(pd$series))
})

test_that("all-constant series produces a straight vertical path", {
  flat <- tibble::tibble(t = 1:10, id = "flat", v = rep(5, 10))
  pd   <- make_plot_bouquet(flat,
    time_col = t, series_col = id, value_col = v, verbose = FALSE
  )$data
  expect_true(all(abs(pd$x) < 1e-10))
  expect_true(all(diff(pd$y) > 0))
})

test_that("all-increasing series turns left (positive heading change)", {
  up <- tibble::tibble(t = 1:5, id = "up", v = 1:5)
  pd <- make_plot_bouquet(up,
    time_col = t, series_col = id, value_col = v, verbose = FALSE
  )$data
  expect_true(all(diff(pd$heading_deg) > 0))
})

test_that("all-decreasing series turns right (negative heading change)", {
  dn <- tibble::tibble(t = 1:5, id = "dn", v = 5:1)
  pd <- make_plot_bouquet(dn,
    time_col = t, series_col = id, value_col = v, verbose = FALSE
  )$data
  expect_true(all(diff(pd$heading_deg) < 0))
})

test_that("step lengths are all exactly 1 (unit steps)", {
  strides <- bouquet()$data |>
    dplyr::group_by(series) |>
    dplyr::mutate(dist = sqrt((x - dplyr::lag(x))^2 + (y - dplyr::lag(y))^2)) |>
    dplyr::filter(!is.na(dist)) |>
    dplyr::pull(dist)
  expect_true(all(abs(strides - 1) < 1e-10))
})


# ── Series ordering ───────────────────────────────────────────────────────────

test_that("series order is alphabetical when series_col is character", {
  lvls <- as.character(unique(bouquet()$data$series))
  expect_equal(lvls, sort(lvls))
})

test_that("series order respects factor levels", {
  gw_f         <- gw_long
  gw_f$station <- factor(gw_f$station, levels = c("C", "A", "B"))
  pd           <- make_plot_bouquet(gw_f,
    time_col = week, series_col = station, value_col = level_m,
    verbose = FALSE
  )$data
  expect_equal(as.character(unique(pd$series)), c("C", "A", "B"))
})


# ── Colour modes ──────────────────────────────────────────────────────────────

test_that("single hex stem colour is applied to all series", {
  cols <- stem_cols(bouquet(stem_colors = "#aabbcc"))
  expect_true(all(toupper(cols) == "#AABBCC"))
})

test_that("'greens' keyword produces distinct colours", {
  cols <- stem_cols(bouquet(stem_colors = "greens"))
  expect_length(cols, 3L)
  expect_gt(length(unique(cols)), 1L)
})

test_that("column-reference stem colour maps levels to colours", {
  p <- make_plot_bouquet(gw_long,
    time_col    = week,
    series_col  = station,
    value_col   = level_m,
    stem_colors = region,
    verbose     = FALSE
  )
  cols <- stem_cols(p)
  # palette() returns colours positionally (alphabetical: A=1, B=2, C=3).
  # A and B share "North"; C is "South" — exactly 2 distinct colours.
  expect_equal(cols[[1L]], cols[[2L]])
  expect_false(cols[[1L]] == cols[[3L]])
})

test_that("recycling warning fires when too few stem colours supplied", {
  expect_warning(
    make_plot_bouquet(gw_long,
      time_col    = week,
      series_col  = station,
      value_col   = level_m,
      stem_colors = c("#aabbcc", "#ddeeff"),  # 2 colours for 3 series
      verbose     = FALSE
    ),
    "recycling"
  )
})

test_that("'blossom' keyword produces a geom_text flower layer", {
  p           <- bouquet(flower_colors = "blossom")
  text_layers <- layers_of(p, "GeomText")
  expect_length(text_layers, 1L)
})


# ── Optional features ─────────────────────────────────────────────────────────

test_that("marker_every adds a geom_point layer", {
  expect_gt(
    length(layers_of(bouquet(marker_every = 5L), "GeomPoint")),
    length(layers_of(bouquet(),                  "GeomPoint"))
  )
})

test_that("show_rings adds a geom_circle layer", {
  expect_gt(
    length(layers_of(bouquet(show_rings = TRUE),  "GeomCircle")),
    length(layers_of(bouquet(show_rings = FALSE), "GeomCircle"))
  )
})

test_that("facet_by adds a FacetWrap component", {
  p <- make_plot_bouquet(gw_long,
    time_col = week, series_col = station, value_col = level_m,
    facet_by = region, verbose = FALSE
  )
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("no facet_by produces FacetNull (single panel)", {
  expect_s3_class(bouquet()$facet, "FacetNull")
})

test_that("legend is present when n_series < hide_legend_after", {
  expect_equal(bouquet(hide_legend_after = 10L)$theme$legend.position, "bottom")
})

test_that("legend is suppressed when n_series >= hide_legend_after", {
  expect_equal(bouquet(hide_legend_after = 3L)$theme$legend.position, "none")
})

test_that("custom title and subtitle are passed through", {
  p <- bouquet(title = "My Title", subtitle = "My Subtitle")
  expect_equal(p$labels$title,    "My Title")
  expect_equal(p$labels$subtitle, "My Subtitle")
})

test_that("dark_mode flips the background colour", {
  expect_equal(bouquet(dark_mode = FALSE)$theme$plot.background$fill, "#f8f8f5")
  expect_equal(bouquet(dark_mode = TRUE)$theme$plot.background$fill,  "#1a1a2e")
})

test_that("launch_deg rotates the initial heading", {
  flat <- tibble::tibble(t = 1:5, id = "s", v = rep(1, 5))
  pd0  <- make_plot_bouquet(flat,
    time_col = t, series_col = id, value_col = v,
    launch_deg = 0, verbose = FALSE
  )$data
  pd90 <- make_plot_bouquet(flat,
    time_col = t, series_col = id, value_col = v,
    launch_deg = 90, verbose = FALSE
  )$data
  expect_true(all(diff(pd0$x)       > 0))
  expect_true(all(abs(diff(pd0$y))  < 1e-10))
  expect_true(all(diff(pd90$y)      > 0))
  expect_true(all(abs(diff(pd90$x)) < 1e-10))
})


# ── Map panel ─────────────────────────────────────────────────────────────────

test_that("lon_col + lat_col returns a patchwork object", {
  skip_if_not_installed("patchwork")
  skip_if_not_installed("maps")
  gw_coords <- dplyr::mutate(gw_long,
    lon = c(rep(9.9, n_obs), rep(13.4, n_obs), rep(8.7, n_obs)),
    lat = c(rep(51.5, n_obs), rep(52.5, n_obs), rep(50.1, n_obs))
  )
  result <- make_plot_bouquet(gw_coords,
    time_col   = week,
    series_col = station,
    value_col  = level_m,
    lon_col    = lon,
    lat_col    = lat,
    verbose    = FALSE
  )
  expect_s3_class(result, "patchwork")
})

test_that("without lon_col/lat_col result is plain ggplot not patchwork", {
  expect_s3_class(bouquet(), "ggplot")
  expect_false(inherits(bouquet(), "patchwork"))
})


# ── Helper functions ──────────────────────────────────────────────────────────

test_that(".format_interval labels daily data correctly", {
  tv <- seq(as.Date("2023-01-01"), by = "day", length.out = 10)
  expect_equal(.format_interval(tv), "daily")
})

test_that(".format_interval labels weekly data correctly", {
  tv <- seq(as.Date("2023-01-01"), by = "week", length.out = 10)
  expect_equal(.format_interval(tv), "weekly")
})

test_that(".format_interval labels monthly data correctly", {
  tv <- seq(as.Date("2023-01-01"), by = "month", length.out = 10)
  expect_equal(.format_interval(tv), "monthly")
})

test_that(".format_interval labels yearly data correctly", {
  tv <- seq(as.Date("2020-01-01"), by = "year", length.out = 5)
  expect_equal(.format_interval(tv), "yearly")
})

test_that(".format_interval handles numeric time vectors", {
  expect_match(.format_interval(1:10), "^interval")
})

test_that(".format_interval handles a single-element vector", {
  expect_equal(.format_interval(as.Date("2023-01-01")), "1 observation")
})

test_that(".format_endpoint formats dates as 'DD Mon YYYY'", {
  expect_equal(.format_endpoint(as.Date("2023-06-15")), "15 Jun 2023")
})

test_that(".format_endpoint converts numeric to character", {
  expect_equal(.format_endpoint(42), "42")
})


# ══════════════════════════════════════════════════════════════════════════════
# cluster_bouquet()
# ══════════════════════════════════════════════════════════════════════════════

# ── Return type ───────────────────────────────────────────────────────────────

test_that("returns a tibble with same rows as input", {
  result <- cluster_bouquet(gw6,
    time_col = week, series_col = station, value_col = level_m,
    verbose = FALSE
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(gw6))
})

test_that("adds a factor column named 'cluster' by default", {
  result <- cluster_bouquet(gw6,
    time_col = week, series_col = station, value_col = level_m,
    verbose = FALSE
  )
  expect_true("cluster" %in% names(result))
  expect_s3_class(result$cluster, "factor")
})

test_that("cluster_col argument renames the output column", {
  result <- cluster_bouquet(gw6,
    time_col    = week,
    series_col  = station,
    value_col   = level_m,
    cluster_col = "group",
    verbose     = FALSE
  )
  expect_true("group" %in% names(result))
  expect_false("cluster" %in% names(result))
})


# ── Input validation ──────────────────────────────────────────────────────────

test_that("non-data-frame input is rejected", {
  expect_error(
    cluster_bouquet(list(a = 1:3), verbose = FALSE),
    "`data` must be a data frame"
  )
})

test_that("fewer than 3 series is rejected", {
  too_few <- dplyr::filter(gw6, station %in% c("S1", "S2"))
  expect_error(
    cluster_bouquet(too_few,
      time_col = week, series_col = station, value_col = level_m,
      verbose = FALSE
    ),
    "At least 3 series"
  )
})

test_that("k out of range is rejected", {
  expect_error(
    cluster_bouquet(gw6,
      time_col = week, series_col = station, value_col = level_m,
      k = 1L, verbose = FALSE
    ),
    "`k`"
  )
  expect_error(
    cluster_bouquet(gw6,
      time_col = week, series_col = station, value_col = level_m,
      k = 100L, verbose = FALSE
    ),
    "`k`"
  )
})

test_that("invalid resolution is rejected", {
  expect_error(
    cluster_bouquet(gw6,
      time_col   = week, series_col = station, value_col = level_m,
      resolution = -1, verbose = FALSE
    ),
    "`resolution`"
  )
})

test_that("invalid pca_variance is rejected", {
  expect_error(
    cluster_bouquet(gw6,
      time_col     = week, series_col = station, value_col = level_m,
      pca_variance = 1.5, verbose = FALSE
    ),
    "`pca_variance`"
  )
})


# ── Cluster labels ────────────────────────────────────────────────────────────

test_that("cluster levels are C1, C2, ... in order", {
  result <- cluster_bouquet(gw6,
    time_col = week, series_col = station, value_col = level_m,
    k = 3L, verbose = FALSE
  )
  expect_equal(levels(result$cluster), c("C1", "C2", "C3"))
})

test_that("C1 is the largest cluster", {
  result <- cluster_bouquet(gw6,
    time_col = week, series_col = station, value_col = level_m,
    k = 2L, verbose = FALSE
  )
  sizes <- table(dplyr::distinct(result, station, cluster)$cluster)
  expect_gte(sizes[["C1"]], sizes[["C2"]])
})

test_that("fixed k produces exactly k clusters", {
  for (ki in 2:4) {
    result <- cluster_bouquet(gw6,
      time_col = week, series_col = station, value_col = level_m,
      k = ki, verbose = FALSE
    )
    n_cl <- dplyr::n_distinct(dplyr::distinct(result, station, cluster)$cluster)
    expect_equal(n_cl, ki)
  }
})


# ── Methods ───────────────────────────────────────────────────────────────────

test_that("all methods run without error for fixed k", {
  methods <- c("pca_hclust", "pca_kmeans", "hclust", "kmeans")
  for (m in methods) {
    expect_no_error(
      cluster_bouquet(gw6,
        time_col = week, series_col = station, value_col = level_m,
        k = 2L, method = m, verbose = FALSE
      )
    )
  }
})

test_that("pam method requires cluster package", {
  skip_if_not_installed("cluster")
  expect_no_error(
    cluster_bouquet(gw6,
      time_col = week, series_col = station, value_col = level_m,
      k = 2L, method = "pam", verbose = FALSE
    )
  )
})

test_that("distance = 'correlation' runs for hclust", {
  expect_no_error(
    cluster_bouquet(gw6,
      time_col   = week, series_col = station, value_col = level_m,
      k = 2L, method = "hclust", distance = "correlation", verbose = FALSE
    )
  )
})

test_that("distance argument on pca_hclust emits a warning", {
  expect_warning(
    cluster_bouquet(gw6,
      time_col   = week, series_col = station, value_col = level_m,
      k = 2L, method = "pca_hclust", distance = "correlation",
      verbose = FALSE
    ),
    "no effect"
  )
})


# ── Integration with make_plot_bouquet() ──────────────────────────────────────

test_that("cluster column works as stem_colors in make_plot_bouquet", {
  clustered <- cluster_bouquet(gw6,
    time_col = week, series_col = station, value_col = level_m,
    k = 2L, verbose = FALSE
  )
  expect_no_error(
    make_plot_bouquet(clustered,
      time_col      = week,
      series_col    = station,
      value_col     = level_m,
      stem_colors   = cluster,
      flower_colors = cluster,
      verbose       = FALSE
    )
  )
})

test_that("cluster column works as facet_by in make_plot_bouquet", {
  clustered <- cluster_bouquet(gw6,
    time_col = week, series_col = station, value_col = level_m,
    k = 2L, verbose = FALSE
  )
  p <- make_plot_bouquet(clustered,
    time_col   = week,
    series_col = station,
    value_col  = level_m,
    facet_by   = cluster,
    verbose    = FALSE
  )
  expect_s3_class(p$facet, "FacetWrap")
})