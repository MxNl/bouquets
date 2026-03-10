# -- Unexported helpers --------------------------------------------------------

#' Format a time vector's typical interval as a human-readable string
#' @noRd
.format_interval <- function(tv) {
  if (length(tv) < 2L) return("1 observation")
  med_diff <- median(as.numeric(diff(sort(unique(tv)))), na.rm = TRUE)
  if (!inherits(tv, c("Date", "POSIXct", "POSIXlt")))
    return(paste0("interval ", round(med_diff, 2L)))
  breaks <- c(daily = 1.6, weekly = 9, fortnightly = 22,
              monthly = 60, quarterly = 180, yearly = 500)
  idx <- findInterval(med_diff, breaks, rightmost.closed = FALSE)
  if (idx < length(breaks)) names(breaks)[idx + 1L]
  else paste0("every ", round(med_diff), " days")
}

#' Format a single time value as a display string
#' @noRd
.format_endpoint <- function(tv) {
  if (inherits(tv, c("Date", "POSIXct", "POSIXlt"))) format(tv, "%d %b %Y")
  else as.character(tv)
}

#' Resolve a colour quosure to a named character vector of hex codes
#' @noRd
.resolve_colors <- function(q, data, path_data, series_names,
                             keyword, palette, dark_mode, arg_name) {
  n_series <- length(series_names)

  if (rlang::quo_is_symbol(q) && rlang::as_name(q) %in% names(path_data)) {
    col_name <- rlang::as_name(q)
    lvls     <- unique(as.character(path_data[[col_name]]))
    n_lvls   <- length(lvls)
    lvl_cols <- stats::setNames(
      hues::iwanthue(n_lvls,
                     lmin = if (dark_mode) 50 else 30,
                     lmax = if (dark_mode) 85 else 70),
      lvls
    )
    series_lvl <- path_data |>
      dplyr::group_by(.data$series) |>
      dplyr::slice_head(n = 1L) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(c("series", col_name)))
    colours <- stats::setNames(
      unname(lvl_cols[as.character(series_lvl[[col_name]])]),
      series_lvl$series
    )[series_names]

  } else {
    value <- rlang::eval_tidy(q)
    if (!is.character(value))
      stop(sprintf("`%s` must be a hex colour vector, \"%s\", or a column name.",
                   arg_name, keyword), call. = FALSE)
    raw <- if (identical(value, keyword)) {
      rep_len(palette, n_series)
    } else {
      if (length(value) > 1L && length(value) < n_series)
        warning(sprintf("Fewer `%s` supplied than series; recycling.", arg_name),
                call. = FALSE)
      rep_len(value, n_series)
    }
    colours <- stats::setNames(unname(as.character(raw)), series_names)
  }
  colours
}


# -- Path-building core ---------------------------------------------------------

#' Build bouquet turtle-paths from a long-format data frame
#'
#' Shared core used by [make_plot_bouquet()].
#' @noRd
.build_bouquet_paths <- function(data, time_col_name, series_col_name,
                                  value_col_name, ceiling_pct,
                                  launch_deg, verbose,
                                  normalise = FALSE) {

  df_long <- data |>
    dplyr::rename(
      bq_t = dplyr::all_of(time_col_name),
      bq_s = dplyr::all_of(series_col_name),
      bq_v = dplyr::all_of(value_col_name)
    ) |>
    dplyr::group_by(.data$bq_s) |>
    dplyr::mutate(
      step      = dplyr::row_number(),
      delta     = .data$bq_v - dplyr::lag(.data$bq_v),
      direction = dplyr::case_when(
        is.na(.data$delta) ~ "start",
        .data$delta  > 0   ~ "increase",
        .data$delta  < 0   ~ "decrease",
        TRUE               ~ "no_change"
      )
    ) |>
    dplyr::ungroup()

  raw_series   <- df_long$bq_s
  series_names <- if (is.factor(raw_series)) levels(raw_series)
                  else sort(unique(as.character(raw_series)))
  n_series <- length(series_names)

  sweep_tbl <- df_long |>
    dplyr::filter(.data$direction != "start") |>
    dplyr::group_by(.data$bq_s) |>
    dplyr::mutate(
      signed_step = dplyr::case_when(
        .data$direction == "increase" ~  1L,
        .data$direction == "decrease" ~ -1L,
        TRUE                          ~  0L
      ),
      cum_net = cumsum(.data$signed_step)
    ) |>
    dplyr::summarise(
      cum_net_range = max(.data$cum_net) - min(.data$cum_net),
      .groups = "drop"
    )

  max_sweep      <- max(sweep_tbl$cum_net_range)
  binding_series <- sweep_tbl$bq_s[which.max(sweep_tbl$cum_net_range)]
  theta_ceiling  <- 360 / max_sweep
  theta_deg      <- theta_ceiling * ceiling_pct
  theta_rad      <- theta_deg * pi / 180
  launch_rad     <- launch_deg * pi / 180

  if (verbose) {
    cat("=== Heading sweep per series ===\n")
    print(sweep_tbl)
    cat(sprintf("\nBinding series : %s (sweep = %d steps)\n",
                binding_series, max_sweep))
    cat(sprintf("Ceiling angle  : %.2f deg\n", theta_ceiling))
    cat(sprintf("theta (%.0f%%)  : %.2f deg%s\n\n",
                ceiling_pct * 100, theta_deg,
                if (normalise) " [overridden per series in normalise mode]" else ""))
  }

  # build_one has access to normalise, theta_rad, ceiling_pct via closure
  build_one <- function(df) {
    if (normalise) {
      steps_seq  <- df$direction[df$direction != "start"]
      s_signed   <- ifelse(steps_seq == "increase", 1L,
                           ifelse(steps_seq == "decrease", -1L, 0L))
      s_cum      <- cumsum(s_signed)
      s_sweep    <- max(s_cum) - min(s_cum)
      if (s_sweep < 1L) s_sweep <- 1L
      use_rad    <- (360 / s_sweep) * ceiling_pct * pi / 180
    } else {
      use_rad <- theta_rad
    }
    turns     <- dplyr::case_when(
      df$direction == "increase" ~  use_rad,
      df$direction == "decrease" ~ -use_rad,
      TRUE                       ~  0
    )
    turns[1L] <- 0
    heading   <- launch_rad + cumsum(turns)
    step_cos  <- cos(heading[-1L])
    step_sin  <- sin(heading[-1L])
    dplyr::mutate(df,
      x           = c(0, cumsum(step_cos)),
      y           = c(0, cumsum(step_sin)),
      heading_deg = heading * 180 / pi
    )
  }

  path_data <- df_long |>
    dplyr::group_by(.data$bq_s) |>
    dplyr::group_modify(~ build_one(.x)) |>
    dplyr::ungroup() |>
    dplyr::rename(series = "bq_s")

  list(
    path_data      = path_data,
    series_names   = series_names,
    n_series       = n_series,
    theta_deg      = theta_deg,
    theta_rad      = theta_rad,
    binding_series = binding_series,
    max_sweep      = max_sweep
  )
}


# -- Location map helper --------------------------------------------------------

#' Build a location map panel for make_plot_bouquet()
#'
#' @param coord_tbl   Data frame: series, lon, lat, flower_col, optionally
#'   hull_col and hull_fill_col.
#' @param bg_col      Background hex colour.
#' @param subtext_col Muted text colour.
#' @param dark_mode   Logical.
#' @param coord_crs   Integer EPSG code or NULL.
#' @param hull_col    String column name for cluster hulls, or NULL.
#' @param hull_coverage Fraction of points (closest to cluster centroid) used
#'   for the hull. 1 = all points.
#' @param blend       Logical. Apply ggblend blending modes to hulls and points.
#' @noRd
.build_location_map <- function(coord_tbl, bg_col, subtext_col, dark_mode,
                                coord_crs = NULL, hull_col = NULL,
                                hull_coverage = 1.0, blend = TRUE) {

  if (!requireNamespace("maps", quietly = TRUE))
    stop('The location map feature requires the maps package.\n',
         'Install it with: install.packages("maps")', call. = FALSE)

  border_col  <- if (dark_mode) "grey50" else "grey60"
  outline_col <- if (dark_mode) "grey20" else "grey85"
  point_size  <- 3.5

  # -- Reproject if needed -----------------------------------------------------
  needs_reproject <- !is.null(coord_crs) ||
    any(abs(coord_tbl$lon) > 180 | abs(coord_tbl$lat) > 90)

  if (needs_reproject) {
    if (!requireNamespace("sf", quietly = TRUE))
      stop('Reprojecting coordinates requires the sf package.\n',
           'Install it with: install.packages("sf")', call. = FALSE)
    epsg <- if (!is.null(coord_crs)) {
      as.integer(coord_crs)
    } else {
      lon1 <- coord_tbl$lon[1]; lat1 <- coord_tbl$lat[1]
      dplyr::case_when(
        lon1 > 4e6 & lon1 < 5e6 & lat1 < lon1 ~ 3035L,
        lon1 > 2e6 & lon1 < 3e6               ~ 31466L,
        lon1 > 3e6 & lon1 < 4e6               ~ 31467L,
        lon1 > 4e6 & lon1 < 5e6               ~ 31468L,
        lon1 > 5e6 & lon1 < 6e6               ~ 31469L,
        lon1 > 2e5 & lon1 < 9e5               ~ 25832L,
        TRUE                                  ~ 25833L
      )
    }
    if (verbose_epsg <- getOption("bouquets.verbose_crs", default = TRUE))
      message(sprintf(
        "bouquets: reprojecting coordinates from EPSG:%d to WGS84. ",
        epsg), "Override with coord_crs if detection is wrong.")
    pts     <- sf::st_as_sf(coord_tbl, coords = c("lon", "lat"), crs = epsg)
    pts_wgs <- sf::st_transform(pts, crs = 4326L)
    wgs     <- as.data.frame(sf::st_coordinates(pts_wgs))
    coord_tbl$lon <- wgs$X
    coord_tbl$lat <- wgs$Y
  }

  # -- Bounding box -------------------------------------------------------------
  lon_range <- range(coord_tbl$lon, na.rm = TRUE)
  lat_range <- range(coord_tbl$lat, na.rm = TRUE)
  lon_pad   <- max(1.0, diff(lon_range) * 0.3)
  lat_pad   <- max(1.0, diff(lat_range) * 0.3)
  xlim      <- c(lon_range[1] - lon_pad, lon_range[2] + lon_pad)
  ylim      <- c(lat_range[1] - lat_pad, lat_range[2] + lat_pad)
  mean_lat  <- mean(ylim)
  asp_ratio <- 1 / cos(mean_lat * pi / 180)

  # -- Background map polygons --------------------------------------------------
  db <- if (requireNamespace("mapdata", quietly = TRUE)) "worldHires" else "world"
  bg_map <- tryCatch(
    ggplot2::map_data(db, xlim = xlim, ylim = ylim),
    error = function(e) tryCatch(
      ggplot2::map_data("world", xlim = xlim, ylim = ylim),
      error = function(e2) NULL
    )
  )

  # -- Base plot ----------------------------------------------------------------
  p <- ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = asp_ratio, xlim = xlim, ylim = ylim,
                         expand = FALSE)

  if (!is.null(bg_map) && nrow(bg_map) > 0L)
    p <- p + ggplot2::geom_polygon(
      data    = bg_map,
      mapping = ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      fill = bg_col, colour = border_col, linewidth = 0.3
    )

  # -- Check ggblend availability -----------------------------------------------
  use_blend <- blend && requireNamespace("ggblend", quietly = TRUE)
  if (blend && !use_blend)
    message("bouquets: install ggblend for blended hull/point rendering ",
            "(install.packages('ggblend')).")

  # -- Optional cluster hulls (ggforce::geom_mark_hull) -------------------------
  if (!is.null(hull_col) && hull_col %in% names(coord_tbl)) {

    hull_data <- coord_tbl

    # Trim to core fraction if hull_coverage < 1
    if (hull_coverage < 1.0) {
      hull_data <- do.call(rbind, lapply(
        split(hull_data, hull_data[[hull_col]]),
        function(cl) {
          if (nrow(cl) <= 2L) return(cl)
          cx  <- mean(cl$lon); cy <- mean(cl$lat)
          d2  <- (cl$lon - cx)^2 + (cl$lat - cy)^2
          keep <- order(d2)[seq_len(max(2L, ceiling(nrow(cl) * hull_coverage)))]
          cl[keep, ]
        }
      ))
    }

    # hull_fill_col carries the per-cluster hex colour (built in main function)
    has_fill_col <- "hull_fill_col" %in% names(hull_data)
    hull_fill_aes <- if (has_fill_col) {
      ggplot2::aes(x = .data$lon, y = .data$lat,
                   group = .data[[hull_col]], fill = I(.data$hull_fill_col))
    } else {
      ggplot2::aes(x = .data$lon, y = .data$lat,
                   group = .data[[hull_col]], fill = .data[[hull_col]])
    }
    hull_layer <- ggforce::geom_mark_hull(
      data        = hull_data,
      mapping     = hull_fill_aes,
      alpha       = 0.15,
      colour      = NA,
      expand      = ggplot2::unit(4, "mm"),
      radius      = ggplot2::unit(4, "mm"),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
    p <- p + if (use_blend) ggblend::blend(hull_layer, "lighten") else hull_layer
  }

  # -- Points -------------------------------------------------------------------
  outline_layer <- ggplot2::geom_point(
    data        = coord_tbl,
    mapping     = ggplot2::aes(x = .data$lon, y = .data$lat),
    color       = outline_col, size = point_size + 1.2, inherit.aes = FALSE
  )
  color_layer <- ggplot2::geom_point(
    data        = coord_tbl,
    mapping     = ggplot2::aes(x = .data$lon, y = .data$lat,
                               color = .data$flower_col),
    size = point_size, inherit.aes = FALSE
  )

  if (use_blend) {
    p <- p +
      ggblend::blend(outline_layer, "multiply") +
      ggblend::blend(color_layer,   "multiply") +
      ggplot2::scale_color_identity()
  } else {
    p <- p + outline_layer + color_layer + ggplot2::scale_color_identity()
  }

  p + ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = bg_col, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_col, color = NA),
      plot.margin      = ggplot2::margin(30, 20, 20, 5)
    )
}


# -- Main exported function -----------------------------------------------------

#' Angular Accumulation (Flower Bouquet) Plot for Time Series
#'
#' @description
#' Draws each time series as a turtle-graphics path from a shared origin,
#' encoding the binary direction of consecutive changes as fixed-angle turns:
#' an increase turns the heading **left** by \eqn{\theta} and a decrease turns
#' it **right** by \eqn{\theta}. No change continues straight ahead. Because
#' all series share the same origin and angle, the visual similarity of paths
#' directly reflects the similarity of fluctuation patterns across series.
#'
#' @param data A long-format data frame or tibble with at minimum a time
#'   column, a series identifier column, and a numeric value column. Additional
#'   factor or character columns may be referenced by `stem_colors`,
#'   `flower_colors`, `facet_by`, and `cluster_hull`.
#' @param time_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The time index
#'   column. Defaults to the first column.
#' @param series_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   identifying each individual time series. Defaults to the second column.
#' @param value_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The numeric
#'   value column. Defaults to the third column.
#' @param stem_colors Controls the colour of the time series lines. Four
#'   options are accepted:
#'   \itemize{
#'     \item A single hex string -- all lines share that colour. Default:
#'       `"#3a7d2c"` (plant green).
#'     \item A character vector of hex strings, one per series -- recycled
#'       with a warning if too short.
#'     \item `"greens"` -- distinct shades from a curated palette of natural
#'       greens.
#'     \item A bare column name in `data` -- each series is coloured by that
#'       column's value using perceptually uniform colours.
#'   }
#' @param flower_colors Controls the colour of the end-of-series flower
#'   symbols. Same four options as `stem_colors`. The keyword is `"blossom"`.
#'   Default: `"#f472b6"` (warm pink).
#' @param facet_by <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional bare
#'   column name. When supplied the plot is split via [ggplot2::facet_wrap()].
#'   `NULL` (default) = single panel.
#' @param ceiling_pct A single number in `(0, 1]`. Fraction of the theoretical
#'   maximum angle to use as \eqn{\theta}. Default `0.80`.
#' @param launch_deg Initial heading in degrees (counter-clockwise from
#'   positive x-axis). `90` = straight up. Defaults to `90`.
#' @param marker_every Positive integer. A dot is plotted at every
#'   `marker_every`-th step. `NULL` (default) = no markers.
#' @param show_rings Logical. Draw faint concentric reference rings. Default
#'   `FALSE`.
#' @param dark_mode Logical. `TRUE` = dark navy background. Default `FALSE`.
#' @param title Plot title string. `NULL` (default) = no title.
#' @param subtitle Subtitle string. `NULL` (default) = auto-generated summary
#'   of theta, binding series, time range, and interval.
#' @param hide_legend_after Positive integer. Legend is suppressed when
#'   `n_series >= hide_legend_after`. Default `10`.
#' @param lon_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional bare
#'   column name for longitude. Must be paired with `lat_col`. When both are
#'   provided a location map is attached to the right via patchwork.
#' @param lat_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional bare
#'   column name for latitude. See `lon_col`.
#' @param map_width Relative width of the map panel as a fraction of the
#'   combined figure width. Only used when coordinates are supplied. Default
#'   `0.35`.
#' @param coord_crs Integer EPSG code or `NULL`. CRS of `lon_col`/`lat_col`.
#'   `NULL` = auto-detect from coordinate magnitude. Decimal-degree WGS84
#'   coordinates require no reprojection.
#' @param cluster_hull <[`tidy-select`][dplyr::dplyr_tidy_select]> Optional
#'   bare column name containing cluster labels (e.g. the `cluster` column
#'   produced by [cluster_bouquet()]). When supplied **and** a location map is
#'   shown (via `lon_col`/`lat_col`), a smooth concave hull is drawn around
#'   each cluster's map points using [ggforce::geom_mark_hull()] from the
#'   \pkg{ggforce} package. Hull fill colour matches the cluster's
#'   `flower_colors`. `NULL` (default) = no hulls.
#' @param hull_coverage A single number in `(0, 1]`. Fraction of each
#'   cluster's points (those closest to the cluster centroid) included when
#'   computing the hull. Values below `1` exclude outlier points so the hull
#'   focuses on the cluster's core area, which is useful when many clusters
#'   overlap. Default `1` = all points.
#' @param blend Logical. When `TRUE` (default) and the \pkg{ggblend} package is
#'   installed, cluster hull fills are composited with the
#'   `"lighten"` blend mode and map points with `"multiply"`, giving perceptually
#'   richer colour mixing where hulls and points overlap. Falls back silently to
#'   normal alpha blending when \pkg{ggblend} is not available. Only affects
#'   the map panel; has no effect when no location map is shown.
#' @param highlight A character vector of series names to emphasise. All other
#'   series are drawn in a pale grey at low opacity. `NULL` (default) = all
#'   series equally prominent.
#' @param from Optional scalar of the same class as `time_col`. Observations
#'   before this value are dropped before computing paths. `NULL` = no filter.
#' @param to Optional scalar of the same class as `time_col`. Observations
#'   after this value are dropped. `NULL` = no filter.
#' @param normalise Logical. When `TRUE`, each series is assigned its own
#'   per-series \eqn{\theta} so that every series uses the full angular range
#'   independently of how volatile it is. This makes shape comparison easier
#'   when series have very different volatility. When `FALSE` (default) all
#'   series share a single global \eqn{\theta} derived from the most volatile
#'   series.
#' @param verbose Logical. Print angle diagnostics to the console. Default
#'   `FALSE`.
#'
#' @return A `bouquet_plot` object. When `lon_col` and `lat_col` are `NULL`,
#'   the underlying object is a [ggplot2::ggplot]. When coordinates are
#'   supplied, it is a [patchwork][patchwork::patchwork-package] object (bouquet
#'   left, map right). Both are wrapped in the `bouquet_plot` S3 class which
#'   provides an informative `print()` header and a `format()` method. All
#'   operations that work on ggplot/patchwork (e.g. `ggsave()`, `+`) continue
#'   to work.
#'
#' @details
#' ## Input format
#' `data` must be in **long** format: one row per time step per series. Series
#' should share the same time index values and have equal length.
#'
#' ## Algorithm
#' For each series, the signed first difference is binarised to \eqn{+1}
#' (increase), \eqn{-1} (decrease), or \eqn{0} (no change). The heading at
#' step \eqn{i} accumulates via [base::cumsum()]; coordinates follow as
#' \eqn{x_i = \sum \cos(h_k)}, \eqn{y_i = \sum \sin(h_k)}.
#'
#' ## Angle derivation
#' \deqn{\theta = \frac{360\degree}{\max_s(\max C_s - \min C_s)} \times
#'   \texttt{ceiling\_pct}}
#'
#' @seealso [cluster_bouquet()] to group series by directional similarity
#'   before plotting. [save_bouquet()] for convenient export.
#'   [make_plot_bouquet_interactive()] for a plotly hover tooltip version.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' n      <- 52
#' weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
#' season <- sin(seq(0, 2 * pi, length.out = n))
#'
#' gw_long <- tibble::tibble(
#'   week    = rep(weeks, 3),
#'   station = rep(c("Station A", "Station B", "Station C"), each = n),
#'   region  = rep(c("North", "North", "South"), each = n),
#'   level_m = c(
#'     8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.18)),
#'     7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.22)),
#'     9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.15))
#'   )
#' )
#'
#' # Minimal call
#' make_plot_bouquet(gw_long,
#'   time_col = week, series_col = station, value_col = level_m)
#'
#' # Highlight one station; others are dimmed
#' make_plot_bouquet(gw_long,
#'   time_col   = week,
#'   series_col = station,
#'   value_col  = level_m,
#'   highlight  = "Station A")
#'
#' # Normalised mode: each series uses the full angular range
#' make_plot_bouquet(gw_long,
#'   time_col   = week,
#'   series_col = station,
#'   value_col  = level_m,
#'   normalise  = TRUE)
#' }
#'
#' @export
#' @importFrom dplyr all_of case_when filter group_by group_modify mutate
#'   pull rename row_number select slice_head slice_tail summarise ungroup
#'   .data
#' @importFrom rlang as_name enquo eval_tidy quo_is_null quo_is_symbol
#'   quo_is_symbolic
#' @importFrom ggplot2 aes annotate coord_equal element_blank element_rect
#'   element_text facet_wrap geom_path geom_point geom_text ggplot labs
#'   margin scale_alpha_identity scale_color_identity scale_color_manual
#'   scale_fill_manual theme theme_void unit vars
#' @importFrom ggforce geom_circle geom_mark_hull
#' @importFrom hues iwanthue
#' @importFrom stats median setNames
#' @importFrom tibble tibble
make_plot_bouquet <- function(
    data,
    time_col          = 1,
    series_col        = 2,
    value_col         = 3,
    stem_colors       = "#3a7d2c",
    flower_colors     = "#f472b6",
    facet_by          = NULL,
    ceiling_pct       = 0.80,
    launch_deg        = 90,
    marker_every      = NULL,
    show_rings        = FALSE,
    dark_mode         = FALSE,
    title             = NULL,
    subtitle          = NULL,
    hide_legend_after = 10L,
    lon_col           = NULL,
    lat_col           = NULL,
    map_width         = 0.35,
    coord_crs         = NULL,
    cluster_hull      = NULL,
    hull_coverage     = 1.0,
    blend             = TRUE,
    highlight         = NULL,
    from              = NULL,
    to                = NULL,
    normalise         = FALSE,
    verbose           = FALSE) {

  # -- Capture quosures ---------------------------------------------------------
  stem_colors_q    <- rlang::enquo(stem_colors)
  flower_colors_q  <- rlang::enquo(flower_colors)
  facet_by_q       <- rlang::enquo(facet_by)
  lon_col_q        <- rlang::enquo(lon_col)
  lat_col_q        <- rlang::enquo(lat_col)
  cluster_hull_q   <- rlang::enquo(cluster_hull)

  # -- Validate scalar inputs ---------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  if (!is.numeric(ceiling_pct) || length(ceiling_pct) != 1L ||
      ceiling_pct <= 0 || ceiling_pct > 1)
    stop("`ceiling_pct` must be a single number in (0, 1].", call. = FALSE)
  if (!is.numeric(launch_deg) || length(launch_deg) != 1L)
    stop("`launch_deg` must be a single number.", call. = FALSE)
  if (!is.null(marker_every)) {
    if (!is.numeric(marker_every) || length(marker_every) != 1L || marker_every < 1L)
      stop("`marker_every` must be a single positive integer or NULL.", call. = FALSE)
    marker_every <- as.integer(marker_every)
  }
  if (!is.logical(show_rings) || length(show_rings) != 1L)
    stop("`show_rings` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(dark_mode) || length(dark_mode) != 1L)
    stop("`dark_mode` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(verbose) || length(verbose) != 1L)
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  if (!is.logical(normalise) || length(normalise) != 1L)
    stop("`normalise` must be TRUE or FALSE.", call. = FALSE)
  if (!is.numeric(hide_legend_after) || length(hide_legend_after) != 1L ||
      hide_legend_after < 1L)
    stop("`hide_legend_after` must be a single positive integer.", call. = FALSE)
  hide_legend_after <- as.integer(hide_legend_after)

  if (!is.numeric(hull_coverage) || length(hull_coverage) != 1L ||
      hull_coverage <= 0 || hull_coverage > 1)
    stop("`hull_coverage` must be a single number in (0, 1].", call. = FALSE)

  # -- Map feature --------------------------------------------------------------
  has_lon <- !rlang::quo_is_null(lon_col_q)
  has_lat <- !rlang::quo_is_null(lat_col_q)
  if (has_lon != has_lat)
    stop("Both `lon_col` and `lat_col` must be supplied together, or both omitted.",
         call. = FALSE)
  has_map <- has_lon && has_lat

  if (has_map) {
    if (!is.numeric(map_width) || length(map_width) != 1L ||
        map_width <= 0 || map_width >= 1)
      stop("`map_width` must be a single number in (0, 1).", call. = FALSE)
    if (!requireNamespace("patchwork", quietly = TRUE))
      stop('The location map feature requires the patchwork package.\n',
           'Install it with: install.packages("patchwork")', call. = FALSE)
    lon_col_name <- names(dplyr::select(data, !!lon_col_q))[1L]
    lat_col_name <- names(dplyr::select(data, !!lat_col_q))[1L]
    if (!is.numeric(data[[lon_col_name]]))
      stop(sprintf("Column '%s' (lon_col) must be numeric.", lon_col_name), call. = FALSE)
    if (!is.numeric(data[[lat_col_name]]))
      stop(sprintf("Column '%s' (lat_col) must be numeric.", lat_col_name), call. = FALSE)
  } else {
    lon_col_name <- NULL; lat_col_name <- NULL
  }

  # -- Resolve column names -----------------------------------------------------
  time_col_name   <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_col_name <- names(dplyr::select(data, {{ series_col }}))[1L]
  value_col_name  <- names(dplyr::select(data, {{ value_col }}))[1L]

  if (!is.numeric(data[[value_col_name]]))
    stop(sprintf("Column '%s' must be numeric.", value_col_name), call. = FALSE)

  facet_col_name <- if (rlang::quo_is_null(facet_by_q)) NULL
                    else names(dplyr::select(data, {{ facet_by }}))[1L]

  hull_col_name <- if (rlang::quo_is_null(cluster_hull_q)) NULL
                   else names(dplyr::select(data, {{ cluster_hull }}))[1L]

  # -- Validate annotation columns ----------------------------------------------
  colour_sym_name <- function(q) if (rlang::quo_is_symbol(q)) rlang::as_name(q) else NULL
  candidate_cols  <- unique(c(colour_sym_name(stem_colors_q),
                               colour_sym_name(flower_colors_q),
                               facet_col_name, hull_col_name))
  missing_cols <- candidate_cols[!candidate_cols %in% names(data)]
  if (length(missing_cols) > 0L)
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  # -- Time window filter -------------------------------------------------------
  if (!is.null(from))
    data <- data[data[[time_col_name]] >= from, , drop = FALSE]
  if (!is.null(to))
    data <- data[data[[time_col_name]] <= to, , drop = FALSE]
  if (nrow(data) == 0L)
    stop("No data remains after applying `from`/`to` filters.", call. = FALSE)

  # -- Build turtle paths -------------------------------------------------------
  bq             <- .build_bouquet_paths(data, time_col_name, series_col_name,
                                          value_col_name, ceiling_pct,
                                          launch_deg, verbose, normalise)
  path_data      <- bq$path_data
  series_names   <- bq$series_names
  n_series       <- bq$n_series
  theta_deg      <- bq$theta_deg
  binding_series <- bq$binding_series
  max_sweep      <- bq$max_sweep

  # -- Resolve colours ----------------------------------------------------------
  greens_pal  <- c("#1a5c2a","#2d8a45","#3a7d2c","#52a85e","#74c476",
                   "#98d494","#a8d5a2","#4e9e57","#236b30","#6abf69")
  blossom_pal <- c("#f472b6","#f9a8d4","#e879a0","#c084fc","#d8b4fe",
                   "#fb7185","#fda4af","#f0abfc","#e9a0c9","#ff8fab")

  stem_colors_resolved <- .resolve_colors(
    q = stem_colors_q, data = data, path_data = path_data,
    series_names = series_names, keyword = "greens", palette = greens_pal,
    dark_mode = dark_mode, arg_name = "stem_colors")

  flower_colors_resolved <- .resolve_colors(
    q = flower_colors_q, data = data, path_data = path_data,
    series_names = series_names, keyword = "blossom", palette = blossom_pal,
    dark_mode = dark_mode, arg_name = "flower_colors")

  # -- Highlight: dim non-highlighted series ------------------------------------
  if (!is.null(highlight)) {
    unknown_hl <- setdiff(as.character(highlight), series_names)
    if (length(unknown_hl) > 0L)
      warning("Series not found for highlighting: ",
              paste(unknown_hl, collapse = ", "), call. = FALSE)
    dim_col <- if (dark_mode) "#2e2e3a" else "#d8d8d8"
    not_hl  <- setdiff(series_names, as.character(highlight))
    stem_colors_resolved[not_hl]   <- dim_col
    flower_colors_resolved[not_hl] <- dim_col
  }

  # -- Alpha per series for highlight dimming -----------------------------------
  path_data <- path_data |>
    dplyr::mutate(
      .alpha = if (!is.null(highlight))
        ifelse(as.character(.data$series) %in% as.character(highlight), 0.92, 0.14)
      else 0.88
    )

  # -- Theme palette -------------------------------------------------------------
  bg_col       <- if (dark_mode) "#1a1a2e" else "#f8f8f5"
  text_col     <- if (dark_mode) "white"   else "#1a1a2e"
  subtext_col  <- if (dark_mode) "grey60"  else "grey40"
  ring_col     <- if (dark_mode) "white"   else "grey50"
  origin_outer <- if (dark_mode) "white"   else "#1a1a2e"
  origin_inner <- bg_col
  ring_alpha   <- if (dark_mode) 0.07      else 0.18

  # -- Auto-subtitle (pure ASCII for cross-platform safety) ---------------------
  n_steps   <- max(path_data$step)
  time_vals <- data[[time_col_name]]

  if (is.null(subtitle)) {
    norm_note <- if (normalise) " [normalised]" else
      paste0(" (binding: ", binding_series, ", sweep = ", max_sweep, " steps)")
    subtitle <- paste0(
      n_series, " series | ",
      n_steps, " ", .format_interval(time_vals), " observations | ",
      .format_endpoint(min(time_vals, na.rm = TRUE)), " to ",
      .format_endpoint(max(time_vals, na.rm = TRUE)), "\n",
      "Angular Accumulation Plot | theta = ", round(theta_deg, 1L), " deg",
      norm_note, "\n",
      "left turn = increase | right turn = decrease | straight = no change"
    )
  }

  # -- Ring radii ----------------------------------------------------------------
  extent     <- max(sqrt(path_data$x^2 + path_data$y^2), na.rm = TRUE)
  ring_step  <- max(5, round(extent / 25) * 5)
  ring_radii <- if (ring_step <= extent)
    seq(ring_step, floor(extent / ring_step) * ring_step, by = ring_step)
  else numeric(0)

  # -- Assemble plot -------------------------------------------------------------
  p <- ggplot2::ggplot(
    path_data,
    ggplot2::aes(x = .data$x, y = .data$y,
                 group = .data$series, color = .data$series)
  )

  if (show_rings && length(ring_radii) > 0L)
    p <- p + ggforce::geom_circle(
      data        = tibble::tibble(x0 = 0, y0 = 0, r = ring_radii),
      ggplot2::aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
      inherit.aes = FALSE, color = ring_col, alpha = ring_alpha, linewidth = 0.3
    )

  p <- p + ggplot2::geom_path(
    ggplot2::aes(alpha = .data$.alpha),
    linewidth = 1.1, lineend = "round", linejoin = "round"
  ) +
    ggplot2::scale_alpha_identity()

  if (!is.null(marker_every))
    p <- p + ggplot2::geom_point(
      data  = dplyr::filter(path_data, .data$step %% marker_every == 1L),
      ggplot2::aes(fill = .data$series),
      shape = 21L, size = 2.0, color = bg_col, stroke = 0.6
    )

  p <- p +
    ggplot2::annotate("point", x = 0, y = 0,
                      color = origin_outer, size = 5, alpha = 0.9) +
    ggplot2::annotate("point", x = 0, y = 0, color = origin_inner, size = 2.2)

  end_data <- path_data |>
    dplyr::group_by(.data$series) |>
    dplyr::slice_tail(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::mutate(flower_col = unname(flower_colors_resolved[.data$series]))

  p <- p + ggplot2::geom_text(
    data        = end_data,
    mapping     = ggplot2::aes(x = .data$x, y = .data$y,
                               label = "\u273f", color = I(.data$flower_col)),
    inherit.aes = FALSE, size = 5.5, family = "sans", show.legend = FALSE
  )

  p <- p +
    ggplot2::scale_color_manual(values = stem_colors_resolved) +
    ggplot2::coord_equal(clip = "off")

  if (!is.null(marker_every))
    p <- p + ggplot2::scale_fill_manual(values = stem_colors_resolved)

  if (!is.null(facet_col_name))
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_col_name]]))

  # -- Caption -------------------------------------------------------------------
  caption <- "flower = last step | o = origin"
  if (!is.null(marker_every))
    caption <- paste0(caption, " | dot = every ", marker_every, " steps")
  if (!is.null(highlight))
    caption <- paste0(caption, " | highlighted: ",
                      paste(highlight, collapse = ", "))

  # -- Theme ---------------------------------------------------------------------
  p_bouquet <- p +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = bg_col, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_col, color = NA),
      strip.background = ggplot2::element_rect(fill = bg_col, color = NA),
      strip.text       = ggplot2::element_text(color = subtext_col, size = 12,
                                               hjust = 0.5,
                                               margin = ggplot2::margin(b = 10)),
      legend.position  = if (n_series >= hide_legend_after) "none" else "bottom",
      legend.direction = "horizontal",
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(color = subtext_col, size = 13),
      legend.key.width  = ggplot2::unit(2,   "lines"),
      legend.key.height = ggplot2::unit(0.5, "lines"),
      legend.spacing.x  = ggplot2::unit(0.8, "lines"),
      plot.title        = ggplot2::element_text(
        color = text_col, size = 22, hjust = 0.5,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        color = subtext_col, size = 9, hjust = 0.5,
        margin = ggplot2::margin(b = 14)
      ),
      plot.caption  = ggplot2::element_text(
        color = subtext_col, size = 10, hjust = 0.5,
        margin = ggplot2::margin(t = 10)
      ),
      plot.margin = ggplot2::margin(30, 30, 20, 30)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  # -- Optional location map -----------------------------------------------------
  bq_meta <- list(
    n_series       = n_series,
    theta_deg      = theta_deg,
    binding_series = binding_series,
    max_sweep      = max_sweep,
    has_map        = has_map,
    normalise      = normalise
  )
  attr(p_bouquet, "bq_meta") <- bq_meta

  if (!has_map) {
    class(p_bouquet) <- c("bouquet_plot", class(p_bouquet))
    return(p_bouquet)
  }

  coord_tbl <- data |>
    dplyr::rename(bq_s = dplyr::all_of(series_col_name)) |>
    dplyr::group_by(.data$bq_s) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(series = "bq_s",
                  lon    = dplyr::all_of(lon_col_name),
                  lat    = dplyr::all_of(lat_col_name))

  coord_tbl$flower_col <- unname(
    flower_colors_resolved[as.character(coord_tbl$series)]
  )

  if (anyNA(coord_tbl$flower_col)) {
    warning("flower_colors could not be resolved for all map series; falling back.",
            call. = FALSE)
    coord_tbl$flower_col[is.na(coord_tbl$flower_col)] <- "#f472b6"
  }

  # Attach hull column if requested
  if (!is.null(hull_col_name)) {
    hull_lkp    <- data |>
      dplyr::rename(bq_s = dplyr::all_of(series_col_name)) |>
      dplyr::group_by(.data$bq_s) |>
      dplyr::slice_head(n = 1L) |>
      dplyr::ungroup() |>
      dplyr::select(series = "bq_s", dplyr::all_of(hull_col_name))
    coord_tbl <- dplyr::left_join(coord_tbl, hull_lkp, by = "series")

    # Derive one representative colour per cluster from flower_colors_resolved:
    # take the modal flower_col among the series in each cluster.
    cl_col_tbl <- coord_tbl |>
      dplyr::group_by(.data[[hull_col_name]]) |>
      dplyr::summarise(
        hull_fill_col = {
          tbl <- sort(table(.data$flower_col), decreasing = TRUE)
          names(tbl)[[1L]]
        },
        .groups = "drop"
      )
    coord_tbl <- dplyr::left_join(coord_tbl, cl_col_tbl, by = hull_col_name)
  }

  p_map <- .build_location_map(
    coord_tbl     = coord_tbl,
    bg_col        = bg_col,
    subtext_col   = subtext_col,
    dark_mode     = dark_mode,
    coord_crs     = coord_crs,
    hull_col      = hull_col_name,
    hull_coverage = hull_coverage,
    blend         = blend
  )

  bouquet_w <- 1 - map_width
  result    <- p_bouquet + p_map +
    patchwork::plot_layout(widths = c(bouquet_w, map_width))
  attr(result, "bq_meta") <- bq_meta
  class(result) <- c("bouquet_plot", class(result))
  result
}


# -- S3 methods for bouquet_plot ------------------------------------------------

#' Print a bouquet_plot object
#'
#' Renders the bouquet plot and prints a brief one-line diagnostic to the
#' console (series count, angle, and binding series).
#'
#' @param x A `bouquet_plot` object returned by [make_plot_bouquet()].
#' @param ... Unused; present for S3 compatibility.
#'
#' @return `x`, invisibly.
#'
#' @export
print.bouquet_plot <- function(x, ...) {
  m <- attr(x, "bq_meta")
  if (!is.null(m)) {
    norm_tag <- if (isTRUE(m$normalise)) " [normalised]" else ""
    map_tag  <- if (isTRUE(m$has_map))  " + map" else ""
    cat(sprintf(
      "<bouquet_plot>  %d series | theta = %.1f deg | binding: %s%s%s\n",
      m$n_series, m$theta_deg, m$binding_series, norm_tag, map_tag
    ))
  }
  # Dispatch to ggplot / patchwork print
  cl <- class(x)
  class(x) <- cl[cl != "bouquet_plot"]
  print(x, ...)
  invisible(x)
}

#' Format a bouquet_plot for display
#'
#' Returns a one-line description of the bouquet plot suitable for
#' [base::cat()] or automatic printing in interactive sessions.
#'
#' @param x A `bouquet_plot` object returned by [make_plot_bouquet()].
#' @param ... Unused; present for S3 compatibility.
#'
#' @return A character string description.
#'
#' @export
format.bouquet_plot <- function(x, ...) {
  m <- attr(x, "bq_meta")
  if (is.null(m))
    return("<bouquet_plot>")
  norm_tag <- if (isTRUE(m$normalise)) " [normalised]" else ""
  map_tag  <- if (isTRUE(m$has_map))  " + map" else ""
  sprintf(
    "<bouquet_plot>  %d series | theta = %.1f deg | binding: %s%s%s",
    m$n_series, m$theta_deg, m$binding_series, norm_tag, map_tag
  )
}