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
#' The rotation angle \eqn{\theta} is derived automatically from the data.
#' The binding constraint is the series whose cumulative heading sweeps the
#' widest arc (measured as `max(cumsum) - min(cumsum)` of the signed steps).
#' \eqn{\theta} is set to `ceiling_pct` of the largest angle that would not
#' cause that series to complete a full 360° loop.
#'
#' @param data A wide data frame or tibble. One column must be the time index;
#'   all remaining columns must be numeric and are treated as individual time
#'   series.
#' @param time_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The time index
#'   column. Accepts a bare column name, a string, or a numeric position.
#'   Defaults to the first column.
#' @param stem_colors Controls the colour of the time series lines.
#'   Three options are accepted:
#'   \itemize{
#'     \item A single hex string (e.g. `"#3a7d2c"`) — all lines share that
#'       colour. This is the default (`"#3a7d2c"`, a plant green).
#'     \item A character vector of length > 1 — one colour per series, matched
#'       by position; recycled silently if shorter than the number of series.
#'     \item `"greens"` — each series is assigned a distinct shade from a
#'       curated palette of natural greens.
#'   }
#' @param ceiling_pct A single number in `(0, 1]`. The fraction of the
#'   theoretical maximum angle to use as \eqn{\theta}. Lower values provide
#'   more visual headroom between paths; higher values make turns more
#'   pronounced. Defaults to `0.80`.
#' @param launch_deg A single number. The initial heading of all series in
#'   degrees, measured counter-clockwise from the positive x-axis. `90`
#'   points straight up (north). Defaults to `90`.
#' @param marker_every A positive integer. If supplied, a filled circle is
#'   plotted at every `marker_every`-th step along each path to aid reading
#'   of temporal progress. `NULL` (default) suppresses markers.
#' @param show_rings Logical. Whether to draw faint concentric reference
#'   rings centred on the origin. Defaults to `FALSE`.
#' @param dark_mode Logical. `TRUE` uses a dark navy background;
#'   `FALSE` (default) uses a light off-white background.
#' @param title A string passed to [ggplot2::labs()] as the plot title.
#'   `NULL` (default) produces no title.
#' @param subtitle A string passed to [ggplot2::labs()] as the subtitle.
#'   `NULL` (default) auto-generates a subtitle reporting \eqn{\theta}, the
#'   binding series, and its heading sweep.
#' @param hide_legend_after A single positive integer. The legend is suppressed
#'   when the number of series equals or exceeds this value. Defaults to `10`.
#' @param flower_color Controls the colour of the end-of-series ✿ symbols.
#'   Three options are accepted:
#'   \itemize{
#'     \item A single hex string (e.g. `"#f472b6"`) — all flowers share that
#'       colour. This is the default (`"#f472b6"`, a warm pink).
#'     \item A character vector of length > 1 — one colour per series, matched
#'       by position; recycled silently if shorter than the number of series.
#'     \item `"blossom"` — each flower is assigned a random blossom-like colour
#'       sampled from a curated palette of pinks, mauves, and soft corals.
#'   }
#' @param verbose Logical. Whether to print angle diagnostics — the heading
#'   sweep per series, the binding series, and the final \eqn{\theta} — to
#'   the console. Defaults to `TRUE`.
#'
#' @return A [ggplot2::ggplot] object, returned visibly.
#'
#' @details
#' ## Input format
#' `data` must be in **wide** format: one row per time step, one column per
#' series, plus the time index column. All series must have the same length.
#'
#' ## Algorithm
#' For each series, the signed first difference is binarised to
#' \eqn{+1} (increase), \eqn{-1} (decrease), or \eqn{0} (no change).
#' The turtle heading after step \eqn{i} is:
#'
#' \deqn{h_i = h_{i-1} + d_i \cdot \theta}
#'
#' where \eqn{d_i \in \{-1, 0, +1\}}. The Cartesian coordinates are
#' updated as \eqn{(x_i, y_i) = (x_{i-1} + \cos h_i,\; y_{i-1} + \sin h_i)}.
#'
#' ## Angle derivation
#' A complete loop occurs when the heading sweeps \eqn{\geq 360°}. The sweep
#' of a series equals \eqn{(\max C - \min C) \cdot \theta}, where \eqn{C} is
#' the cumulative sum of the signed steps. The angle is therefore:
#'
#' \deqn{\theta = \frac{360°}{\max_s(\max C_s - \min C_s)} \times \texttt{ceiling\_pct}}
#'
#' Note that `max(abs(C))` is **not** the correct constraint: a series that
#' dips to \eqn{-5} then climbs to \eqn{+20} has a sweep of 25 steps even
#' though neither extreme alone reaches 25.
#'
#' @seealso
#' The technique is structurally related to **DNA walk** visualisations
#' (Gates 1986; Yau et al. 2003), which encode nucleotide sequences as
#' 2-D paths using similar binary-direction rules.
#'
#' @examples
#' set.seed(42)
#' n      <- 52
#' weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
#' season <- sin(seq(0, 2 * pi, length.out = n))
#'
#' gw_data <- tibble::tibble(
#'   week        = weeks,
#'   `Station A` = 8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.18)),
#'   `Station B` = 7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.22)),
#'   `Station C` = 9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.15))
#' )
#'
#' # Minimal call
#' make_plot_bouquet(gw_data, time_col = week)
#'
#' # Full call with all options
#' make_plot_bouquet(
#'   data         = gw_data,
#'   time_col     = week,
#'   title        = "Groundwater Level Fluctuations \u2014 2023",
#'   stem_colors       = c("#4fc3f7", "#81c784", "#ffb74d"),
#'   ceiling_pct  = 0.80,
#'   launch_deg   = 90,
#'   marker_every = 13L,
#'   show_rings   = TRUE,
#'   dark_mode    = FALSE,
#'   verbose      = FALSE
#' )
#'
#' @export
#'
#' @importFrom dplyr filter group_by mutate rename select slice_tail summarise
#'   ungroup case_when .data
#' @importFrom tidyr pivot_longer
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggplot aes annotate coord_equal element_blank
#'   element_rect element_text geom_path geom_point geom_text labs
#'   margin scale_color_manual scale_fill_manual theme theme_void unit
#' @importFrom ggforce geom_circle
#' @importFrom tibble tibble
make_plot_bouquet <- function(
    data,
    time_col     = 1,
    stem_colors       = "#3a7d2c",
    ceiling_pct  = 0.80,
    launch_deg   = 90,
    marker_every = NULL,
    show_rings   = FALSE,
    dark_mode    = FALSE,
    title        = NULL,
    subtitle          = NULL,
    hide_legend_after = 10L,
    flower_color      = "#f472b6",
    verbose           = TRUE) {

  # ── Validate inputs ────────────────────────────────────────────────────────
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.numeric(ceiling_pct) || length(ceiling_pct) != 1L ||
      ceiling_pct <= 0 || ceiling_pct > 1) {
    stop("`ceiling_pct` must be a single number in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(launch_deg) || length(launch_deg) != 1L) {
    stop("`launch_deg` must be a single number.", call. = FALSE)
  }
  if (!is.null(marker_every)) {
    if (!is.numeric(marker_every) || length(marker_every) != 1L ||
        marker_every < 1) {
      stop("`marker_every` must be a single positive integer or NULL.",
           call. = FALSE)
    }
    marker_every <- as.integer(marker_every)
  }
  if (!is.logical(show_rings) || length(show_rings) != 1L) {
    stop("`show_rings` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(dark_mode) || length(dark_mode) != 1L) {
    stop("`dark_mode` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("`verbose` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(hide_legend_after) || length(hide_legend_after) != 1L ||
      hide_legend_after < 1) {
    stop("`hide_legend_after` must be a single positive integer.", call. = FALSE)
  }
  hide_legend_after <- as.integer(hide_legend_after)

  # ── Identify columns ───────────────────────────────────────────────────────
  time_col_name <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_names  <- setdiff(names(data), time_col_name)

  if (length(series_names) < 1L) {
    stop(
      "`data` must contain at least one numeric series column ",
      "besides the time column.",
      call. = FALSE
    )
  }

  non_numeric <- series_names[
    !vapply(data[series_names], is.numeric, logical(1L))
  ]
  if (length(non_numeric) > 0L) {
    stop(
      "All series columns must be numeric. Non-numeric columns found: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  # ── Compute direction ──────────────────────────────────────────────────────
  df_long <- data |>
    tidyr::pivot_longer(
      cols      = dplyr::all_of(series_names),
      names_to  = "series",
      values_to = "value"
    ) |>
    dplyr::rename(time = dplyr::all_of(time_col_name)) |>
    dplyr::group_by(.data$series) |>
    dplyr::mutate(
      step      = dplyr::row_number(),
      delta     = .data$value - dplyr::lag(.data$value),
      direction = dplyr::case_when(
        is.na(.data$delta)    ~ "start",
        .data$delta > 0       ~ "increase",
        .data$delta < 0       ~ "decrease",
        TRUE                  ~ "no_change"
      )
    ) |>
    dplyr::ungroup()

  # ── Derive theta ───────────────────────────────────────────────────────────
  # The heading sweeps (max(cum_net) - min(cum_net)) * theta degrees.
  # Constraint: sweep < 360 for every series.
  sweep_tbl <- df_long |>
    dplyr::filter(.data$direction != "start") |>
    dplyr::group_by(.data$series) |>
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
  binding_series <- sweep_tbl$series[which.max(sweep_tbl$cum_net_range)]
  theta_ceiling  <- 360 / max_sweep
  theta_deg      <- theta_ceiling * ceiling_pct
  theta_rad      <- theta_deg * pi / 180

  if (verbose) {
    cat("=== Heading sweep per series (max - min of cumulative net steps) ===\n")
    print(sweep_tbl)
    cat(
      sprintf("\nBinding series : %s (sweep = %d steps)\n",
              binding_series, max_sweep),
      sprintf("Ceiling angle  : %.2f\u00b0\n", theta_ceiling),
      sprintf("theta (%.0f%%)  : %.2f\u00b0\n\n",
              ceiling_pct * 100, theta_deg)
    )
  }

  # ── Build turtle paths ─────────────────────────────────────────────────────
  build_path <- function(df) {
    n          <- nrow(df)
    x          <- numeric(n)
    y          <- numeric(n)
    heading    <- numeric(n)
    heading[1] <- launch_deg * pi / 180

    for (i in seq(2L, n)) {
      turn       <- switch(
        df$direction[i],
        increase  =  theta_rad,
        decrease  = -theta_rad,
        no_change =  0,
        0
      )
      heading[i] <- heading[i - 1L] + turn
      x[i]       <- x[i - 1L] + cos(heading[i])
      y[i]       <- y[i - 1L] + sin(heading[i])
    }

    dplyr::mutate(df,
      x           = x,
      y           = y,
      heading_deg = heading * 180 / pi
    )
  }

  path_data <- df_long |>
    dplyr::group_by(.data$series) |>
    dplyr::group_modify(~ build_path(.x)) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$series) |>
    dplyr::mutate(
      progress = (.data$step - 1L) / (max(.data$step) - 1L)
    ) |>
    dplyr::ungroup()

  # ── Resolve line colours ───────────────────────────────────────────────────
  n_series <- length(series_names)
  # Curated natural greens palette
  greens_pal <- c(
    "#1a5c2a", "#2d8a45", "#3a7d2c", "#52a85e", "#74c476",
    "#98d494", "#a8d5a2", "#4e9e57", "#236b30", "#6abf69"
  )

  if (!is.character(stem_colors)) {
    stop(
      '`stem_colors` must be a character vector of hex colours or "greens".',
      call. = FALSE
    )
  }

  stem_colors <- if (identical(stem_colors, "greens")) {
    setNames(rep_len(greens_pal, n_series), series_names)
  } else {
    if (length(stem_colors) < n_series) {
      warning(
        "Fewer colours supplied than series; colours will be recycled.",
        call. = FALSE
      )
    }
    setNames(rep_len(stem_colors, n_series), series_names)
  }

  # ── Theme values ───────────────────────────────────────────────────────────
  bg_col       <- if (dark_mode) "#1a1a2e" else "#f8f8f5"
  text_col     <- if (dark_mode) "white"   else "#1a1a2e"
  subtext_col  <- if (dark_mode) "grey60"  else "grey40"
  ring_col     <- if (dark_mode) "white"   else "grey50"
  origin_outer <- if (dark_mode) "white"   else "#1a1a2e"
  origin_inner <- bg_col
  ring_alpha   <- if (dark_mode) 0.07 else 0.18

  # ── Auto-subtitle ──────────────────────────────────────────────────────────
  n_steps   <- max(path_data$step)
  time_vals <- data[[time_col_name]]

  # Detect the typical interval between consecutive time steps
  format_interval <- function(tv) {
    if (length(tv) < 2L) return("1 observation")
    med_diff <- median(as.numeric(diff(tv)), na.rm = TRUE)

    if (inherits(tv, c("Date", "POSIXct", "POSIXlt"))) {
      label <- dplyr::case_when(
        abs(med_diff -   1) < 0.6  ~ "daily",
        abs(med_diff -   7) < 2    ~ "weekly",
        abs(med_diff -  14) < 3    ~ "fortnightly",
        abs(med_diff -  30) < 5    ~ "monthly",
        abs(med_diff -  91) < 10   ~ "quarterly",
        abs(med_diff - 365) < 20   ~ "yearly",
        TRUE                       ~ paste0("every ", round(med_diff), " days")
      )
    } else {
      label <- paste0("interval ", round(med_diff, 2L))
    }
    label
  }

  format_endpoint <- function(tv) {
    if (inherits(tv, c("Date", "POSIXct", "POSIXlt"))) {
      format(tv, "%d %b %Y")
    } else {
      as.character(tv)
    }
  }

  time_sentence <- paste0(
    n_series, " series  \u00b7  ",
    n_steps, " ", format_interval(time_vals), " observations",
    "  \u00b7  ",
    format_endpoint(min(time_vals, na.rm = TRUE)), " \u25cb",
    " \u2013 ",
    format_endpoint(max(time_vals, na.rm = TRUE)), " \u273f"
  )

  if (is.null(subtitle)) {
    subtitle <- paste0(
      time_sentence, "\n",
      "Angular Accumulation Plot  \u00b7  ",
      "\u03b8 = ", round(theta_deg, 1L), "\u00b0",
      "  (binding series: ", binding_series,
      ", sweep = ", max_sweep, " steps)\n",
      "\u21ba\u00a0increase  |  \u21bb\u00a0decrease  |  straight\u00a0=\u00a0no change"
    )
  }

  # ── Reference ring radii ───────────────────────────────────────────────────
  extent     <- max(sqrt(path_data$x^2 + path_data$y^2), na.rm = TRUE)
  ring_step  <- max(5, round(extent / 5 / 5) * 5)
  ring_radii <- seq(ring_step, floor(extent / ring_step) * ring_step,
                    by = ring_step)

  # ── Assemble plot ──────────────────────────────────────────────────────────
  p <- ggplot2::ggplot(
    path_data,
    ggplot2::aes(x = .data$x, y = .data$y, group = .data$series,
                 color = .data$series)
  )

  if (show_rings && length(ring_radii) > 0L) {
    p <- p + ggforce::geom_circle(
      data = tibble::tibble(x0 = 0, y0 = 0, r = ring_radii),
      ggplot2::aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
      inherit.aes = FALSE,
      color       = ring_col,
      alpha       = ring_alpha,
      linewidth   = 0.3
    )
  }

  p <- p +
    ggplot2::geom_path(
      linewidth = 1.1, alpha = 0.88,
      lineend = "round", linejoin = "round"
    )

  if (!is.null(marker_every)) {
    marker_data <- dplyr::filter(path_data, .data$step %% marker_every == 1L)
    p <- p + ggplot2::geom_point(
      data  = marker_data,
      ggplot2::aes(fill = .data$series),
      shape = 21L, size = 2.0, color = bg_col, stroke = 0.6
    )
  }

  # Origin marker — annotate() avoids broadcasting over the full dataset
  p <- p +
    ggplot2::annotate(
      "point", x = 0, y = 0,
      color = origin_outer, size = 5, alpha = 0.9
    ) +
    ggplot2::annotate(
      "point", x = 0, y = 0,
      color = origin_inner, size = 2.2
    )

  # ── Resolve flower colours ─────────────────────────────────────────────────
  # Curated blossom palette: pinks, mauves, soft corals, lilacs
  blossom_pal <- c(
    "#f472b6", "#f9a8d4", "#e879a0", "#c084fc", "#d8b4fe",
    "#fb7185", "#fda4af", "#f0abfc", "#e9a0c9", "#ff8fab"
  )

  if (!is.character(flower_color)) {
    stop(
      '`flower_color` must be a character vector of hex colours or "blossom".',
      call. = FALSE
    )
  }

  flower_colors <- if (identical(flower_color, "blossom")) {
    # Random blossom-like colour per series, recycled if more series than palette
    sample(rep_len(blossom_pal, n_series))
  } else {
    rep_len(flower_color, n_series)
  }

  # Strip names to guarantee plain character indexing
  flower_colors <- unname(as.character(flower_colors))

  # End-of-series flower symbols (✿ BLACK FLORETTE, U+273F)
  # One layer per series to avoid conflicting with the path colour scale.
  end_data <- dplyr::slice_tail(dplyr::group_by(path_data, .data$series),
                                n = 1L)

  for (i in seq_len(n_series)) {
    p <- p + ggplot2::geom_text(
      data        = dplyr::filter(end_data, .data$series == series_names[[i]]),
      mapping     = ggplot2::aes(x = .data$x, y = .data$y, label = "\u273f"),
      inherit.aes = FALSE,
      color       = flower_colors[i],
      size        = 7, family = "sans", show.legend = FALSE
    )
  }

  # Scales and coordinates
  p <- p +
    ggplot2::scale_color_manual(values = stem_colors) +
    ggplot2::coord_equal(clip = "off")

  if (!is.null(marker_every)) {
    p <- p + ggplot2::scale_fill_manual(values = stem_colors)
  }

  # Caption
  caption <- "\u2740\u00a0= last step  \u00b7  \u25cb\u00a0= origin"
  if (!is.null(marker_every)) {
    caption <- paste0(
      caption, "  \u00b7  \u25cf\u00a0= every ", marker_every, " steps"
    )
  }

  # Theme
  p <- p +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = bg_col,      color = NA),
      panel.background  = ggplot2::element_rect(fill = bg_col,      color = NA),
      legend.position   = if (n_series >= hide_legend_after) "none" else "bottom",
      legend.direction  = "horizontal",
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(color = subtext_col, size = 13),
      legend.key.width  = ggplot2::unit(2,   "lines"),
      legend.key.height = ggplot2::unit(0.5, "lines"),
      legend.spacing.x  = ggplot2::unit(0.8, "lines"),
      plot.title        = ggplot2::element_text(
        color = text_col, size = 22,
        hjust = 0.5, margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle     = ggplot2::element_text(
        color = subtext_col, size = 12,
        hjust = 0.5, margin = ggplot2::margin(b = 14)
      ),
      plot.caption      = ggplot2::element_text(
        color = subtext_col, size = 10,
        hjust = 0.5, margin = ggplot2::margin(t = 10)
      ),
      plot.margin       = ggplot2::margin(30, 30, 20, 30)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, caption = caption)

  p
}