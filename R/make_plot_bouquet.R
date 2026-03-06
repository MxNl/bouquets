# ── Unexported helpers ────────────────────────────────────────────────────────

#' Format a time vector's typical interval as a human-readable string
#'
#' @param tv A vector of time values (Date, POSIXct, or numeric).
#' @return A single character string such as `"weekly"` or `"monthly"`.
#' @noRd
.format_interval <- function(tv) {
  if (length(tv) < 2L) return("1 observation")

  med_diff <- median(as.numeric(diff(sort(unique(tv)))), na.rm = TRUE)

  if (!inherits(tv, c("Date", "POSIXct", "POSIXlt"))) {
    return(paste0("interval ", round(med_diff, 2L)))
  }

  # Named breakpoints: upper boundary (exclusive) for each label
  breaks <- c(daily = 1.6, weekly = 9, fortnightly = 22,
              monthly = 60, quarterly = 180, yearly = 500)
  idx    <- findInterval(med_diff, breaks, rightmost.closed = FALSE)

  if (idx < length(breaks)) {
    names(breaks)[idx + 1L]
  } else {
    paste0("every ", round(med_diff), " days")
  }
}


#' Format a single time value as a display string
#'
#' @param tv A scalar time value.
#' @return A character string.
#' @noRd
.format_endpoint <- function(tv) {
  if (inherits(tv, c("Date", "POSIXct", "POSIXlt"))) {
    format(tv, "%d %b %Y")
  } else {
    as.character(tv)
  }
}


#' Resolve a colour quosure to a named character vector of hex codes
#'
#' Handles the four modes shared by `stem_colors` and `flower_colors`:
#' a single/multi hex vector, a keyword string, or a bare column reference.
#'
#' @param q          A quosure (captured with `rlang::enquo()`).
#' @param data       The original long-format data frame.
#' @param path_data  The path data frame (has a `series` column).
#' @param series_names Character vector of ordered series identifiers.
#' @param keyword    The special keyword string (e.g. `"greens"` or
#'   `"blossom"`).
#' @param palette    Named character vector: the curated palette for the
#'   keyword mode.
#' @param hue_range  Length-2 numeric vector giving the hue range for
#'   column-reference mode.
#' @param dark_mode  Logical; controls HCL lightness in column-reference mode.
#' @param arg_name   String used in error / warning messages.
#' @return A named character vector of hex colours, one per series.
#' @noRd
.resolve_colors <- function(q, data, path_data, series_names,
                            keyword, palette,
                            dark_mode, arg_name) {
  n_series <- length(series_names)

  if (rlang::quo_is_symbol(q) && rlang::as_name(q) %in% names(path_data)) {
    # ── Column-reference mode ────────────────────────────────────────────────
    # hues::iwanthue() spaces colours evenly across the perceptual gamut;
    # lightness bounds are shifted for dark vs light backgrounds.
    col_name <- rlang::as_name(q)
    lvls     <- unique(as.character(path_data[[col_name]]))
    n_lvls   <- length(lvls)
    lvl_cols <- stats::setNames(
      hues::iwanthue(
        n_lvls,
        lmin = if (dark_mode) 50 else 30,
        lmax = if (dark_mode) 85 else 70
      ),
      lvls
    )
    # One representative value per series (first row)
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
    # ── Scalar / vector / keyword mode ───────────────────────────────────────
    value <- rlang::eval_tidy(q)

    if (!is.character(value)) {
      stop(
        sprintf(
          "`%s` must be a hex colour vector, \"%s\", or a column name.",
          arg_name, keyword
        ),
        call. = FALSE
      )
    }

    raw <- if (identical(value, keyword)) {
      rep_len(palette, n_series)
    } else {
      if (length(value) > 1L && length(value) < n_series) {
        warning(
          sprintf("Fewer `%s` supplied than series; recycling.", arg_name),
          call. = FALSE
        )
      }
      rep_len(value, n_series)
    }

    colours <- stats::setNames(unname(as.character(raw)), series_names)
  }

  colours
}


# ── Main exported function ─────────────────────────────────────────────────────

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
#' @param data A long-format data frame or tibble with at minimum a time
#'   column, a series identifier column, and a numeric value column. Additional
#'   factor or character columns may be referenced by `stem_colors`,
#'   `flower_colors`, and `facet_by`.
#' @param time_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The time index
#'   column. Defaults to the first column.
#' @param series_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The column
#'   identifying each individual time series (e.g. a station name).
#'   Defaults to the second column.
#' @param value_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The numeric
#'   column containing the measured values. Defaults to the third column.
#' @param stem_colors Controls the colour of the time series lines. Four
#'   options are accepted:
#'   \itemize{
#'     \item A single hex string — all lines share that colour. Default:
#'       `"#3a7d2c"` (plant green).
#'     \item A character vector of hex strings, one per series — matched by
#'       position; recycled with a warning if too short.
#'     \item `"greens"` — each series receives a distinct shade from a curated
#'       palette of natural greens.
#'     \item A bare column name referencing a factor or character column in
#'       `data` — each series is coloured by that column's value.
#'   }
#' @param flower_colors Controls the colour of the end-of-series ✿ symbols.
#'   Four options are accepted:
#'   \itemize{
#'     \item A single hex string — all flowers share that colour. Default:
#'       `"#f472b6"` (warm pink).
#'     \item A character vector of hex strings, one per series — matched by
#'       position; recycled with a warning if too short.
#'     \item `"blossom"` — each flower is assigned a colour from a curated
#'       palette of pinks, mauves, and soft corals.
#'     \item A bare column name referencing a factor or character column in
#'       `data` — each flower is coloured by that column's value.
#'   }
#' @param facet_by <[`tidy-select`][dplyr::dplyr_tidy_select]> An optional
#'   bare column name referencing a factor or character column in `data`. When
#'   supplied the plot is split into facets via [ggplot2::facet_wrap()], one
#'   panel per level. `NULL` (default) produces a single panel.
#' @param ceiling_pct A single number in `(0, 1]`. The fraction of the
#'   theoretical maximum angle to use as \eqn{\theta}. Lower values give more
#'   visual headroom; higher values make turns more pronounced. Default `0.80`.
#' @param launch_deg A single number. The initial heading of all series in
#'   degrees, measured counter-clockwise from the positive x-axis. `90` points
#'   straight up (north). Defaults to `90`.
#' @param marker_every A positive integer. If supplied, a filled circle is
#'   plotted at every `marker_every`-th step to aid reading of temporal
#'   progress. `NULL` (default) suppresses markers.
#' @param show_rings Logical. Whether to draw faint concentric reference rings
#'   centred on the origin. Defaults to `FALSE`.
#' @param dark_mode Logical. `TRUE` uses a dark navy background; `FALSE`
#'   (default) uses a light off-white background.
#' @param title A string used as the plot title. `NULL` (default) = no title.
#' @param subtitle A string used as the subtitle. `NULL` (default)
#'   auto-generates a subtitle reporting \eqn{\theta}, the binding series, time
#'   range, and interval.
#' @param hide_legend_after A single positive integer. The legend is suppressed
#'   when the number of series is greater than or equal to this value.
#'   Defaults to `10`.
#' @param verbose Logical. Whether to print angle diagnostics to the console.
#'   Defaults to `TRUE`.
#'
#' @return A [ggplot2::ggplot] object, returned visibly.
#'
#' @details
#' ## Input format
#' `data` must be in **long** format: one row per time step per series. Series
#' should share the same time index values and have equal length.
#'
#' ## Algorithm
#' For each series, the signed first difference is binarised to \eqn{+1}
#' (increase), \eqn{-1} (decrease), or \eqn{0} (no change). The heading at
#' step \eqn{i} is accumulated as:
#'
#' \deqn{h_i = h_{\text{launch}} + \sum_{k=2}^{i} d_k \cdot \theta}
#'
#' which is computed in one vectorised pass via [base::cumsum()]. The
#' Cartesian coordinates follow as:
#' \eqn{x_i = \sum_{k=2}^{i} \cos(h_k)}, and similarly for \eqn{y}.
#'
#' ## Angle derivation
#' A complete loop occurs when the heading sweeps \eqn{\geq 360°}. The sweep
#' equals \eqn{(\max C - \min C) \cdot \theta}, where \eqn{C} is the cumulative
#' sum of the signed steps. The angle is therefore:
#'
#' \deqn{\theta = \frac{360°}{\max_s(\max C_s - \min C_s)} \times
#'   \texttt{ceiling\_pct}}
#'
#' Note that `max(abs(C))` is **not** the correct constraint: a series that
#' dips to \eqn{-5} then climbs to \eqn{+20} sweeps 25 steps' worth of angle
#' even though neither extreme alone reaches 25.
#'
#' @seealso
#' The technique is structurally related to **DNA walk** visualisations
#' (Gates 1986; Yau et al. 2003), which encode nucleotide sequences as 2-D
#' paths using similar binary-direction rules.
#'
#' @examples
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
#' # Column-driven colours and faceting
#' make_plot_bouquet(
#'   data          = gw_long,
#'   time_col      = week,
#'   series_col    = station,
#'   value_col     = level_m,
#'   stem_colors   = region,
#'   flower_colors = region,
#'   facet_by      = region,
#'   title         = "Groundwater Level Fluctuations \u2014 2023"
#' )
#'
#' @export
#'
#' @importFrom dplyr all_of case_when filter group_by group_modify mutate
#'   pull rename row_number select slice_head slice_tail summarise ungroup
#'   .data
#' @importFrom rlang as_name enquo eval_tidy quo_is_null quo_is_symbolic
#' @importFrom ggplot2 aes annotate coord_equal element_blank element_rect
#'   element_text facet_wrap geom_path geom_point geom_text ggplot labs
#'   margin scale_color_manual scale_fill_manual theme theme_void unit vars
#' @importFrom ggforce geom_circle
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
    verbose           = TRUE) {

  # ── Capture colour / facet arguments as quosures before any evaluation ─────
  stem_colors_q   <- rlang::enquo(stem_colors)
  flower_colors_q <- rlang::enquo(flower_colors)
  facet_by_q      <- rlang::enquo(facet_by)

  # ── Validate scalar inputs ─────────────────────────────────────────────────
  if (!is.data.frame(data))
    stop("`data` must be a data frame or tibble.", call. = FALSE)

  if (!is.numeric(ceiling_pct) || length(ceiling_pct) != 1L ||
      ceiling_pct <= 0 || ceiling_pct > 1)
    stop("`ceiling_pct` must be a single number in (0, 1].", call. = FALSE)

  if (!is.numeric(launch_deg) || length(launch_deg) != 1L)
    stop("`launch_deg` must be a single number.", call. = FALSE)

  if (!is.null(marker_every)) {
    if (!is.numeric(marker_every) || length(marker_every) != 1L ||
        marker_every < 1L)
      stop("`marker_every` must be a single positive integer or NULL.",
           call. = FALSE)
    marker_every <- as.integer(marker_every)
  }

  if (!is.logical(show_rings)  || length(show_rings)  != 1L)
    stop("`show_rings` must be TRUE or FALSE.",  call. = FALSE)
  if (!is.logical(dark_mode)   || length(dark_mode)   != 1L)
    stop("`dark_mode` must be TRUE or FALSE.",   call. = FALSE)
  if (!is.logical(verbose)     || length(verbose)     != 1L)
    stop("`verbose` must be TRUE or FALSE.",     call. = FALSE)

  if (!is.numeric(hide_legend_after) || length(hide_legend_after) != 1L ||
      hide_legend_after < 1L)
    stop("`hide_legend_after` must be a single positive integer.",
         call. = FALSE)
  hide_legend_after <- as.integer(hide_legend_after)

  # ── Resolve structural column names ───────────────────────────────────────
  time_col_name   <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_col_name <- names(dplyr::select(data, {{ series_col }}))[1L]
  value_col_name  <- names(dplyr::select(data, {{ value_col }}))[1L]

  if (!is.numeric(data[[value_col_name]]))
    stop(sprintf("Column '%s' must be numeric.", value_col_name),
         call. = FALSE)

  facet_col_name <- if (rlang::quo_is_null(facet_by_q)) {
    NULL
  } else {
    names(dplyr::select(data, {{ facet_by }}))[1L]
  }

  # ── Validate any extra annotation columns ─────────────────────────────────
  # quo_is_symbol() (not quo_is_symbolic()) guards against call quosures like
  # c("#aaa","#bbb") reaching as_name(), which would throw an error.
  colour_sym_name <- function(q) {
    if (rlang::quo_is_symbol(q)) rlang::as_name(q) else NULL
  }
  candidate_cols <- unique(c(
    colour_sym_name(stem_colors_q),
    colour_sym_name(flower_colors_q),
    facet_col_name
  ))
  missing_cols <- candidate_cols[!candidate_cols %in% names(data)]
  if (length(missing_cols) > 0L)
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)

  # ── Prepare long data ─────────────────────────────────────────────────────
  # Rename structural columns to stable internal names (bq_ prefix avoids
  # conflict with tidyselect's .data pronoun for dot-prefixed names).
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
        is.na(.data$delta)  ~ "start",
        .data$delta  > 0    ~ "increase",
        .data$delta  < 0    ~ "decrease",
        TRUE                ~ "no_change"
      )
    ) |>
    dplyr::ungroup()

  # Use factor levels if available, otherwise sort alphabetically, to ensure
  # a stable and predictable series order across calls.
  raw_series <- df_long$bq_s
  series_names <- if (is.factor(raw_series)) {
    levels(raw_series)
  } else {
    sort(unique(as.character(raw_series)))
  }
  n_series <- length(series_names)

  # ── Derive theta ───────────────────────────────────────────────────────────
  # A complete loop occurs when the heading sweeps >= 360°.
  # Sweep = (max(cum_net) - min(cum_net)) * theta.
  # Constraint: max sweep < 360°  =>  theta < 360 / max_range.
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
    cat("=== Heading sweep per series (max - min of cumulative net steps) ===\n")
    print(sweep_tbl)
    cat(sprintf("\nBinding series : %s (sweep = %d steps)\n",
                binding_series, max_sweep))
    cat(sprintf("Ceiling angle  : %.2f\u00b0\n", theta_ceiling))
    cat(sprintf("theta (%.0f%%)  : %.2f\u00b0\n\n",
                ceiling_pct * 100, theta_deg))
  }

  # ── Build turtle paths (vectorised) ───────────────────────────────────────
  # Heading at step i: h_i = launch_rad + cumsum(turns)[i], where turns[1] = 0
  # and turns[i] = ±theta_rad or 0 for i >= 2.
  # Position: x[1] = 0; x[i] = cumsum(cos(h[2:n]))[i-1] for i >= 2.
  build_path <- function(df) {
    turns      <- dplyr::case_when(
      df$direction == "increase" ~  theta_rad,
      df$direction == "decrease" ~ -theta_rad,
      TRUE                       ~  0
    )
    turns[1L]  <- 0                         # first row is the origin
    heading    <- launch_rad + cumsum(turns)
    step_cos   <- cos(heading[-1L])
    step_sin   <- sin(heading[-1L])

    dplyr::mutate(df,
      x           = c(0, cumsum(step_cos)),
      y           = c(0, cumsum(step_sin)),
      heading_deg = heading * 180 / pi
    )
  }

  path_data <- df_long |>
    dplyr::group_by(.data$bq_s) |>
    dplyr::group_modify(~ build_path(.x)) |>
    dplyr::ungroup() |>
    dplyr::rename(series = "bq_s")

  # ── Resolve stem and flower colours ───────────────────────────────────────
  greens_pal  <- c("#1a5c2a", "#2d8a45", "#3a7d2c", "#52a85e", "#74c476",
                   "#98d494", "#a8d5a2", "#4e9e57", "#236b30", "#6abf69")
  blossom_pal <- c("#f472b6", "#f9a8d4", "#e879a0", "#c084fc", "#d8b4fe",
                   "#fb7185", "#fda4af", "#f0abfc", "#e9a0c9", "#ff8fab")

  stem_colors_resolved <- .resolve_colors(
    q            = stem_colors_q,
    data         = data,
    path_data    = path_data,
    series_names = series_names,
    keyword      = "greens",
    palette      = greens_pal,
    dark_mode    = dark_mode,
    arg_name     = "stem_colors"
  )

  flower_colors_resolved <- .resolve_colors(
    q            = flower_colors_q,
    data         = data,
    path_data    = path_data,
    series_names = series_names,
    keyword      = "blossom",
    palette      = blossom_pal,
    dark_mode    = dark_mode,
    arg_name     = "flower_colors"
  )

  # ── Theme palette ──────────────────────────────────────────────────────────
  bg_col       <- if (dark_mode) "#1a1a2e" else "#f8f8f5"
  text_col     <- if (dark_mode) "white"   else "#1a1a2e"
  subtext_col  <- if (dark_mode) "grey60"  else "grey40"
  ring_col     <- if (dark_mode) "white"   else "grey50"
  origin_outer <- if (dark_mode) "white"   else "#1a1a2e"
  origin_inner <- bg_col
  ring_alpha   <- if (dark_mode) 0.07      else 0.18

  # ── Auto-subtitle ──────────────────────────────────────────────────────────
  n_steps   <- max(path_data$step)
  time_vals <- data[[time_col_name]]

  if (is.null(subtitle)) {
    time_sentence <- paste0(
      n_series, " series  \u00b7  ",
      n_steps, " ", .format_interval(time_vals), " observations",
      "  \u00b7  ",
      .format_endpoint(min(time_vals, na.rm = TRUE)), " \u25cb",
      " \u2013 ",
      .format_endpoint(max(time_vals, na.rm = TRUE)), " \u273f"
    )
    subtitle <- paste0(
      time_sentence, "\n",
      "Angular Accumulation Plot  \u00b7  ",
      "\u03b8 = ", round(theta_deg, 1L), "\u00b0",
      "  (binding: ", binding_series,
      ", sweep = ", max_sweep, " steps)\n",
      "\u21ba\u00a0increase  |  \u21bb\u00a0decrease",
      "  |  straight\u00a0=\u00a0no change"
    )
  }

  # ── Reference ring radii ───────────────────────────────────────────────────
  extent     <- max(sqrt(path_data$x^2 + path_data$y^2), na.rm = TRUE)
  ring_step  <- max(5, round(extent / 25) * 5)
  ring_radii <- if (ring_step <= extent) {
    seq(ring_step, floor(extent / ring_step) * ring_step, by = ring_step)
  } else {
    numeric(0)
  }

  # ── Assemble plot layers ───────────────────────────────────────────────────
  p <- ggplot2::ggplot(
    path_data,
    ggplot2::aes(x = .data$x, y = .data$y,
                 group = .data$series, color = .data$series)
  )

  if (show_rings && length(ring_radii) > 0L) {
    p <- p + ggforce::geom_circle(
      data        = tibble::tibble(x0 = 0, y0 = 0, r = ring_radii),
      ggplot2::aes(x0 = .data$x0, y0 = .data$y0, r = .data$r),
      inherit.aes = FALSE,
      color       = ring_col,
      alpha       = ring_alpha,
      linewidth   = 0.3
    )
  }

  p <- p + ggplot2::geom_path(
    linewidth = 1.1, alpha = 0.88,
    lineend = "round", linejoin = "round"
  )

  if (!is.null(marker_every)) {
    p <- p + ggplot2::geom_point(
      data  = dplyr::filter(path_data, .data$step %% marker_every == 1L),
      ggplot2::aes(fill = .data$series),
      shape = 21L, size = 2.0, color = bg_col, stroke = 0.6
    )
  }

  # Origin marker — annotate() avoids broadcasting over the full dataset
  p <- p +
    ggplot2::annotate("point", x = 0, y = 0,
                      color = origin_outer, size = 5, alpha = 0.9) +
    ggplot2::annotate("point", x = 0, y = 0,
                      color = origin_inner, size = 2.2)

  # End-of-series flowers — attach resolved colour as a column and use I() to
  # bypass the colour scale entirely; avoids n_series separate layers.
  end_data <- path_data |>
    dplyr::group_by(.data$series) |>
    dplyr::slice_tail(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      flower_col = unname(flower_colors_resolved[.data$series])
    )

  p <- p + ggplot2::geom_text(
    data        = end_data,
    mapping     = ggplot2::aes(x = .data$x, y = .data$y,
                               label  = "\u273f",
                               color  = I(.data$flower_col)),
    inherit.aes = FALSE,
    size        = 7, family = "sans", show.legend = FALSE
  )

  # Colour and fill scales
  p <- p +
    ggplot2::scale_color_manual(values = stem_colors_resolved) +
    ggplot2::coord_equal(clip = "off")

  if (!is.null(marker_every)) {
    p <- p + ggplot2::scale_fill_manual(values = stem_colors_resolved)
  }

  # Optional faceting
  if (!is.null(facet_col_name)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[facet_col_name]]))
  }

  # ── Caption ────────────────────────────────────────────────────────────────
  caption <- "\u273f\u00a0= last step  \u00b7  \u25cb\u00a0= origin"
  if (!is.null(marker_every)) {
    caption <- paste0(caption,
                      "  \u00b7  \u25cf\u00a0= every ", marker_every, " steps")
  }

  # ── Theme ──────────────────────────────────────────────────────────────────
  p +
    ggplot2::theme_void(base_family = "sans") +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = bg_col,       color = NA),
      panel.background  = ggplot2::element_rect(fill = bg_col,       color = NA),
      strip.background  = ggplot2::element_rect(fill = bg_col,       color = NA),
      strip.text        = ggplot2::element_text(color = subtext_col, size  = 12,
                                               hjust = 0.5,
                                               margin = ggplot2::margin(b = 10)),
      legend.position   = if (n_series >= hide_legend_after) "none" else "bottom",
      legend.direction  = "horizontal",
      legend.title      = ggplot2::element_blank(),
      legend.text       = ggplot2::element_text(color = subtext_col, size  = 13),
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
}