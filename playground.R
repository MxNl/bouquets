# ============================================================
#  plot_bouquet()  —  Angular Accumulation / Flower Bouquet
#                     Time Series Plot
#
#  Each time series is drawn as a turtle-graphics path:
#    increase between steps  →  turn LEFT  by θ
#    decrease between steps  →  turn RIGHT by θ
#    no change               →  continue straight
#
#  θ is derived from the data so that no series ever
#  completes a full 360° loop.
# ============================================================


library(ggforce)   # for geom_circle (reference rings)
library(ggplot)   # for geom_circle (reference rings)
library(dplyr)   # for geom_circle (reference rings)
library(tidyr)   # for geom_circle (reference rings)


# ------------------------------------------------------------
#  plot_bouquet()
#
#  Arguments
#  ---------
#  data         Wide tibble: one column is the time index,
#               all other columns are numeric time series.
#
#  time_col     <tidy-select> or quoted name of the time
#               column. Default: first column.
#
#  colors       Named or unnamed character vector of hex
#               colours, one per series. NULL = built-in
#               palette.
#
#  ceiling_pct  Numeric in (0, 1]. Fraction of the theoretical
#               maximum angle to use. 0.80 means θ is 80% of
#               the largest angle that would NOT cause a loop.
#               Lower values give more visual headroom.
#               Default: 0.80.
#
#  launch_deg   Initial heading in degrees. 90 = straight up
#               (north). Change to rotate the whole chart,
#               e.g. 45 = upper-right. Default: 90.
#
#  marker_every Integer. Place a dot marker every N steps to
#               help read progress along each path. Set to
#               NULL or 0 to suppress. Default: NULL.
#
#  show_rings   Logical. Draw faint concentric reference rings
#               centred on the origin. Default: TRUE.
#
#  dark_mode    Logical. Dark navy background (TRUE) or clean
#               white background (FALSE). Default: TRUE.
#
#  title        Plot title string. Default: NULL (no title).
#
#  subtitle     Plot subtitle string. NULL = auto-generated
#               summary of θ and the binding series.
#
#  save_to      File path to save the plot (e.g. "plot.png").
#               NULL = do not save. Width/height fixed at
#               10 × 10 in, 200 dpi. Default: NULL.
#
#  verbose      Logical. Print angle diagnostics to console.
#               Default: TRUE.
#
#  Returns
#  -------
#  A ggplot object (invisibly).
# ------------------------------------------------------------

plot_bouquet <- function(
    data,
    time_col     = 1,
    colors       = NULL,
    ceiling_pct  = 0.80,
    launch_deg   = 90,
    marker_every = NULL,
    show_rings   = TRUE,
    dark_mode    = TRUE,
    title        = NULL,
    subtitle     = NULL,
    save_to      = NULL,
    verbose      = TRUE
) {

  # ── 0. Input validation ──────────────────────────────────
  stopifnot(
    is.data.frame(data),
    is.numeric(ceiling_pct), ceiling_pct > 0, ceiling_pct <= 1,
    is.numeric(launch_deg),
    is.logical(dark_mode),
    is.logical(show_rings),
    is.logical(verbose)
  )

  # ── 1. Identify time column & series columns ─────────────
  time_col_name <- names(dplyr::select(data, {{ time_col }}))[1]
  series_names  <- setdiff(names(data), time_col_name)

  if (length(series_names) < 1)
    stop("data must contain at least one series column besides the time column.")

  # ── 2. Pivot & compute direction ─────────────────────────
  df_long <- data |>
    pivot_longer(
      cols      = all_of(series_names),
      names_to  = "series",
      values_to = "value"
    ) |>
    rename(time = all_of(time_col_name)) |>
    group_by(series) |>
    mutate(
      step      = row_number(),
      delta     = value - lag(value),
      direction = case_when(
        is.na(delta) ~ "start",
        delta > 0    ~ "increase",
        delta < 0    ~ "decrease",
        TRUE         ~ "no_change"
      )
    ) |>
    ungroup()

  # ── 3. Angle calculation ──────────────────────────────────
  #  A full loop occurs when the heading SWEEPS >= 360°.
  #  Sweep = (max(cum_net) - min(cum_net)) * θ, where
  #  cum_net is the running sum of (+1 / -1 / 0).
  #  Constraint: sweep < 360°  →  θ < 360 / range.
  #  Apply ceiling_pct as a safety margin.

  sweep_per_series <- df_long |>
    filter(direction != "start") |>
    group_by(series) |>
    mutate(
      signed_step = case_when(
        direction == "increase" ~  1L,
        direction == "decrease" ~ -1L,
        TRUE                    ~  0L
      ),
      cum_net = cumsum(signed_step)
    ) |>
    summarise(
      cum_net_range = max(cum_net) - min(cum_net),
      .groups = "drop"
    )

  max_sweep      <- max(sweep_per_series$cum_net_range)
  binding_series <- sweep_per_series$series[which.max(sweep_per_series$cum_net_range)]
  theta_ceiling  <- 360 / max_sweep
  theta_deg      <- theta_ceiling * ceiling_pct
  theta_rad      <- theta_deg * pi / 180

  if (verbose) {
    cat("=== Heading sweep per series (max - min of cumulative net steps) ===\n")
    print(sweep_per_series)
    cat(sprintf("\nBinding series : %s  (sweep = %d steps)\n", binding_series, max_sweep))
    cat(sprintf("Ceiling        : %.2f\u00b0\n", theta_ceiling))
    cat(sprintf("θ used (%.0f%%) : %.2f\u00b0\n\n", ceiling_pct * 100, theta_deg))
  }

  # ── 4. Build turtle paths ─────────────────────────────────
  build_path <- function(df) {
    n          <- nrow(df)
    x          <- numeric(n)
    y          <- numeric(n)
    heading    <- numeric(n)
    heading[1] <- launch_deg * pi / 180

    for (i in seq(2, n)) {
      heading[i] <- heading[i - 1] + switch(
        df$direction[i],
        increase  =  theta_rad,
        decrease  = -theta_rad,
        no_change =  0,
        0
      )
      x[i] <- x[i - 1] + cos(heading[i])
      y[i] <- y[i - 1] + sin(heading[i])
    }
    df |> mutate(x = x, y = y, heading_deg = heading * 180 / pi)
  }

  path_data <- df_long |>
    group_by(series) |>
    group_modify(~ build_path(.x)) |>
    ungroup() |>
    group_by(series) |>
    mutate(progress = (step - 1) / (max(step) - 1)) |>
    ungroup()

  # ── 5. Colours ────────────────────────────────────────────
  n_series <- length(series_names)

  if (is.null(colors)) {
    default_pal <- c("#4fc3f7", "#81c784", "#ffb74d",
                     "#f06292", "#ce93d8", "#80cbc4",
                     "#fff176", "#ff8a65")
    colors <- setNames(rep_len(default_pal, n_series), series_names)
  } else if (is.null(names(colors))) {
    colors <- setNames(rep_len(colors, n_series), series_names)
  }

  # ── 6. Theme colours ──────────────────────────────────────
  bg_col      <- if (dark_mode) "#1a1a2e" else "#f8f8f5"
  text_col    <- if (dark_mode) "white"   else "#1a1a2e"
  subtext_col <- if (dark_mode) "grey60"  else "grey40"
  ring_col    <- if (dark_mode) "white"   else "grey50"
  origin_ring <- if (dark_mode) "white"   else "#1a1a2e"
  origin_fill <- if (dark_mode) "#1a1a2e" else "white"

  # ── 7. Auto subtitle ──────────────────────────────────────
  n_steps <- max(path_data$step)

  if (is.null(subtitle)) {
    subtitle <- paste0(
      "Angular Accumulation Plot  \u00b7  \u03b8 = ", round(theta_deg, 1),
      "\u00b0  (binding series: ", binding_series,
      ", sweep = ", max_sweep, " steps)\n",
      "\u21ba increase  |  \u21bb decrease  |  straight = no change"
    )
  }

  # ── 8. Reference ring radii ───────────────────────────────
  extent     <- max(sqrt(path_data$x^2 + path_data$y^2), na.rm = TRUE)
  ring_step  <- max(5, round(extent / 5 / 5) * 5)   # sensible spacing
  ring_radii <- seq(ring_step, floor(extent / ring_step) * ring_step,
                    by = ring_step)

  # ── 9. Build plot ─────────────────────────────────────────
  p <- ggplot(path_data, aes(x = x, y = y, group = series, color = series))

  if (show_rings && length(ring_radii) > 0) {
    p <- p + ggforce::geom_circle(
      data        = tibble(x0 = 0, y0 = 0, r = ring_radii),
      aes(x0 = x0, y0 = y0, r = r),
      inherit.aes = FALSE,
      color       = ring_col,
      alpha       = if (dark_mode) 0.07 else 0.15,
      linewidth   = 0.3
    )
  }

  p <- p +
    geom_path(linewidth = 1.1, alpha = 0.88,
              lineend = "round", linejoin = "round")

  if (!is.null(marker_every) && marker_every > 0) {
    p <- p + geom_point(
      data  = path_data |> filter(step %% marker_every == 1),
      aes(fill = series),
      shape = 21, size = 2.0, color = bg_col, stroke = 0.6
    )
  }

  p <- p +
    geom_point(aes(x = 0, y = 0), inherit.aes = FALSE,
               color = origin_ring, size = 5,  alpha = 0.9) +
    geom_point(aes(x = 0, y = 0), inherit.aes = FALSE,
               color = origin_fill, size = 2.2) +
    geom_point(
      data  = path_data |> group_by(series) |> slice_tail(n = 1),
      aes(color = series),
      shape = 18, size = 4.5
    ) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values  = colors) +
    coord_equal(clip = "off")

  # Bottom-left annotation
  p <- p + annotate(
    "text",
    x = -Inf, y = -Inf,
    label = sprintf("\u03b8 = %.1f\u00b0  |  n = %d steps  |  ceiling = %.0f%%",
                    theta_deg, n_steps, ceiling_pct * 100),
    hjust = -0.05, vjust = -0.8,
    size  = 2.8, color = subtext_col, family = "mono"
  )

  cap_parts <- "\u25c6 = last step  \u00b7  \u25cb = origin"
  if (!is.null(marker_every) && marker_every > 0)
    cap_parts <- paste0(cap_parts, "  \u00b7  \u25cf = every ", marker_every, " steps")

  p <- p +
    theme_void(base_family = "sans") +
    theme(
      plot.background   = element_rect(fill = bg_col,      color = NA),
      panel.background  = element_rect(fill = bg_col,      color = NA),
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.title      = element_blank(),
      legend.text       = element_text(color = subtext_col, size = 11),
      legend.key.width  = unit(2,   "lines"),
      legend.key.height = unit(0.5, "lines"),
      legend.spacing.x  = unit(0.8, "lines"),
      plot.title        = element_text(color = text_col,    size = 18,
                                       hjust = 0.5, margin = margin(b = 6)),
      plot.subtitle     = element_text(color = subtext_col, size = 9.5,
                                       hjust = 0.5, margin = margin(b = 14)),
      plot.caption      = element_text(color = subtext_col, size = 8,
                                       hjust = 0.5, margin = margin(t = 10)),
      plot.margin       = margin(30, 30, 20, 30)
    ) +
    labs(title = title, subtitle = subtitle, caption = cap_parts)

  # ── 10. Save ──────────────────────────────────────────────
  #if (!is.null(save_to)) {
      if (FALSE) {
    ggplot2::ggsave(
      filename = save_to, plot = p,
      width = 10, height = 10, dpi = 200, bg = bg_col
    )
    if (verbose) cat(sprintf("Plot saved \u2192 %s\n", save_to))
  }

  #invisible(p)
  p
}


# ============================================================
#  DEMO
# ============================================================

set.seed(42)
n      <- 52
weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
season <- sin(seq(0, 2 * pi, length.out = n))

gw_data <- tibble(
  week        = weeks,
  `Station A` = 8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.18)),
  `Station B` = 7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.22)),
  `Station C` = 9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.15)),
  `Station D` = 8) |> 
  mutate(`Station E` = 8 + row_number() * 0.01) |> 
  mutate(`Station F` = 8 - row_number() * 0.01)
  

plot_bouquet(
  data         = gw_data,
  time_col     = week,
  title        = "Groundwater Level Fluctuations \u2014 2023",
  colors       = c("#4fc3f7", "#81c784", "#ffb74d"),
  ceiling_pct  = 0.95,
  launch_deg   = 90,
  marker_every = 13,          # quarterly dots
  show_rings   = TRUE,
  dark_mode    = TRUE,
  save_to      = "/mnt/user-data/outputs/bouquet_timeseries.png",
  verbose      = TRUE
)
