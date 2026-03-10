# -- Utility functions ----------------------------------------------------------

#' Save a bouquet plot to a file
#'
#' A thin wrapper around [ggplot2::ggsave()] with sensible defaults for the
#' square-ish aspect ratio typical of bouquet plots, so you don't have to
#' remember the right `width`/`height` each time.
#'
#' @param plot A ggplot or patchwork object returned by [make_plot_bouquet()].
#'   Defaults to the last plot printed (`ggplot2::last_plot()`).
#' @param file Path to the output file. The format is inferred from the
#'   extension (`.png`, `.pdf`, `.svg`, etc.). Default `"bouquet.png"`.
#' @param width Width in `units`. Default `10`.
#' @param height Height in `units`. Default `8`.
#' @param units One of `"in"`, `"cm"`, `"mm"`, or `"px"`. Default `"in"`.
#' @param dpi Resolution in dots per inch. Used for raster formats (PNG,
#'   JPEG, TIFF). Default `300`.
#' @param ... Additional arguments forwarded to [ggplot2::ggsave()].
#'
#' @return The `file` path, invisibly.
#'
#' @seealso [make_plot_bouquet()] which produces the plots saved by this
#'   function. [make_plot_bouquet_interactive()] for an interactive plotly
#'   version that can be exported with `htmlwidgets::saveWidget()`.
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' n   <- 52L
#' gw  <- tibble::tibble(
#'   week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 3L),
#'   station = rep(c("A", "B", "C"), each = n),
#'   level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)))
#' )
#' p <- make_plot_bouquet(gw, week, station, level)
#' tmp <- tempfile(fileext = ".png")
#' save_bouquet(p, tmp)
#' }
#'
#' @export
#' @importFrom ggplot2 ggsave last_plot
save_bouquet <- function(plot  = ggplot2::last_plot(),
                         file  = "bouquet.png",
                         width = 10,
                         height = 8,
                         units  = "in",
                         dpi    = 300,
                         ...) {
  if (!is.character(file) || length(file) != 1L || !nzchar(file))
    stop("`file` must be a non-empty file path string.", call. = FALSE)
  if (!is.numeric(width)  || width  <= 0) stop("`width` must be positive.",  call. = FALSE)
  if (!is.numeric(height) || height <= 0) stop("`height` must be positive.", call. = FALSE)

  ggplot2::ggsave(
    filename = file,
    plot     = plot,
    width    = width,
    height   = height,
    units    = units,
    dpi      = dpi,
    ...
  )
  message(sprintf("Saved bouquet plot to: %s", normalizePath(file, mustWork = FALSE)))
  invisible(file)
}


#' Interactive Angular Accumulation Plot (plotly hover tooltips)
#'
#' Builds the same turtle-graphics bouquet as [make_plot_bouquet()] and
#' converts it to an interactive plotly figure. Hovering over any point on a
#' path shows the series name, date/time, step number, and value.
#'
#' This function requires the \pkg{plotly} package.
#'
#' @param data A long-format data frame -- same format as [make_plot_bouquet()].
#' @param time_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Time column.
#'   Default: first column.
#' @param series_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Series
#'   identifier column. Default: second column.
#' @param value_col <[`tidy-select`][dplyr::dplyr_tidy_select]> Numeric value
#'   column. Default: third column.
#' @param stem_colors Single hex string or a named character vector (one hex
#'   per series name). Default `"#3a7d2c"`.
#' @param flower_colors Single hex string or a named character vector for the
#'   end-of-series markers. Default `"#f472b6"`.
#' @param ceiling_pct Fraction of maximum angle. Default `0.80`.
#' @param launch_deg Launch heading in degrees. Default `90`.
#' @param dark_mode Logical. Default `FALSE`.
#' @param title Plot title string. Default `NULL`.
#' @param from Optional time lower bound for filtering. Default `NULL`.
#' @param to Optional time upper bound for filtering. Default `NULL`.
#' @param normalise Logical. Per-series angle normalisation. Default `FALSE`.
#' @param verbose Logical. Print angle diagnostics. Default `FALSE`.
#'
#' @return A `plotly` htmlwidget object.
#'
#' @seealso [make_plot_bouquet()] for the static ggplot2 version with full
#'   styling options. [save_bouquet()] to export the static version to a file.
#'   [cluster_bouquet()] to colour-code series by directional cluster before
#'   plotting.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("plotly", quietly = TRUE)) {
#' set.seed(42)
#' n   <- 52L
#' gw  <- tibble::tibble(
#'   week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 3L),
#'   station = rep(c("A", "B", "C"), each = n),
#'   level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)))
#' )
#' make_plot_bouquet_interactive(gw, "week", "station", "level")
#' }
#' }
#'
#' @export
#' @importFrom dplyr group_by mutate rename select ungroup
make_plot_bouquet_interactive <- function(
    data,
    time_col      = 1,
    series_col    = 2,
    value_col     = 3,
    stem_colors   = "#3a7d2c",
    flower_colors = "#f472b6",
    ceiling_pct   = 0.80,
    launch_deg    = 90,
    dark_mode     = FALSE,
    title         = NULL,
    from          = NULL,
    to            = NULL,
    normalise     = FALSE,
    verbose       = FALSE) {

  if (!requireNamespace("plotly", quietly = TRUE))
    stop('make_plot_bouquet_interactive() requires the plotly package.\n',
         'Install it with: install.packages("plotly")', call. = FALSE)

  if (!is.data.frame(data))
    stop("`data` must be a data frame or tibble.", call. = FALSE)

  # Resolve column names. Bare names, strings, and integer positions all work.
  time_col_name   <- names(dplyr::select(data, {{ time_col }}))[1L]
  series_col_name <- names(dplyr::select(data, {{ series_col }}))[1L]
  value_col_name  <- names(dplyr::select(data, {{ value_col }}))[1L]

  # -- Time window filter --------------------------------------------------------
  if (!is.null(from)) data <- data[data[[time_col_name]] >= from, , drop = FALSE]
  if (!is.null(to))   data <- data[data[[time_col_name]] <= to,   , drop = FALSE]
  if (nrow(data) == 0L)
    stop("No data remains after applying `from`/`to` filters.", call. = FALSE)

  # -- Build paths ---------------------------------------------------------------
  bq           <- .build_bouquet_paths(data, time_col_name, series_col_name,
                                        value_col_name, ceiling_pct,
                                        launch_deg, verbose, normalise)
  path_data    <- bq$path_data
  series_names <- bq$series_names

  # NOTE: path_data already carries bq_t and bq_v (renamed internally by
  # .build_bouquet_paths from the original time/value columns), so no
  # additional join is needed for the tooltip columns.

  # -- Resolve colours -----------------------------------------------------------
  n_series <- length(series_names)

  resolve_flat <- function(col_val, default_col) {
    if (is.character(col_val)) {
      if (length(col_val) == 1L)
        stats::setNames(rep(col_val, n_series), series_names)
      else
        stats::setNames(rep_len(col_val, n_series), series_names)
    } else {
      stats::setNames(rep(default_col, n_series), series_names)
    }
  }

  stem_cols   <- resolve_flat(stem_colors,   "#3a7d2c")
  flower_cols <- resolve_flat(flower_colors, "#f472b6")

  # -- Theme ---------------------------------------------------------------------
  bg_col  <- if (dark_mode) "#1a1a2e" else "#f8f8f5"
  ax_col  <- if (dark_mode) "#2a2a3e" else "#e0e0e0"
  txt_col <- if (dark_mode) "white"   else "#1a1a2e"

  # -- Build plotly traces -------------------------------------------------------
  fig <- plotly::plot_ly()

  for (sn in series_names) {
    df_s <- path_data[as.character(path_data$series) == sn, ]

    # Line trace
    fig <- plotly::add_trace(
      fig,
      data       = df_s,
      x          = ~x,
      y          = ~y,
      type       = "scatter",
      mode       = "lines",
      name       = sn,
      line       = list(color = stem_cols[[sn]], width = 2),
      text       = ~paste0("<b>", series, "</b><br>",
                            "Step : ", step, "<br>",
                            "Date : ", bq_t, "<br>",
                            "Value: ", round(bq_v, 3L)),
      hoverinfo  = "text"
    )

    # Flower endpoint
    end_row <- df_s[nrow(df_s), ]
    fig <- plotly::add_trace(
      fig,
      x         = end_row$x,
      y         = end_row$y,
      type      = "scatter",
      mode      = "markers",
      name      = sn,
      marker    = list(color = flower_cols[[sn]], size = 10, symbol = "star"),
      text      = paste0("<b>", sn, "</b><br>",
                         "End  : ", end_row$bq_t, "<br>",
                         "Value: ", round(end_row$bq_v, 3L)),
      hoverinfo = "text",
      showlegend = FALSE
    )
  }

  # Origin marker
  fig <- plotly::add_trace(
    fig, x = 0, y = 0, type = "scatter", mode = "markers",
    marker    = list(color = if (dark_mode) "white" else "#1a1a2e",
                     size = 8, symbol = "circle-open"),
    name      = "origin",
    hoverinfo = "text",
    text      = "Origin",
    showlegend = FALSE
  )

  # -- Layout --------------------------------------------------------------------
  fig <- plotly::layout(
    fig,
    title      = list(text = title, font = list(color = txt_col)),
    paper_bgcolor = bg_col,
    plot_bgcolor  = bg_col,
    xaxis = list(scaleanchor = "y", scaleratio = 1,
                 showgrid = FALSE, zeroline = FALSE,
                 showticklabels = FALSE, color = ax_col),
    yaxis = list(showgrid = FALSE, zeroline = FALSE,
                 showticklabels = FALSE, color = ax_col),
    legend = list(font = list(color = txt_col),
                  bgcolor = bg_col)
  )

  fig
}