# Angular Accumulation (Flower Bouquet) Plot for Time Series

Draws each time series as a turtle-graphics path from a shared origin,
encoding the binary direction of consecutive changes as fixed-angle
turns: an increase turns the heading **left** by \\\theta\\ and a
decrease turns it **right** by \\\theta\\. No change continues straight
ahead. Because all series share the same origin and angle, the visual
similarity of paths directly reflects the similarity of fluctuation
patterns across series.

The rotation angle \\\theta\\ is derived automatically from the data.
The binding constraint is the series whose cumulative heading sweeps the
widest arc (measured as `max(cumsum) - min(cumsum)` of the signed
steps). \\\theta\\ is set to `ceiling_pct` of the largest angle that
would not cause that series to complete a full 360° loop.

## Usage

``` r
make_plot_bouquet(
  data,
  time_col = 1,
  series_col = 2,
  value_col = 3,
  stem_colors = "#3a7d2c",
  flower_colors = "#f472b6",
  facet_by = NULL,
  ceiling_pct = 0.8,
  launch_deg = 90,
  marker_every = NULL,
  show_rings = FALSE,
  dark_mode = FALSE,
  title = NULL,
  subtitle = NULL,
  hide_legend_after = 10L,
  lon_col = NULL,
  lat_col = NULL,
  map_width = 0.35,
  coord_crs = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A long-format data frame or tibble with at minimum a time column, a
  series identifier column, and a numeric value column. Additional
  factor or character columns may be referenced by `stem_colors`,
  `flower_colors`, and `facet_by`.

- time_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The time index column. Defaults to the first column.

- series_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The column identifying each individual time series (e.g. a station
  name). Defaults to the second column.

- value_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The numeric column containing the measured values. Defaults to the
  third column.

- stem_colors:

  Controls the colour of the time series lines. Four options are
  accepted:

  - A single hex string — all lines share that colour. Default:
    `"#3a7d2c"` (plant green).

  - A character vector of hex strings, one per series — matched by
    position; recycled with a warning if too short.

  - `"greens"` — each series receives a distinct shade from a curated
    palette of natural greens.

  - A bare column name referencing a factor or character column in
    `data` — each series is coloured by that column's value.

- flower_colors:

  Controls the colour of the end-of-series ✿ symbols. Four options are
  accepted:

  - A single hex string — all flowers share that colour. Default:
    `"#f472b6"` (warm pink).

  - A character vector of hex strings, one per series — matched by
    position; recycled with a warning if too short.

  - `"blossom"` — each flower is assigned a colour from a curated
    palette of pinks, mauves, and soft corals.

  - A bare column name referencing a factor or character column in
    `data` — each flower is coloured by that column's value.

- facet_by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  An optional bare column name referencing a factor or character column
  in `data`. When supplied the plot is split into facets via
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
  one panel per level. `NULL` (default) produces a single panel.

- ceiling_pct:

  A single number in `(0, 1]`. The fraction of the theoretical maximum
  angle to use as \\\theta\\. Lower values give more visual headroom;
  higher values make turns more pronounced. Default `0.80`.

- launch_deg:

  A single number. The initial heading of all series in degrees,
  measured counter-clockwise from the positive x-axis. `90` points
  straight up (north). Defaults to `90`.

- marker_every:

  A positive integer. If supplied, a filled circle is plotted at every
  `marker_every`-th step to aid reading of temporal progress. `NULL`
  (default) suppresses markers.

- show_rings:

  Logical. Whether to draw faint concentric reference rings centred on
  the origin. Defaults to `FALSE`.

- dark_mode:

  Logical. `TRUE` uses a dark navy background; `FALSE` (default) uses a
  light off-white background.

- title:

  A string used as the plot title. `NULL` (default) = no title.

- subtitle:

  A string used as the subtitle. `NULL` (default) auto-generates a
  subtitle reporting \\\theta\\, the binding series, time range, and
  interval.

- hide_legend_after:

  A single positive integer. The legend is suppressed when the number of
  series is greater than or equal to this value. Defaults to `10`.

- lon_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional bare column name containing the longitude of each series
  location (decimal degrees, WGS 84). Must be supplied together with
  `lat_col`. When both are provided a location map is attached to the
  right of the bouquet plot. `NULL` (default) = no map.

- lat_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optional bare column name containing the latitude. See `lon_col`.

- map_width:

  Relative width of the location map panel as a fraction of the total
  combined width. A value of `0.35` means the map takes 35 \\ the
  horizontal space and the bouquet takes 65 \\ and `lat_col` are both
  provided. Defaults to `0.35`.

- coord_crs:

  Integer or `NULL`. The EPSG code of the coordinate reference system
  used by `lon_col` and `lat_col`. When `NULL` (default) the function
  attempts to auto-detect the CRS from the coordinate magnitude. Supply
  an explicit code (e.g. `coord_crs = 3035` for ETRS89-LAEA,
  `coord_crs = 25832` for ETRS89-UTM32) whenever auto-detection gives
  wrong results. Decimal-degree coordinates (WGS84 / EPSG:4326) require
  no reprojection and this argument is ignored.

- verbose:

  Logical. Whether to print angle diagnostics to the console. Defaults
  to `TRUE`.

## Value

When `lon_col` and `lat_col` are `NULL` (default), a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object. When both coordinate columns are supplied, a
[patchwork](https://patchwork.data-imaginist.com/reference/patchwork-package.html)
object combining the bouquet panel (left) and a location map (right);
patchwork objects behave like ggplot objects for
[`print()`](https://rdrr.io/r/base/print.html), `ggsave()`, and `+`.

## Details

### Input format

`data` must be in **long** format: one row per time step per series.
Series should share the same time index values and have equal length.

### Algorithm

For each series, the signed first difference is binarised to \\+1\\
(increase), \\-1\\ (decrease), or \\0\\ (no change). The heading at step
\\i\\ is accumulated as:

\$\$h_i = h\_{\text{launch}} + \sum\_{k=2}^{i} d_k \cdot \theta\$\$

which is computed in one vectorised pass via
[`base::cumsum()`](https://rdrr.io/r/base/cumsum.html). The Cartesian
coordinates follow as: \\x_i = \sum\_{k=2}^{i} \cos(h_k)\\, and
similarly for \\y\\.

### Angle derivation

A complete loop occurs when the heading sweeps \\\geq 360°\\. The sweep
equals \\(\max C - \min C) \cdot \theta\\, where \\C\\ is the cumulative
sum of the signed steps. The angle is therefore:

\$\$\theta = \frac{360°}{\max_s(\max C_s - \min C_s)} \times
\texttt{ceiling\\pct}\$\$

Note that `max(abs(C))` is **not** the correct constraint: a series that
dips to \\-5\\ then climbs to \\+20\\ sweeps 25 steps' worth of angle
even though neither extreme alone reaches 25.

## See also

The technique is structurally related to **DNA walk** visualisations
(Gates 1986; Yau et al. 2003), which encode nucleotide sequences as 2-D
paths using similar binary-direction rules.

## Examples

``` r
set.seed(42)
n      <- 52
weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
season <- sin(seq(0, 2 * pi, length.out = n))

gw_long <- tibble::tibble(
  week    = rep(weeks, 3),
  station = rep(c("Station A", "Station B", "Station C"), each = n),
  region  = rep(c("North", "North", "South"), each = n),
  level_m = c(
    8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.18)),
    7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.22)),
    9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.15))
  )
)

# Minimal call
make_plot_bouquet(gw_long,
  time_col = week, series_col = station, value_col = level_m)
#> === Heading sweep per series (max - min of cumulative net steps) ===
#> # A tibble: 3 × 2
#>   bq_s      cum_net_range
#>   <chr>             <int>
#> 1 Station A            13
#> 2 Station B            10
#> 3 Station C            26
#> 
#> Binding series : Station C (sweep = 26 steps)
#> Ceiling angle  : 13.85°
#> theta (80%)  : 11.08°
#> 


# Column-driven colours and faceting
make_plot_bouquet(
  data          = gw_long,
  time_col      = week,
  series_col    = station,
  value_col     = level_m,
  stem_colors   = region,
  flower_colors = region,
  facet_by      = region,
  title         = "Groundwater Level Fluctuations \u2014 2023"
)
#> === Heading sweep per series (max - min of cumulative net steps) ===
#> # A tibble: 3 × 2
#>   bq_s      cum_net_range
#>   <chr>             <int>
#> 1 Station A            13
#> 2 Station B            10
#> 3 Station C            26
#> 
#> Binding series : Station C (sweep = 26 steps)
#> Ceiling angle  : 13.85°
#> theta (80%)  : 11.08°
#> 

```
