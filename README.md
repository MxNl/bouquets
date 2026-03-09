# bouquets <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
<!-- Replace YOUR_GITHUB_USERNAME below with your actual GitHub username, then
     add the file .github/workflows/R-CMD-check.yaml to your repo (included in
     this package skeleton) to activate the CI badge. -->
[![R-CMD-check](https://github.com/MxNl/bouquets/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MxNl/bouquets/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![pkgdown](https://github.com/YOU/bouquets/actions/workflows/pkgdown.yaml/badge.svg)](https://YOU.github.io/bouquets/)
<!-- badges: end -->

**bouquets** creates angular accumulation plots for collections of time
series. Each series is encoded as a turtle-graphics path that turns **left**
on increases and **right** on decreases — all series share the same origin
and turning angle, so paths that look alike come from series with similar
directional dynamics, regardless of their absolute values.

The technique is structurally related to DNA walk visualisations (Gates 1986;
Yau et al. 2003).

---

## Installation

```r
# Development version from GitHub
# install.packages("remotes")
remotes::install_github("MxNl/bouquets")
```

Optional dependencies that unlock extra features:

| Package | Feature |
|---|---|
| `patchwork` | Location map panel (`lon_col` + `lat_col`) |
| `maps` | Map background (required for map panel) |
| `mapdata` | Sub-national boundaries on the map |
| `sf` | Reprojection of non-WGS84 coordinates |

---

## Basic usage

```r
library(bouquets)

set.seed(42)
n      <- 52L
weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
season <- sin(seq(0, 2 * pi, length.out = n))

gw_long <- tibble::tibble(
  week    = rep(weeks, 3L),
  station = rep(c("Station A", "Station B", "Station C"), each = n),
  region  = rep(c("North", "North", "South"), each = n),
  level_m = c(
    8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.18)),
    7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.22)),
    9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.15))
  )
)

make_plot_bouquet(gw_long,
  time_col   = week,
  series_col = station,
  value_col  = level_m
)
```

---

## Key features

### Four colour modes

Both `stem_colors` (path lines) and `flower_colors` (✿ end markers) accept:

```r
# 1. Single hex colour
make_plot_bouquet(gw_long, ..., stem_colors = "#2d6a9f")

# 2. Named keyword palette
make_plot_bouquet(gw_long, ..., stem_colors = "greens", flower_colors = "blossom")

# 3. Vector of hex colours (one per series)
make_plot_bouquet(gw_long, ..., stem_colors = c("#c0392b", "#2980b9", "#27ae60"))

# 4. Bare column name — colours derived from that column's values
make_plot_bouquet(gw_long, ..., stem_colors = region, flower_colors = region)
```

### Faceting

```r
make_plot_bouquet(gw_long,
  time_col      = week,
  series_col    = station,
  value_col     = level_m,
  stem_colors   = region,
  flower_colors = region,
  facet_by      = region,
  title         = "Groundwater dynamics by region"
)
```

### Clustering

`cluster_bouquet()` groups series by the similarity of their directional
sequences and appends a `cluster` column that plugs directly into
`make_plot_bouquet()`:

```r
gw_long |>
  cluster_bouquet(
    time_col   = week,
    series_col = station,
    value_col  = level_m
  ) |>
  make_plot_bouquet(
    time_col      = week,
    series_col    = station,
    value_col     = level_m,
    stem_colors   = cluster,
    flower_colors = cluster,
    facet_by      = cluster
  )
```

Five clustering methods are available (`"pca_hclust"`, `"pca_kmeans"`,
`"hclust"`, `"kmeans"`, `"pam"`) with three distance metrics
(`"euclidean"`, `"correlation"`, `"manhattan"`). The optimal number of
clusters is selected automatically via a composite silhouette score, or you
can specify `k` directly.

### Location map panel

Supply coordinate columns to attach a location map alongside the bouquet.
Any projected CRS is handled automatically with `coord_crs`:

```r
make_plot_bouquet(gw_long_with_coords,
  time_col   = week,
  series_col = station,
  value_col  = level_m,
  lon_col    = easting,
  lat_col    = northing,
  coord_crs  = 3035,      # ETRS89-LAEA; omit for WGS84 decimal degrees
  map_width  = 0.35
)
```

### Visual options

```r
make_plot_bouquet(gw_long, ...,
  show_rings    = TRUE,   # concentric reference rings
  marker_every  = 13L,    # dot every N steps
  dark_mode     = TRUE,   # dark navy background
  launch_deg    = 45,     # initial heading (default 90° = north)
  ceiling_pct   = 0.75    # tightness of turns (default 0.80)
)
```

---

## How it works

For each series, the signed first difference is binarised to +1 (increase),
−1 (decrease), or 0 (no change). The heading at step *i* is:

$$h_i = h_{\text{launch}} + \sum_{k=2}^{i} d_k \cdot \theta$$

The angle θ is derived so that even the most volatile series in the dataset
never completes a full loop:

$$\theta = \frac{360°}{\max_s(\max C_s - \min C_s)} \times \texttt{ceiling\_pct}$$

where *C*ₛ is the cumulative sum of signed steps for series *s*. All
coordinates follow as a vectorised `cumsum(cos(heading))` / `cumsum(sin(heading))`.

---

## Citation

If you use bouquets in a publication, please cite it as:

```
Last, F. (2025). bouquets: Angular accumulation plots for time series.
R package version 0.1.0. https://github.com/MxNl/bouquets
```

---

## References

Gates, M. A. (1986). A simple way to look at DNA. *Journal of Theoretical
Biology*, 119(3), 319–328.

Yau, S. S.-T., et al. (2003). DNA sequence representation without degeneracy.
*Nucleic Acids Research*, 31(12), 3078–3080.