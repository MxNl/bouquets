# Cluster Time Series by Directional Dynamics

Encodes each time series as a feature vector and clusters the series
based on similarity. The feature representation is controlled by
`method` and `normalise`, and should match the value used in
[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
so that cluster assignments align visually with the rendered paths. The
resulting `cluster` column can be passed directly to
[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
via `stem_colors`, `flower_colors`, `facet_by`, or `cluster_hull`.

The returned object is both the original `data` tibble (with an added
cluster column) **and** a `cluster_bouquet` S3 object. Call
[`summary.cluster_bouquet()`](https://mxnl.github.io/bouquets/reference/summary.cluster_bouquet.md)
on it to print a structured report of cluster quality, sizes, and member
series.

## Usage

``` r
cluster_bouquet(
  data,
  time_col = 1,
  series_col = 2,
  value_col = 3,
  k = "auto",
  k_max = 8L,
  method = c("coords_hclust", "coords_kmeans", "coords_pam", "heading_hclust",
    "heading_kmeans", "heading_pam", "area_hclust", "area_pam"),
  resolution = 0.5,
  cluster_col = "cluster",
  normalise = FALSE,
  ceiling_pct = 0.8,
  launch_deg = 90,
  seed = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  A long-format data frame or tibble – the same format expected by
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md).

- time_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The time index column. Defaults to the first column.

- series_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The column identifying each individual time series. Defaults to the
  second column.

- value_col:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  The numeric value column. Defaults to the third column.

- k:

  Either a positive integer specifying the number of clusters, or
  `"auto"` (default) to select the optimal number by maximising the
  composite silhouette score across `k = 2, ..., k_max`.

- k_max:

  A single positive integer. Upper bound considered when `k = "auto"`.
  Capped at `n_series - 1`. Defaults to `8L`.

- method:

  The clustering algorithm and feature representation. One of:

  `"coords_hclust"`

  :   (Default) Clusters on the actual (x, y) path coordinates. Paths
      that look similar in the plot cluster together. Uses Ward's D2
      hierarchical clustering. Deterministic.

  `"coords_kmeans"`

  :   As `"coords_hclust"` but k-means. Set `seed` for reproducibility.

  `"coords_pam"`

  :   As `"coords_hclust"` but Partitioning Around Medoids. More robust
      to outliers. Requires cluster.

  `"heading_hclust"`

  :   Clusters on the cumulative heading angle sequence. Captures
      turning patterns independently of absolute position;
      rotation-invariant. Uses Ward's D2.

  `"heading_kmeans"`

  :   As `"heading_hclust"` but k-means.

  `"heading_pam"`

  :   As `"heading_hclust"` but PAM. Requires cluster.

  `"area_hclust"`

  :   Pairwise area distance: the area of the polygon enclosed by two
      paths from the shared origin, computed via the cross-product sum
      \\\frac{1}{2}\|\sum_i (x^{(1)}\_i y^{(2)}\_i - x^{(2)}\_i
      y^{(1)}\_i)\|\\. Paths spatially close and similarly shaped get
      small distances. Uses Ward's D2.

  `"area_pam"`

  :   As `"area_hclust"` but PAM. Requires cluster.

- resolution:

  A non-negative number. Composite k-selection score: \\\bar{s}(k)
  \times \min_c \bar{s}\_c(k) \times k^{\text{resolution}}\\. Higher
  values favour finer clusters. `0` = plain mean silhouette. Default
  `0.5`.

- cluster_col:

  String. Name of the cluster column added to `data`. Default
  `"cluster"`.

- normalise:

  Logical. Controls path building for clustering. Should match the value
  passed to
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
  so that cluster assignments align visually with the rendered paths.
  `FALSE` (default) uses a single global \\\theta\\; `TRUE` normalises
  per series. See
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
  for full details.

- ceiling_pct:

  A single number in `(0, 1]`. Passed to path building for path-geometry
  methods. Should match the value used in
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md).
  Default `0.80`.

- launch_deg:

  Initial heading in degrees for path building. Only used by
  path-geometry methods. Should match
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md).
  Default `90`.

- seed:

  Integer or `NULL`. Random seed passed to
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html) before
  k-means initialisation. Ensures reproducible results for
  `"coords_kmeans"` and `"heading_kmeans"`. Default `NULL` = no seed
  (non-reproducible).

- verbose:

  Logical. Print k selection table, chosen k, silhouette score, and
  cluster sizes. Default `FALSE`.

## Value

The original `data` tibble with one additional factor column (named by
`cluster_col`, levels `"C1"`, `"C2"`, ... ordered by decreasing size)
plus a `cluster_bouquet` S3 class. The object also carries a `bq_meta`
attribute containing all clustering diagnostics; use
[`summary.cluster_bouquet()`](https://mxnl.github.io/bouquets/reference/summary.cluster_bouquet.md)
to print them.

## Details

### Feature representations

**Path coordinates** (`"coords_*"`): each series is encoded as
\\2(n-1)\\ values \\(x_1, \ldots, x\_{n-1}, y_1, \ldots, y\_{n-1})\\.
Directly reflects the visual plot – paths close together cluster
together. Most intuitive for visual interpretation. This is the default.

**Heading sequence** (`"heading_*"`): cumulative heading angle in
degrees at each step. Captures the turning pattern independently of
absolute position; rotation-invariant.

**Area distance** (`"area_*"`): pairwise distance is the area of the
polygon enclosed by two paths (shoelace formula). Captures both shape
difference and spatial separation in a single scalar with a clean
geometric interpretation.

### Composite k-selection

Plain mean silhouette consistently selects k = 2. The composite score
adds a worst-cluster penalty and a resolution exponent to favour finer
structure.

### Pairing with [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)

Pass the same `normalise`, `ceiling_pct`, and `launch_deg` to both
functions. When a `cluster_bouquet` object is piped into
[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md),
`normalise` is inherited automatically.

## See also

[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
for visualising the result.
[`summary.cluster_bouquet()`](https://mxnl.github.io/bouquets/reference/summary.cluster_bouquet.md)
for a structured quality report.
[`plot_cluster_quality()`](https://mxnl.github.io/bouquets/reference/plot_cluster_quality.md)
to plot silhouette scores across k.

## Examples

``` r
set.seed(42)
n      <- 52L
weeks  <- seq(as.Date("2023-01-01"), by = "week", length.out = n)
season <- sin(seq(0, 2 * pi, length.out = n))

gw_long <- tibble::tibble(
  week    = rep(weeks, 6L),
  station = rep(paste0("S", 1:6), each = n),
  level_m = c(
    8.5 + 0.8 * season + cumsum(rnorm(n,  0.00, 0.2)),
    8.3 + 0.7 * season + cumsum(rnorm(n,  0.01, 0.2)),
    7.2 + 0.5 * season + cumsum(rnorm(n,  0.02, 0.2)),
    7.0 + 0.6 * season + cumsum(rnorm(n,  0.00, 0.2)),
    9.1 + 1.1 * season + cumsum(rnorm(n, -0.01, 0.2)),
    9.3 + 1.0 * season + cumsum(rnorm(n, -0.02, 0.2))
  )
)

# Default: path-coordinate clustering (coords_hclust), auto k
clustered <- cluster_bouquet(gw_long,
  time_col = week, series_col = station, value_col = level_m)
summary(clustered)
#> -- cluster_bouquet summary ----------------------------------------------
#>   Method    : coords_hclust
#>   Normalise : FALSE
#>   Seed      : none
#>   Series    : 6   k = 3   Resolution = 0.50
#>   Mean silhouette : 0.364  (reasonable structure)
#> 
#>   Auto k selection (composite silhouette score):
#>     k = 2 : 0.2077
#>     k = 3 : 0.3641  <-- selected
#>     k = 4 : 0.2998
#>     k = 5 : 0.1471
#> 
#>   Cluster sizes and members:
#>     C1  (3 series, mean sil = 0.399)
#>        S1, S4, S5
#>     C2  (2 series, mean sil = 0.493)
#>        S2, S6
#>     C3  (1 series, mean sil = 0.000)
#>        S3
#> 
#>   Per-series silhouette widths:
#>     S5                    C1   0.553  |||||||||||
#>     S1                    C1   0.375  ||||||||
#>     S4                    C1   0.269  |||||
#>     S2                    C2   0.590  ||||||||||||
#>     S6                    C2   0.396  ||||||||
#>     S3                    C3   0.000  
#> ------------------------------------------------------------------------

# Path-coordinate clustering -- most visually intuitive
# \donttest{
cluster_bouquet(gw_long,
  time_col = week, series_col = station, value_col = level_m,
  method = "coords_hclust")
#> # A tibble: 312 × 4
#>    week       station level_m cluster
#>    <date>     <chr>     <dbl> <fct>  
#>  1 2023-01-01 S1         8.77 C1     
#>  2 2023-01-08 S1         8.76 C1     
#>  3 2023-01-15 S1         8.93 C1     
#>  4 2023-01-22 S1         9.15 C1     
#>  5 2023-01-29 S1         9.32 C1     
#>  6 2023-02-05 S1         9.38 C1     
#>  7 2023-02-12 S1         9.76 C1     
#>  8 2023-02-19 S1         9.81 C1     
#>  9 2023-02-26 S1        10.3  C1     
#> 10 2023-03-05 S1        10.3  C1     
#> # ℹ 302 more rows

# Area-based clustering
cluster_bouquet(gw_long,
  time_col = week, series_col = station, value_col = level_m,
  method = "area_hclust")
#> # A tibble: 312 × 4
#>    week       station level_m cluster
#>    <date>     <chr>     <dbl> <fct>  
#>  1 2023-01-01 S1         8.77 C1     
#>  2 2023-01-08 S1         8.76 C1     
#>  3 2023-01-15 S1         8.93 C1     
#>  4 2023-01-22 S1         9.15 C1     
#>  5 2023-01-29 S1         9.32 C1     
#>  6 2023-02-05 S1         9.38 C1     
#>  7 2023-02-12 S1         9.76 C1     
#>  8 2023-02-19 S1         9.81 C1     
#>  9 2023-02-26 S1        10.3  C1     
#> 10 2023-03-05 S1        10.3  C1     
#> # ℹ 302 more rows
# }
```
