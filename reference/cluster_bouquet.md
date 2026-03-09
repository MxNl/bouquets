# Cluster Time Series by Directional Dynamics

Encodes each time series as a sequence of signed directional steps
(\\+1\\ increase, \\-1\\ decrease, \\0\\ no change) and clusters the
series based on the similarity of those sequences. The resulting
`cluster` column can be passed directly to
[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
via `stem_colors`, `flower_colors`, or `facet_by`.

## Usage

``` r
cluster_bouquet(
  data,
  time_col = 1,
  series_col = 2,
  value_col = 3,
  k = "auto",
  k_max = 8L,
  method = c("pca_hclust", "pca_kmeans", "hclust", "kmeans", "pam"),
  distance = c("euclidean", "correlation", "manhattan"),
  resolution = 0.5,
  pca_variance = 0.9,
  cluster_col = "cluster",
  verbose = TRUE
)
```

## Arguments

- data:

  A long-format data frame or tibble — the same format expected by
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
  composite silhouette score across `k = 2, …, k_max`.

- k_max:

  A single positive integer. The upper bound considered when
  `k = "auto"`. Capped internally at `n_series - 1`. Defaults to `8L`.

- method:

  The clustering algorithm. One of:

  `"pca_hclust"`

  :   (Default) Projects direction sequences onto their principal
      components retaining `pca_variance` of variance, then applies
      agglomerative hierarchical clustering with Ward's D2 linkage in
      that space. The PCA step removes noise dimensions that dilute
      cluster separation in long series. Deterministic; no extra
      packages required. The `distance` argument does not apply —
      distance is always Euclidean in PCA space.

  `"pca_kmeans"`

  :   PCA projection followed by k-means. Faster for large numbers of
      series; non-deterministic (set a seed for reproducibility). The
      `distance` argument does not apply.

  `"hclust"`

  :   Hierarchical clustering with Ward's D2 directly on the direction
      sequences, using the `distance` metric. Best for short series (\<
      ~50 steps) where the raw sequence distance is meaningful.

  `"kmeans"`

  :   k-means directly on the direction sequences. The `distance`
      argument does not apply (k-means always uses Euclidean
      internally). Suitable for short series with many observations.

  `"pam"`

  :   Partitioning Around Medoids using the `distance` metric. More
      robust to outlier series than k-means because cluster centres are
      always real observations. Requires the cluster package.

- distance:

  The distance metric applied to the raw direction sequences. One of:

  `"euclidean"`

  :   (Default) Penalises step-by-step disagreements between sequences.

  `"correlation"`

  :   Converts Pearson correlation to a distance as \\1 - r\\, measuring
      whether series *co-move* in their ups and downs regardless of
      their absolute patterns. Often the most meaningful metric for
      hydrological series.

  `"manhattan"`

  :   Sum of absolute step differences. Less sensitive to occasional
      large divergences than Euclidean.

  **Note**: only applied by `"hclust"` and `"pam"`. The PCA methods
  always use Euclidean distance in the reduced space; k-means has no
  distance argument by design.

- resolution:

  A non-negative number controlling the preference for more vs fewer
  clusters during automatic k selection. The composite score is:

  \$\$\text{score}(k) = \bar{s}(k) \times \min_c \bar{s}\_c(k) \times
  k^{\text{resolution}}\$\$

  where \\\bar{s}(k)\\ is the overall mean silhouette width and \\\min_c
  \bar{s}\_c(k)\\ is the mean silhouette of the *worst* cluster.
  Increasing `resolution` rewards finer groupings as long as all
  clusters remain well-defined. `resolution = 0` falls back to plain
  mean silhouette. Defaults to `0.5`.

- pca_variance:

  A number in `(0, 1]`. The cumulative proportion of variance to retain
  during PCA compression. Only used by `"pca_hclust"` and
  `"pca_kmeans"`. Defaults to `0.90`.

- cluster_col:

  A string. The name of the column added to `data`. Defaults to
  `"cluster"`.

- verbose:

  Logical. Whether to print the k selection table, chosen k, mean
  silhouette, and cluster sizes. Defaults to `TRUE`.

## Value

The original `data` tibble with one additional factor column named by
`cluster_col`. Levels are `"C1"`, `"C2"`, … ordered by decreasing
cluster size.

## Details

### Feature representation

Each series of length \\n\\ is represented as a vector of \\n - 1\\
signed steps \\d_i \in \\-1, 0, +1\\\\. These form the rows of a feature
matrix \\M \in \mathbb{R}^{n\_\text{series} \times (n-1)}\\.

### Which method and distance to choose

For typical hydrological series (weekly to daily, many years):

- Start with `"pca_hclust"` (default). PCA compression removes the noise
  that makes long raw sequences hard to separate.

- If you suspect the series differ mainly in *co-movement* rather than
  exact pattern, try `method = "hclust", distance = "correlation"`. This
  is often the most geologically meaningful choice.

- Use `"pam"` when you want cluster representatives to be real observed
  series that you can point to in the data.

### Composite k-selection score

Plain mean silhouette consistently selects k = 2 because two large blobs
almost always score well globally even when finer structure exists. The
composite score adds two corrections: the worst-cluster term \\\min_c
\bar{s}\_c(k)\\ kills solutions where one cluster is ill-defined, and
the resolution exponent \\k^\text{resolution}\\ rewards cleaner
solutions at higher k.

### Pairing with [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)

    data |>
      cluster_bouquet(time_col = date, series_col = id, value_col = gwl) |>
      make_plot_bouquet(
        time_col      = date,
        series_col    = id,
        value_col     = gwl,
        stem_colors   = cluster,
        flower_colors = cluster,
        facet_by      = cluster
      )

## See also

[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
for visualising the result.

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

# Default: PCA + hclust, auto k
clustered <- cluster_bouquet(gw_long,
  time_col = week, series_col = station, value_col = level_m)
#> PCA: retaining 5 / 51 components (100% variance explained)
#> 
#> Auto k selection (composite silhouette score):
#>   k = 2 : 0.0063
#>   k = 3 : 0.0011
#>   k = 4 : 0.0533 <-- selected
#>   k = 5 : 0.0302
#> 
#> cluster_bouquet  |  method = pca_hclust  |  distance = euclidean (unused)
#> k = 4  |  mean silhouette = 0.053  |  resolution = 0.50
#> Cluster sizes:
#>   C1 : 2 series
#>   C2 : 2 series
#>   C3 : 1 series
#>   C4 : 1 series
#> 

# Correlation distance with hclust — often best for hydrological series
clustered_corr <- cluster_bouquet(gw_long,
  time_col   = week,
  series_col = station,
  value_col  = level_m,
  method     = "hclust",
  distance   = "correlation"
)
#> 
#> Auto k selection (composite silhouette score):
#>   k = 2 : 0.0272
#>   k = 3 : 0.0040
#>   k = 4 : 0.1164 <-- selected
#>   k = 5 : 0.0719
#> 
#> cluster_bouquet  |  method = hclust  |  distance = correlation
#> k = 4  |  mean silhouette = 0.116  |  resolution = 0.50
#> Cluster sizes:
#>   C1 : 2 series
#>   C2 : 2 series
#>   C3 : 1 series
#>   C4 : 1 series
#> 

# Increase resolution to push toward finer, cleaner clusters
clustered_fine <- cluster_bouquet(gw_long,
  time_col   = week,
  series_col = station,
  value_col  = level_m,
  resolution = 1.5
)
#> PCA: retaining 5 / 51 components (100% variance explained)
#> 
#> Auto k selection (composite silhouette score):
#>   k = 2 : 0.0126
#>   k = 3 : 0.0033
#>   k = 4 : 0.0533 <-- selected
#>   k = 5 : 0.0302
#> 
#> cluster_bouquet  |  method = pca_hclust  |  distance = euclidean (unused)
#> k = 4  |  mean silhouette = 0.053  |  resolution = 1.50
#> Cluster sizes:
#>   C1 : 2 series
#>   C2 : 2 series
#>   C3 : 1 series
#>   C4 : 1 series
#> 

# Visualise — colour and facet by cluster
# \donttest{
make_plot_bouquet(clustered,
  time_col      = week,
  series_col    = station,
  value_col     = level_m,
  stem_colors   = cluster,
  flower_colors = cluster,
  facet_by      = cluster,
  title         = "Groundwater Stations by Directional Cluster"
)
#> === Heading sweep per series (max - min of cumulative net steps) ===
#> # A tibble: 6 × 2
#>   bq_s  cum_net_range
#>   <chr>         <int>
#> 1 S1               11
#> 2 S2               10
#> 3 S3               18
#> 4 S4                8
#> 5 S5               11
#> 6 S6               14
#> 
#> Binding series : S3 (sweep = 18 steps)
#> Ceiling angle  : 20.00°
#> theta (80%)  : 16.00°
#> 

# }
```
