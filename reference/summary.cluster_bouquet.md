# Summarise a cluster_bouquet result

Prints a structured report of the clustering: method, k selection scores
(when k was chosen automatically), silhouette quality, cluster sizes,
and per-series silhouette widths.

## Usage

``` r
# S3 method for class 'cluster_bouquet'
summary(object, ...)
```

## Arguments

- object:

  A `cluster_bouquet` object returned by
  [`cluster_bouquet()`](https://mxnl.github.io/bouquets/reference/cluster_bouquet.md).

- ...:

  Unused; present for S3 compatibility.

## Value

`object`, invisibly (so the result can still be piped).

## Examples

``` r
set.seed(42)
n <- 52L
gw <- tibble::tibble(
  week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 4L),
  station = rep(paste0("S", 1:4), each = n),
  level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)),
              cumsum(rnorm(n)), cumsum(rnorm(n)))
)
summary(cluster_bouquet(gw, week, station, level))
#> -- cluster_bouquet summary ----------------------------------------------
#>   Method    : pca_hclust
#>   Distance  : euclidean (unused)
#>   Normalise : FALSE
#>   Seed      : none
#>   Series    : 4   k = 3   Resolution = 0.50
#>   Mean silhouette : 0.072  (weak structure -- consider more data or fewer clusters)
#> 
#>   Auto k selection (composite silhouette score):
#>     k = 2 : 0.0349
#>     k = 3 : 0.0716  <-- selected
#> 
#>   Cluster sizes and members:
#>     C1  (2 series, mean sil = 0.143)
#>        S1, S4
#>     C2  (1 series, mean sil = 0.000)
#>        S2
#>     C3  (1 series, mean sil = 0.000)
#>        S3
#> 
#>   Per-series silhouette widths:
#>     S4                    C1   0.158  |||
#>     S1                    C1   0.129  |||
#>     S2                    C2   0.000  
#>     S3                    C3   0.000  
#> ------------------------------------------------------------------------
```
