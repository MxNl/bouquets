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
#>   Method    : coords_hclust
#>   Normalise : FALSE
#>   Seed      : none
#>   Series    : 4   k = 2   Resolution = 0.50
#>   Mean silhouette : 0.347  (reasonable structure)
#> 
#>   Auto k selection (composite silhouette score):
#>     k = 2 : 0.3472  <-- selected
#>     k = 3 : 0.1765
#> 
#>   Cluster sizes and members:
#>     C1  (3 series, mean sil = 0.463)
#>        S1, S3, S4
#>     C2  (1 series, mean sil = 0.000)
#>        S2
#> 
#>   Per-series silhouette widths:
#>     S1                    C1   0.587  ||||||||||||
#>     S4                    C1   0.462  |||||||||
#>     S3                    C1   0.340  |||||||
#>     S2                    C2   0.000  
#> ------------------------------------------------------------------------
```
