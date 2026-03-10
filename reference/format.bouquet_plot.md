# Format a bouquet_plot for display

Returns a one-line description of the bouquet plot suitable for
[`base::cat()`](https://rdrr.io/r/base/cat.html) or automatic printing
in interactive sessions.

## Usage

``` r
# S3 method for class 'bouquet_plot'
format(x, ...)
```

## Arguments

- x:

  A `bouquet_plot` object returned by
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md).

- ...:

  Unused; present for S3 compatibility.

## Value

A character string description.
