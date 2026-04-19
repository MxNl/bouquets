# Save a bouquet plot to a file

A thin wrapper around
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
with sensible defaults for the square-ish aspect ratio typical of
bouquet plots, so you don't have to remember the right `width`/`height`
each time.

## Usage

``` r
save_bouquet(
  plot = ggplot2::last_plot(),
  file = "bouquet.png",
  width = 10,
  height = 8,
  units = "in",
  dpi = 300,
  ...
)
```

## Arguments

- plot:

  A ggplot or patchwork object returned by
  [`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md).
  Defaults to the last plot printed
  ([`ggplot2::last_plot()`](https://ggplot2.tidyverse.org/reference/get_last_plot.html)).

- file:

  Path to the output file. The format is inferred from the extension
  (`.png`, `.pdf`, `.svg`, etc.). Default `"bouquet.png"`.

- width:

  Width in `units`. Default `10`.

- height:

  Height in `units`. Default `8`.

- units:

  One of `"in"`, `"cm"`, `"mm"`, or `"px"`. Default `"in"`.

- dpi:

  Resolution in dots per inch. Used for raster formats (PNG, JPEG,
  TIFF). Default `300`.

- ...:

  Additional arguments forwarded to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Value

The `file` path, invisibly.

## See also

[`make_plot_bouquet()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet.md)
which produces the plots saved by this function.
[`make_plot_bouquet_interactive()`](https://mxnl.github.io/bouquets/reference/make_plot_bouquet_interactive.md)
for an interactive plotly version that can be exported with
[`htmlwidgets::saveWidget()`](https://rdrr.io/pkg/htmlwidgets/man/saveWidget.html).

## Examples

``` r
# \donttest{
set.seed(42)
n   <- 52L
gw  <- tibble::tibble(
  week    = rep(seq(as.Date("2023-01-01"), by = "week", length.out = n), 3L),
  station = rep(c("A", "B", "C"), each = n),
  level   = c(cumsum(rnorm(n)), cumsum(rnorm(n)), cumsum(rnorm(n)))
)
p <- make_plot_bouquet(gw, week, station, level)
tmp <- tempfile(fileext = ".png")
save_bouquet(p, tmp)
#> <bouquet_plot>  3 series | theta = 14.4 deg | binding: C
#> Saved bouquet plot to: /tmp/RtmpzS2tR6/file1d5546b9013b.png
# }
```
