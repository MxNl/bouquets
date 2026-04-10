# ============================================================
#  DEMO
# ============================================================

library(dplyr)

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
  mutate(`Station E` = 8 + dplyr::row_number() * 0.01) |> 
  mutate(`Station F` = 8 - dplyr::row_number() * 0.01)
  
make_plot_bouquet(
  data         = gw_data,
  time_col     = week,
  title        = "Groundwater Level Fluctuations \u2014 2023",
  # colors       = c("#4fc3f7", "#81c784", "#ffb74d"),
  ceiling_pct  = 0.95,
  launch_deg   = 90,
  # marker_every = 13,          # quarterly dots
  # show_rings   = FALSE,
  verbose      = TRUE
)








# GEMS GER Data ----------------------------------------------------------

library(data.table)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)


# Import ------------------------------------------------------------------

data_directory <- "../../git_daten/bouquet/GEMS-GER_data/dynamic/"

n_wells <- 50
data_paths <- list.files(data_directory, full.names = TRUE)

# set.seed(42)
# data_paths <- 
#   data_paths |> 
#   sample(size = n_wells)

data_gems_static <- read_csv("../../git_daten/bouquet/GEMS-GER_data/static/static_features_MW_1toMW_3207.csv") |> 
  janitor::clean_names()

set.seed(42)
wells_to_read <- data_gems_static |> 
  slice_sample(n = 10, by = hyraum_hd) |> 
  pull(mw_id) |> 
  str_c(... = _, ".csv")

data_gems <- 
  data_paths |>
  keep(\(x) str_detect(x, str_c(wells_to_read, collapse = "|"))) |> 
  map(\(p) fread(p) |> as_tibble() |> mutate(path = p)) |>
  list_rbind() |>
  mutate(id = word(path, sep = "/", start = -1) |> str_remove(".csv")) |> 
  relocate(id, path) |> 
  select(-path) |> 
  rename(date = V1) |> 
  select(id, date, gwl = GWL) |> 
  tidyr::pivot_wider(names_from = "id", values_from = "gwl")

# data_gems <- 
#   data_gems |> 
#   filter(year(date) >= 2020)

data_gems <- 
  data_gems |> 
  pivot_longer(cols = -"date", names_to = "id", values_to = "gwl") |> 
  left_join(rename(data_gems_static, id = mw_id), by = join_by(id)) |> 
  mutate(state = str_sub(proj_id, end = 2)) |> 
  select(-proj_id) |> 
  mutate(across(contains("hyraum"), as.factor))

data_gems |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    flower_color = "blossom",
    stem_colors = "greens"
  )

svg_text <- readLines("inst/extdata/flower.svg")

ggplot(data.frame(x=1, y=1, g="A"), aes(x,y)) +
  ggsvg::geom_point_svg(aes(colour=g), size=10, svg=svg_text) +
  ggplot2::scale_colour_manual(values = "green")

snowman_txt <- '
  <svg viewBox="0 0 100 100 ">
    <circle id="top" cx="50" cy="20" r="20" fill="brown" stroke="black" />
    <circle id="bot" cx="50" cy="70" r="30" fill="brown" stroke="black" />
  </svg>
  '

svg_data

svg_data <- paste(readLines("inst/extdata/flower.svg"), collapse = "\n")

ggplot(data.frame(x=1, y=1, g="A")) +
  ggsvg::geom_point_svg(aes(x, y, css("path#blossom", fill = g)), size=10, svg=svg_data) +
  

test_df <- data.frame(
  x = runif(10), 
  y = runif(10), 
  count = sample(3:5, 10, T),
  type  = sample(c('a', 'b', 'c'), 10, T))

ggplot(test_df) + 
  geom_point_svg(aes(x, y), svg = svg_txt) + 
  theme_bw()

ggplot(test_df) + 
  geom_point_svg(
    aes(x, y, css("circle#top", fill = type)),
    # css("circle#bot", stroke = 'brown'),
    # css("circle", 'stroke-width'=10),
    svg = svg_data
  ) +
  theme_bw()
  scale_svg_fill_brewer(aesthetics = css("circle#top", fill = type), palette = 'Dark2')


data_gems |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    flower_color = "blossom",
    stem_colors = "greens",
    normalise = TRUE,
    highlight = "MW_1018"
  )

data_gems |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    flower_color = "blossom",
    stem_colors = "greens",
    highlight = c("MW_1018", "MW_1107")
  )

  data_gems |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = "blossom",
    facet_by = hyraum_hd,
    stem_colors = "greens"
  )

data_gems |> 
  cluster_bouquet(
    # distance = "correlation", 
    method = "hclust",
    k = 25
  ) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = cluster,
    facet_by = cluster,
    stem_colors = "greens"
  )


data_gems |> 
  cluster_bouquet(
    distance = "correlation", 
    method = "hclust",
    k = 12
  ) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = "blossom",
    facet_by = cluster,
    lon_col = easting_epsg_3035,
    lat_col = northing_epsg_3035,
    stem_colors = "greens",
    coord_crs = 3035
  )



data_gems |> 
  cluster_bouquet(
    distance = "correlation", 
    method = "hclust",
    k = 5
  ) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = cluster,
    facet_by = cluster,
    lon_col = easting_epsg_3035,
    lat_col = northing_epsg_3035,
    stem_colors = cluster,
    coord_crs = 3035,
    cluster_hull = cluster,
    hull_coverage = .5
  )


data_gems |> 
  cluster_bouquet(
    distance = "correlation", 
    method = "hclust",
    k = 5
  ) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = cluster,
    facet_by = cluster,
    lon_col = easting_epsg_3035,
    lat_col = northing_epsg_3035,
    stem_colors = cluster,
    coord_crs = 3035,
    cluster_hull = cluster,
    hull_coverage = .5
  )

sample_groups <- function(df, col, n) {
  groups <- df %>% distinct({{ col }}) %>% slice_sample(n = n)
  semi_join(df, groups, by = rlang::as_name(rlang::ensym(col)))
}


# clustering method needs some tuning as the clusters visually from the plot are not really logical or consistent
# replace hues package for colours by another modern colour palette that also adresses colourblind friendliness


set.seed(42)
data_gems |> 
  sample_groups(id, 20) |> 
  bouquets::cluster_bouquet(
  # seed = NULL,
  normalise = FALSE,
  method = "coords_hclust"
) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.9,
    flower_color = cluster,
    stem_colors = cluster,
    marker_every = "year",
    show_labels = TRUE,
    label_color = "grey"
  )

set.seed(42)
data_gems |> 
  sample_groups(id, 20) |> 
  arrange() |> 
  group_by(id, year(date), month(date)) |> 
  slice_head() |> 
  ungroup() |> 
  cluster_bouquet(
  # seed = NULL,
  normalise = FALSE,
  method = "coords_hclust"
) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.7,
    flower_color = cluster,
    stem_colors = cluster,
    # stem_colors = "greens",
    # marker_every = "year",
    show_labels = TRUE,
    label_color = "grey",
    lon_col = easting_epsg_3035,
    lat_col = northing_epsg_3035,
    cluster_hull = cluster
  )



data_gems |> 
  cluster_bouquet(
    distance = "correlation", 
    method = "hclust",
    k = 12
  ) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = cluster,
    facet_by = cluster,
    lon_col = easting_epsg_3035,
    lat_col = northing_epsg_3035,
    stem_colors = cluster,
    coord_crs = 3035,
    cluster_hull = cluster
  )


data_gems |> 
  cluster_bouquet(
    distance = "correlation", 
    method = "hclust",
    k = 5
  ) |> 
  make_plot_bouquet(
    time_col = date,
    series_col = id,
    value_col = gwl,
    title = "Groundwater levels in Germany",
    ceiling_pct = 0.95,
    flower_color = cluster,
    stem_colors = "greens"
  )

# data_gems |> 
#   make_plot_bouquet_geofacet(
#     time_col = date,
#     series_col = id,
#     value_col = gwl,
#     state_col = state,
#     title = "Groundwater levels in Germany",
#     ceiling_pct = 0.95,
#     flower_color = "blossoms",
#     stem_colors = "greens"
#   )
