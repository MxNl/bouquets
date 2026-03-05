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

n_wells <- 25
data_paths <- list.files(data_directory, full.names = TRUE)

set.seed(42)
data_paths <- data_paths |> 
  sample(size = n_wells)

data_gems_static <- read_csv("../../git_daten/bouquet/GEMS-GER_data/static/static_features_MW_1toMW_3207.csv")


data_gems <- data_paths |>
  map(\(p) fread(p) |> as_tibble() |> mutate(path = p)) |>
  list_rbind() |>
  mutate(id = word(path, sep = "/", start = -1) |> str_remove(".csv")) |> 
  relocate(id, path) |> 
  select(-path) |> 
  rename(date = V1) |> 
  select(id, date, gwl = GWL) |> 
  tidyr::pivot_wider(names_from = "id", values_from = "gwl")

data_gems <- data_gems |> 
  filter(year(date) >= 2010)

data_gems |> 
  make_plot_bouquet(
    title = "Groundwater levels in Germany",
    flower_color = "blossom",
    stem_colors = "greens"
  )
