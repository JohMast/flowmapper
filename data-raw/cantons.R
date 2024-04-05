library(sf)
library(dplyr)
cantons <-
  geodata::gadm(country="CHE", level=1, path=tempdir()) |>
  st_as_sf()|>
  select(NAME_1) |>
  st_transform(3857)

usethis::use_data(cantons, overwrite = TRUE)
