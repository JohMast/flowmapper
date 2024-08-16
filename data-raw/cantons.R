library(sf)
library(dplyr)
cantons <-
  geodata::gadm(country="CHE", level=1, path=tempdir()) |>
  st_as_sf()|>
  select(NAME_1) |>
  st_transform(3857)

# remove crs from cantons to be suitable with cran (https://github.com/r-spatial/sf/issues/1341)
st_crs(cantons) <- NA

usethis::use_data(cantons, overwrite = TRUE)
