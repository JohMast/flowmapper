library(sf)
library(dplyr)
cantons <-
  geodata::gadm(country="CHE", level=1, path=tempdir()) |>
  st_as_sf()|>
  select(NAME_1) |>
  st_transform(3857)

# remove crs from cantons to be suitable with cran (https://github.com/r-spatial/sf/issues/1341)
st_crs(cantons) <- NA

# this avoids R CMD check complained about non-ASCII characters
cantons$NAME_1 <- iconv(cantons$NAME_1, from = "UTF-8", to = "UTF-8")

usethis::use_data(cantons, overwrite = TRUE)
