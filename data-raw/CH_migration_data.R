library(tidyverse)
library(sf)
# download migration data from Federal Statistical Office
data_url <- "https://dam-api.bfs.admin.ch/hub/api/dam/assets/3222163/master"
temp <- tempfile()
download.file(data_url,temp, mode = "wb")

data <-
  # read the data
  readxl::read_xlsx(temp,skip=3) |>
  slice_head(n=28) |>
  # transform to long format
  pivot_longer(cols=-Wegzugskanton,names_to = "Zuzugskanton",values_to = "migrations") |>
  # remove unknown flow sources and country-wide flows
  filter(!Wegzugskanton %in% c("unbekannt","Schweiz"),
         !Zuzugskanton %in% c("unbekannt","Schweiz")) |>
  # change some names to be consistent with the GADM NAME_1 field
  mutate(Wegzugskanton  = case_when(
    Wegzugskanton=="Appenzell A.Rh." ~ "Appenzell Ausserrhoden",
    Wegzugskanton=="Appenzell I.Rh." ~ "Appenzell Innerrhoden",
    Wegzugskanton=="Schweiz" ~ "Schwyz",
    Wegzugskanton=="Luzern" ~ "Lucerne",
    Wegzugskanton=="Freiburg" ~ "Fribourg",
    Wegzugskanton=="St. Gallen" ~ "Sankt Gallen",
    Wegzugskanton=="Tessin" ~ "Ticino",
    Wegzugskanton=="Waadt" ~ "Vaud",
    Wegzugskanton=="Wallis" ~ "Valais",
    Wegzugskanton=="Neuenburg" ~ "Neuchâtel",
    Wegzugskanton=="Genf" ~ "Genève",
    TRUE ~ Wegzugskanton
  ))|>
  mutate(Zuzugskanton = case_when(
    Zuzugskanton=="Appenzell A.Rh." ~ "Appenzell Ausserrhoden",
    Zuzugskanton=="Appenzell I.Rh." ~ "Appenzell Innerrhoden",
    Zuzugskanton=="Schweiz" ~ "Schwyz",
    Zuzugskanton=="Luzern" ~ "Lucerne",
    Zuzugskanton=="Freiburg" ~ "Fribourg",
    Zuzugskanton=="St. Gallen" ~ "Sankt Gallen",
    Zuzugskanton=="Tessin" ~ "Ticino",
    Zuzugskanton=="Waadt" ~ "Vaud",
    Zuzugskanton=="Wallis" ~ "Valais",
    Zuzugskanton=="Neuenburg" ~ "Neuchâtel",
    Zuzugskanton=="Genf" ~ "Genève",
    TRUE ~ Zuzugskanton
  )) |>
  rename(orig=Wegzugskanton,
         dest=Zuzugskanton)

# get gadm admin units (cantons)
kantons <-
  geodata::gadm(country="CHE", level=1, path=tempdir()) |>
  st_as_sf()|>
  select(NAME_1) |>
  st_transform(3857)

# get the centroid coords
kantons_coords <-
  kantons |>
  st_centroid() %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2]) |>
  st_drop_geometry() |>
  select(NAME_1,x=lon,y=lat)

# join the centroid coordinates to the flows
kanton_flows <-
  data |>
  select(id_a = orig,id_b = dest,flow_ab=migrations) |>
  mutate(flow_ab=round(flow_ab)) |>
  # coords of origin
  left_join(kantons_coords |> select(NAME_1,xa=x,ya=y),by=c("id_a"="NAME_1"))|>
  # coords of destination
  left_join(kantons_coords |> select(NAME_1,xb=x,yb=y),by=c("id_b"="NAME_1")) |>
  # remove self-flows
  filter(id_a!=id_b)

# combine flows from a-b and b-a in one row by jpining the flow data with itself
CH_migration_data <- kanton_flows |>
  left_join(kanton_flows |>
              select(id_a=id_b,id_b=id_a,flow_ba=flow_ab),
            by=c("id_a", "id_b")) |>
  # select only one of the two rows (a-b and b-a) where the orig comes before the dest in alphabetical order
  filter(id_a>id_b)

# this avoids R CMD check complained about non-ASCII characters
CH_migration_data$id_a <- iconv(CH_migration_data$id_a, from = "UTF-8", to = "ASCII//TRANSLIT")
CH_migration_data$id_b <- iconv(CH_migration_data$id_b, from = "UTF-8", to = "ASCII//TRANSLIT")
write_csv(CH_migration_data, "data-raw/CH_migration_data.csv")

usethis::use_data(CH_migration_data, overwrite = TRUE)
