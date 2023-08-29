library(sf)
library(rnaturalearth)
library(scico)
library(tidyverse)
library(glue)

st <- list.files("data")

all_states <- map_df(st, function(s) {
  
  if (length(list.files(glue("data/{s}"))) > 0) {
    all_st <- read_rds(glue("data/{s}/calculated.rda"))
    
    all_st %>%
      mutate(efficiency = dist / l_length) %>%
      group_by("NAME" = county) %>%
      summarise(total_eff = sum(dist, na.rm = TRUE) / sum(l_length, na.rm = TRUE),
                total_dist = sum(dist, na.rm = TRUE),
                total_length = sum(l_length, na.rm = TRUE)) %>%
      mutate(te = as.numeric(total_eff),
             state = s)
  }
})

counties <- map_df(st, function(s) {
  if (length(list.files(glue("data/{s}"))) > 0) {
    counties <- tigris::counties(state = s)
    counties |> 
      select(NAME, geometry, NAMELSAD) |> 
      mutate(state = s)
  }
})

l <- ne_download(type = "lakes", category = "physical", scale = "large") |> 
  st_as_sf(crs = st_crs(counties))

lakes <- c("Lake Erie",
           "Lake Michigan",
           "Lake Superior",
           "Lake Huron",
           "Lake Ontario")
gl <- l |>
  filter(name %in% lakes) |>
  st_transform(crs = st_crs(counties)) |> 
  st_union()

land <- ne_download(type = "land", category = "physical", scale = "large")  |>
  st_as_sf() |>
  st_transform(., crs = st_crs(counties)) |> 
  st_union()

skinny_s <- st_difference(counties, gl)

skinny_s <- st_intersection(skinny_s, land)


joined <- left_join(all_states, skinny_s)

saveRDS(joined, "data/joined.rda")

st_as_sf(joined) |> 
  st_bbox() |> 
  st_as_sfc() |> 
  st_centroid()


p <- joined |> 
  # filter(te > .99) |> 
  st_as_sf() |> 
  ggplot(aes(fill = te)) +
  geom_sf(color = "white", linewidth = .01) +
  scale_fill_scico(palette = "bilbao", direction = -1) +
  theme_void() +
  coord_sf(crs = 2163)

ggsave(p, filename = "plots/us_counties.png", bg = "white")



