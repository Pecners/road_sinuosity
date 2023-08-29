library(ggiraph)

j <- joined |> 
  select(NAME = NAMELSAD,
         state,
         te,
         geometry) |> 
  st_as_sf()

j_coords <- st_coordinates(st_centroid(j))

all <- bind_cols(j, j_coords)

st_write(all, driver = "GeoJSON", dsn = "data5.geojson", 
         layer_options = "ID_GENERATE=YES")

centroids <- j |> 
  st_centroid()

scico(n = 5, palette = "bilbao")

quantile(j$te)

st_write(all, driver = "GeoJSON", dsn = "data4.geojson", 
         layer_options = "ID_GENERATE=YES")

st_write(centroids |> select(geometry), driver = "GeoJSON", dsn = "centers.geojson", 
         layer_options = "ID_GENERATE=YES")

p <- j |> 
  ggplot(aes(fill = te)) +
  geom_sf_interactive(aes(tooltip = glue("{NAME}, {state}"),
                          data_id = id),
                      color = "white", linewidth = .005) +
  scale_fill_scico(palette = "bilbao", direction = -1) +
  theme_void() +
  theme(legend.position = "none") +
  coord_sf(crs = 2163)


ggiraph(ggobj = p,)

