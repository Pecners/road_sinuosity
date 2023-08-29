library(tidyverse) 
library(sf)

dirs <- list.files("data")

r <- map_df(dirs[9], function(di) {
  ff <- list.files(glue("data/{di}"))
  if (length(ff) > 0) {
    secondary <- read_rds(glue("data/{di}/secondary.rda"))
    tertiary <- read_rds(glue("data/{di}/tertiary.rda"))
    
    roads <- list(
      #motorways = motorways$osm_lines,
      #trunk = trunk$osm_lines,
      #primary = primary$osm_lines,
      secondary = secondary |> select(geometry),
      tertiary = tertiary |> select(geometry)
    )
    
    set_crs <- st_crs(roads$secondary)
    
    r <- bind_rows(roads$secondary, roads$tertiary) |> 
      mutate(state = di)
  }
  
})

big <- c(
  "California",
  "New York",
  "Texas"
)

r <- map_df(big, function(di) {
  ff <- list.files(glue("data/{di}"))
  these <- str_detect(ff, "secondary.rda|tertiary.rda")
  these_ff <- ff[these]
  map_df(these_ff, function(tf) {
    cat(crayon::cyan(glue("Starting {di} {tf}")), "\n")
    tmp <- read_rds(glue("data/{di}/{tf}")) 
    
    if (!is.null(tmp)) {
      return(tmp |> 
               select(geometry) |> 
               mutate(state = di))
    }
  })
})
