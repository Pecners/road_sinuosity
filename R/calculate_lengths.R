library(glue)

st <- list.files("data")

counties <- map_df(st, function(s) {
  if (length(list.files(glue("data/{s}"))) > 20) {
    counties <- tigris::counties(state = s)
    counties |> 
      select(NAME, geometry) |> 
      mutate(state = s) |> 
      st_transform(crs = st_crs(r))
  }
})

sf_use_s2(FALSE)

#eff_start_time1 <- Sys.time()
all_st <- map_df(c("California", "New York", "Texas"), function(s) {
  r_st <- r |> 
    filter(state == s)
  
  cst <- counties |> 
    filter(state == s)
  
  temp_ <- map_df(1:nrow(cst), function(c) {
    one_c <- cst[c,] 
    county <- one_c$NAME
    
    p_county <- st_intersection(r_st, one_c)
    
    temp_ <- map_df(1:nrow(p_county), function(x) {
      cat(crayon::cyan("Starting", s, "County", c, "Road", x, "  "))
      t_ <- p_county[x,"geometry"]
      
      if (length(st_geometry_type(t_)) > 0) {
        # Extract LINESTRINGS from GEOMETRYCOLLECTION
        if (st_geometry_type(t_) == "GEOMETRYCOLLECTION") {
          t_ <- st_collection_extract(t_,type = c("LINESTRING"))
        }
        
        if (st_geometry_type(t_[1,]) == "MULTILINESTRING") {
          t_ <- st_cast(t_, "LINESTRING")
        }
        
        # Handle LINESTRINGS
        if (st_geometry_type(t_[1,]) == "LINESTRING") {
          cat(crayon::white(paste0(st_geometry_type(t_), "\n")))
          
          map_df(1:nrow(t_), function(ml) {
            sub_l <- t_[ml,]
            t_points <- st_cast(sub_l, "POINT")
            
            dist <- st_distance(t_points[1,], t_points[nrow(t_points),])
            l_length <- st_length(sub_l)
            point_count <- nrow(t_points)
            
            df <- tibble(dist = dist,
                         l_length = l_length,
                         point_count = point_count,
                         county = county)
          })
          
        } else {
          cat(crayon::red(paste0("OTHER GEOMETRY: ", st_geometry_type(t_), "\n")))
        }
      }
      
    })
    
    return(temp_)
  })
  saveRDS(temp_, glue("data/{s}/calculated.rda"))
  
})
#eff_end_time1 <- Sys.time()
#comment_time <- eff_end_time - eff_start_time



