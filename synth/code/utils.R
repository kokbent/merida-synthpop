slice_raster <- function (in_raster, shp, to_file = NULL) {
  if (is.character(in_raster) & is.character(shp)) {
    in_raster <- raster::raster(in_raster)
    shp <- sf::st_read(shp)
  }
  
  shp1 <- shp %>%
    sf::st_transform(crs(in_raster)) %>%
    sf::as_Spatial()
  ras <- raster::crop(in_raster, shp1)
  ras <- raster::mask(ras, shp1)
  
  return(ras)
}

extract_mc <- function (rast, sp, fun = sum, ...) {
  val <- 1:nrow(sp) %>%
    furrr::future_map(~ raster::extract(rast, sp[.x,], fun = fun, ...)) %>%
    purrr::map_dbl(~ ifelse(is.null(unlist(.x)), NA, .x))
  
  return(val)
}

st_sample_id <- function (sf_obj, size, id) {
  pt <- sf::st_sample(sf_obj, size) %>%
    sf::st_coordinates()
  pt <- cbind(pt, id) %>%
    as.data.frame()
  return(pt)
}

st_sample_mc <- function (sf_obj, size, ...) {
  pt <- 1:nrow(sf_obj) %>%
    furrr::future_map_dfr(~ st_sample_id(sf_obj[.x,], size[.x], .x))
  pt <- st_as_sf(pt, coords = c("X", "Y")) %>%
    st_set_crs(crs(sf_obj))
  
  return(pt)
}

create_hhcoord <- function (x, y, xdiff = 0.0008333, ydiff = 0.0008333) {
  hh_x <- x
  hh_y <- y
  hh_x <- hh_x + runif(length(x), -xdiff/2, xdiff/2)
  hh_y <- hh_y + runif(length(y), -ydiff/2, ydiff/2)
  
  return(data.frame(x = hh_x, y = hh_y))
}

sample_pts <- function (size, pts, prob) {
  if (size == 0) return(data.frame())
  s <- sample(1:nrow(pts), size = size, replace = T, prob = prob)
  df <- create_hhcoord(pts$X[s], pts$Y[s])
  
  return(df)
}

weighted_spatsamp <- function (rast, sf_obj, size) {
  rast_df <- rast %>%
    raster::as.data.frame(xy = T)
  colnames(rast_df) <- c("x", "y", "z")
  rast_df <- rast_df %>%
    dplyr::filter(!is.na(z))
  
  rast_xy <- sf::st_as_sf(rast_df, coords = c("x", "y"))
  rast_xy <- st_set_crs(rast_xy, crs(rast))
  rast_xy <- st_transform(rast_xy, 4326)
  
  sf_obj <- sf_obj %>%
    dplyr::select(geometry) %>%
    dplyr::mutate(id = dplyr::row_number())
  sf_obj <- sf::st_transform(sf_obj, 4326)
  
  rast_xy <- sf::st_join(rast_xy, sf_obj, left = F)
  rast_xy <- cbind(rast_xy, sf::st_coordinates(rast_xy))
  rast_xy <- sf::st_set_geometry(rast_xy, NULL)
  
  rast_xy <- split(rast_xy, rast_xy$id)
  
  samp <- furrr::future_map2_dfr(
    .x = 1:length(rast_xy),
    .y = rast_xy,
    .f = ~ sample_pts(size[.x], .y[,c("X", "Y")], .y$z),
    .id = "id")
  samp$id <- as.numeric(samp$id)
  
  return(samp)
}

make_full_path <- function (cfg) {
  ## Add cfg$data_folder to the front of cfg$path_XXX
  ## And also check existence of files
  ind <- names(cfg) %>% 
    str_starts("path_") %>% 
    which
  
  cfg[ind] <- cfg[ind] %>%
    lapply(function (x) file.path(cfg$data_folder, x) %>%
             normalizePath(mustWork = T))
  
  return(cfg)
}

name_type_key <- function (name, lis) {
  string <- paste(name, lis$type)
  if (!is.null(lis$primary)) {
    if (lis$primary == 1) {
      string <- paste(string, "PRIMARY KEY")
    }
  }
  return(string)
}

json_to_sql_schema <- function (json_file) {
  json <- jsonlite::fromJSON(json_file)
  
  schema <- c()
  for (i in 1:length(json)) {
    stm1 <- map2(names(json[[i]]), json[[i]], 
                 ~ name_type_key(.x, .y)) %>%
      unlist %>%
      paste(collapse = ", ")
    
    stm <- paste("CREATE TABLE", 
                 names(json)[i], "(", 
                 stm1, ");")
    
    schema <- c(schema, stm)
  }
  
  return(schema)
}

check_extra <- function (json_file) {
  json <- jsonlite::fromJSON(json_file)
  extra_cond <- lapply(json$loc, function (x) any("extra" %in% names(x)))
  extra_name <- names(json$loc)[unlist(extra_cond)]
  
  return(extra_name)
}
