rm(list = ls()[ls() != "cfg"])

source("synth/code/utils.R")

### Some initial stuff
planar_crs <- CRS(cfg$planar_crs)
# ext <- extent(-89.75218, -89.50441, 20.84762, 21.07664)

ddi <- read_ipums_ddi(cfg$path_ipums_xml)
ipums <- read_ipums_micro(ddi) # .dat file needs to be in same folder as xml
ipums$CVE_MUN <- as.character(ipums$GEO2_MX2015) %>%
  str_sub(start = 3, end = 5)

### Population (Slice Mexico raster to Yucatan)
r <- slice_raster(cfg$path_popdens_raster, cfg$path_state_shp)

### Dealing with urban census block group
blocshp <- st_read(cfg$path_urban_cbg_shp)

blocshp_sp <- as(blocshp, "Spatial")
plan(multiprocess, workers = 5)
system.time(bloc_pop <- extract_mc(r, blocshp_sp, fun = sum, na.rm = T))
blocshp$bloc_pop <- bloc_pop

#### Person per household
urb_pers_ph <- ipums %>%
  filter(URBAN == 2) %>%
  summarise(pph = n() / length(unique(SERIAL))) %>%
  .$pph
rur_pers_ph <- ipums %>%
  filter(URBAN == 1) %>%
  summarise(pph = n() / length(unique(SERIAL))) %>%
  .$pph

urb_tot <- ipums %>%
  filter(URBAN == 2) %>%
  summarise(TOTALPOP = sum(PERWT)) %>%
  .$TOTALPOP

rur_tot <- ipums %>%
  filter(URBAN == 1) %>%
  summarise(TOTALPOP = sum(PERWT)) %>%
  .$TOTALPOP

#### Sample urban household coordinates
urb_blocshp <- blocshp %>%
  filter(AMBITO == "U", bloc_pop != 0) %>%
  mutate(HH = ceiling((urb_tot / sum(bloc_pop)) * (bloc_pop / urb_pers_ph)))

if (!cfg$statewide_mode) {
  ext <- cfg$target_extent
  ext_mat <- matrix(c(ext[1], ext[3],
                      ext[1], ext[4],
                      ext[2], ext[4],
                      ext[2], ext[3],
                      ext[1], ext[3]),
                    nrow = 5, byrow = T)
  ext <- st_polygon(list(ext_mat))
  cond <- st_intersects(urb_blocshp, ext, sparse = F)

  urb_blocshp <- urb_blocshp[cond,]
}

urb_blocshp_planar <- urb_blocshp %>%
  st_transform(planar_crs)

plan(multiprocess, workers = 5)
system.time(urb_hh <- st_sample_mc(urb_blocshp_planar, urb_blocshp_planar$HH))

urb_hh$CVE_MUN <- urb_blocshp$CVE_MUN[urb_hh$id]
urb_hh$CVE_MZA <- urb_blocshp$CVE_MZA[urb_hh$id]
urb_hh$URBAN <- "U"
urb_hh1 <- st_transform(urb_hh, 4326)
urb_hh1 <- cbind(urb_hh1, st_coordinates(urb_hh1))
urb_hh1 <- urb_hh1 %>%
  st_set_geometry(NULL)
urb_hh1 <- rename(urb_hh1, x = X, y = Y)

#### Dealing and sampling coordinates for rural groups
rur_ageb_shp <- st_read(cfg$path_rural_cbg_shp)
rur_ageb_sp <- as(rur_ageb_shp, "Spatial")
plan(multiprocess, workers = 5)
system.time(rur_ageb_pop <- extract_mc(r, rur_ageb_sp, fun = sum, na.rm = T))
rur_ageb_shp$pop <- rur_ageb_pop

rur_ageb_shp <- rur_ageb_shp %>%
  filter(!is.na(pop)) %>%
  mutate(HH = round((rur_tot / sum(pop)) * (pop / rur_pers_ph)))

if (!cfg$statewide_mode) {
  ext <- cfg$target_extent
  ext_mat <- matrix(c(ext[1], ext[3],
                      ext[1], ext[4],
                      ext[2], ext[4],
                      ext[2], ext[3],
                      ext[1], ext[3]),
                    nrow = 5, byrow = T)
  ext <- st_polygon(list(ext_mat))
  cond <- st_intersects(rur_ageb_shp, ext, sparse = F)

  rur_ageb_shp <- rur_ageb_shp[cond,]
}

rur_hh <- weighted_spatsamp(r, rur_ageb_shp, rur_ageb_shp$HH)

rur_hh$CVE_MUN <- rur_ageb_shp$CVE_MUN[rur_hh$id]
rur_hh$URBAN <- "R"

#### Plot
ggplot() +
  geom_point(aes(x=x, y=y), data = urb_hh1, col = "#FF000050", pch = ".") +
  geom_point(aes(x=x, y=y), data = rur_hh, col = "#0000FF50", pch = ".") +
  coord_fixed()

#### Assign these coordinates to IPUMS households
ipums_hh <- ipums %>%
  select(SERIAL, HHWT, URBAN, CVE_MUN) %>%
  distinct()
CVE_MUNs <- unique(ipums_hh$CVE_MUN) %>%
  sort

gen_hh <- data.frame()
for (m in CVE_MUNs) {
  print(m)
  sub_ipums_hh <- ipums_hh %>%
    filter(CVE_MUN == m)
  
  urb_m <- urb_hh1 %>%
    filter(CVE_MUN == m) %>%
    select(x, y)
  rur_m <- rur_hh %>%
    filter(CVE_MUN == m) %>%
    select(x, y)
  
  if (sub_ipums_hh %>% filter(URBAN == 2) %>% nrow == 0) {
    urb_samp_hh <- sub_ipums_hh %>%
      sample_n(nrow(urb_m), replace = T, weight = HHWT)
  } else {
    urb_samp_hh <- sub_ipums_hh %>%
      filter(URBAN == 2) %>%
      sample_n(nrow(urb_m), replace = T, weight = HHWT)
  }
  urb_samp_hh$x <- urb_m$x
  urb_samp_hh$y <- urb_m$y
  
  rur_samp_hh <- sub_ipums_hh %>%
    filter(URBAN == 1) %>%
    sample_n(nrow(rur_m), replace = T, weight = HHWT)
  rur_samp_hh$x <- rur_m$x
  rur_samp_hh$y <- rur_m$y
  
  gen_hh <- bind_rows(gen_hh, urb_samp_hh, rur_samp_hh)
}

if (!cfg$statewide_mode) {
  ext <- cfg$target_extent
  gen_hh <- gen_hh %>%
    filter(x >= ext[1], x <= ext[2]) %>%
    filter(y >= ext[3], y <= ext[4])
}

gen_hh$hid <- 1:nrow(gen_hh)

gen_pers <- gen_hh %>%
  left_join(ipums %>% 
              select(SERIAL, AGE, SEX, EMPSTAT = MX2015A_EMPSTAT, 
                     SCHSTAT = MX2015A_SCHSTAT,
                     SCHMUN = MX2015A_SCHMUN,
                     TRANTIMWORK = MX2015A_TRANTIMWORK,
                     WORKMUN = MX2015A_WORKMUN), 
            by = "SERIAL")
gen_pers$pid <- 1:nrow(gen_pers)

if (!dir.exists("synth/output")) dir.create("synth/output")
write_csv(gen_hh, "synth/output/gen_hh.csv")
write_csv(gen_pers, "synth/output/gen_pers.csv")
