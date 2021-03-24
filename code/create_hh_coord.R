rm(list = ls()[ls() != "cfg"])

source("code/utils.R")

### Some initial stuff
planar_crs <- CRS(cfg$planar_crs)
ext <- extent(-89.75218, -89.50441, 20.84762, 21.07664)

ipums <- read_rds(cfg$ipums_rds)
ipums$CVE_MUN <- as.character(ipums$GEO2_MX2015) %>%
  str_sub(start = 3, end = 5)

### Population (Slice Mexico raster to Yucatan)
r <- slice_raster(cfg$pop_raster, cfg$domain_shp)

### Dealing with urban census block group
blocshp <- st_read(cfg$urb_cbg_shp)
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
rur_ageb_shp <- st_read(cfg$rur_cbg_shp)
rur_ageb_sp <- as(rur_ageb_shp, "Spatial")
plan(multiprocess, workers = 5)
system.time(rur_ageb_pop <- extract_mc(r, rur_ageb_sp, fun = sum, na.rm = T))
rur_ageb_shp$pop <- rur_ageb_pop

rur_ageb_shp <- rur_ageb_shp %>%
  filter(!is.na(pop)) %>%
  mutate(HH = round((rur_tot / sum(pop)) * (pop / rur_pers_ph)))
rur_hh <- weighted_spatsamp(r, rur_ageb_shp, rur_ageb_shp$HH)

rur_hh$CVE_MUN <- rur_ageb_shp$CVE_MUN[rur_hh$id]
rur_hh$URBAN <- "R"

#### Plot
ggplot() +
  geom_point(aes(x=x, y=y), data = urb_hh1, pch = ".") +
  geom_point(aes(x=x, y=y), data = rur_hh, col = "blue", pch = ".") +
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

gen_hh$hid <- 1:nrow(gen_hh)
gen_hh

gen_pers <- gen_hh %>%
  left_join(ipums %>% 
              select(SERIAL, AGE, SEX, EMPSTAT = MX2015A_EMPSTAT, 
                     SCHSTAT = MX2015A_SCHSTAT,
                     SCHMUN = MX2015A_SCHMUN,
                     TRANTIMWORK = MX2015A_TRANTIMWORK,
                     WORKMUN = MX2015A_WORKMUN), 
            by = "SERIAL")
gen_pers$pid <- 1:nrow(gen_pers)

if (!dir.exists("output")) dir.create("output")
write_csv(gen_hh, "output/gen_hh.csv")
write_csv(gen_pers, "output/gen_pers.csv")

#### Junk
# 
# hh_agg_mun <- ipums %>%
#   group_by(CVE_MUN, URBAN, SERIAL) %>% 
#   summarise(hh_count = mean(HHWT)) %>%
#   group_by(CVE_MUN, URBAN) %>%
#   summarise(hh_count = sum(hh_count))
# hh_agg_mun %>% filter(CVE_MUN == "050")
# 
# #### Start with establishing shapes and rasters
# munshp <- st_read("shp/yuc_municipio.shp")
# # ggplot(munshp) + geom_sf() + xlim(ext[1:2]) + ylim(ext[3:4])
# 
# blocshp <- st_read("shp/yuc_manzana.shp")
# # ggplot(blocshp) + geom_sf()+ xlim(c(-89.75, -89.7)) + ylim(c(20.85, 20.9))
# 
# rur_ageb_shp <- st_read("shp/yuc_ageb_rural.shp")
# 
# yucshp <- st_read("shp/31ent_new.shp") %>%
#   st_transform(4326)
# 
# mex_pop <- raster("data/mex_ppp_2020_UNadj.tif")
# yuc_pop <- crop(mex_pop, as(yucshp, "Spatial"))
# yuc_pop <- mask(yuc_pop, as(yucshp, "Spatial"))
# plot(yuc_pop)
# 
# blocshp_sp <- as(blocshp, "Spatial")
# bloc_pop <- raster::extract(yuc_pop, blocshp_sp, fun = sum, na.rm = T)
# blocshp$bloc_pop <- bloc_pop
# 
# #### Person per household
# urb_pers_ph <- ipums %>%
#   filter(URBAN == 2) %>%
#   summarise(pph = n() / length(unique(SERIAL))) %>%
#   .$pph
# rur_pers_ph <- ipums %>%
#   filter(URBAN == 1) %>%
#   summarise(pph = n() / length(unique(SERIAL))) %>%
#   .$pph
# 
# urb_tot <- ipums %>%
#   filter(URBAN == 2) %>%
#   summarise(TOTALPOP = sum(PERWT)) %>%
#   .$TOTALPOP
# 
# rur_tot <- ipums %>%
#   filter(URBAN == 1) %>%
#   summarise(TOTALPOP = sum(PERWT)) %>%
#   .$TOTALPOP
# 
# ####
# urb_blocshp <- blocshp %>%
#   filter(AMBITO == "U") %>%
#   mutate(HH = ceiling((urb_tot / sum(bloc_pop)) * (bloc_pop / urb_pers_ph)))
# urb_blocshp_not_tirs <- urb_blocshp %>%
#   left_join(tirs %>% select(CVE_MUN, CVE_LOC, CVE_AGEB, CVE_MZA, Pobtotal) %>% st_set_geometry(NULL))
# urb_blocshp_not_tirs <- urb_blocshp_not_tirs %>%
#   filter(is.na(Pobtotal))
# urb_blocshp_not_tirs <- st_transform(urb_blocshp_not_tirs, st_crs(tirs))
# 
# urb_hh <- st_sample(urb_blocshp_not_tirs, urb_blocshp_not_tirs$HH)
# urb_hh1 <- st_join(st_as_sf(urb_hh), tirs)
# ind <- which(is.na(urb_hh1$CVEGEO))
# urb_hh1 <- urb_hh[ind,]
# urb_hh1 <- st_transform(urb_hh1, st_crs(munshp))
# urb_hh1 <- st_join(st_as_sf(urb_hh1), munshp)
# 
# 
# ####
# rur_ageb_sp <- as(rur_ageb_shp, "Spatial")
# rur_ageb_pop <- raster::extract(yuc_pop, rur_ageb_sp, fun = sum, na.rm = T)
# rur_ageb_shp$pop <- rur_ageb_pop
# 
# rur_ageb_shp <- rur_ageb_shp %>%
#   filter(!is.na(pop)) %>%
#   mutate(HH = round((rur_tot / sum(pop)) * (pop / rur_pers_ph)))
# 
# ####
# yuc_pop_df <- yuc_pop %>%
#   as.data.frame(xy = T)
# yuc_pop_df <- yuc_pop_df %>%
#   filter(!is.na(mex_ppp_2020_UNadj))
# 
# pop_xy <- SpatialPoints(yuc_pop_df[,c("x", "y")], 
#                         proj4string = crs(rur_ageb_sp))
# 
# ageb_xy <- over(pop_xy, rur_ageb_sp)
# yuc_pop_df <- cbind(yuc_pop_df, ageb_xy)
# 
# cond <- !is.na(yuc_pop_df$CVE_MUN)
# yuc_pop_df <- yuc_pop_df[cond,]
# pop_xy <- pop_xy[which(cond)]
# 
# ####
# xdiff <- 0.0008333 # pixel size
# ydiff <- 0.0008333
# create_hhcoord <- function (x, y, xdiff, ydiff) {
#   hh_x <- x
#   hh_y <- y
#   hh_x <- hh_x + runif(length(x), -xdiff/2, xdiff/2)
#   hh_y <- hh_y + runif(length(y), -ydiff/2, ydiff/2)
#   
#   return(data.frame(x = hh_x, y = hh_y))
# }
# 
# ####
# rur_hh <- data.frame()
# for (i in 1:nrow(rur_ageb_shp)) {
#   if (rur_ageb_shp$HH[i] == 0) next
#   
#   sub_pop <- yuc_pop_df %>%
#     filter(CVE_MUN == rur_ageb_shp$CVE_MUN[i] & CVE_AGEB == rur_ageb_shp$CVE_AGEB[i])
#   s <- sample(1:nrow(sub_pop), size = rur_ageb_shp$HH[i], replace = T, prob = sub_pop$mex_ppp_2020_UNadj)
#   
#   df <- create_hhcoord(sub_pop$x[s], sub_pop$y[s], xdiff, ydiff)
#   df$CVE_MUN <- rur_ageb_shp$CVE_MUN[i]
#   rur_hh <- bind_rows(rur_hh, df)
# }
# 
# rur_hh1 <- st_as_sf(rur_hh, coords = c("x", "y"))
# rur_hh1 <- st_set_crs(rur_hh1, 4326)
# 
# ggplot() +
#   geom_sf(data = urb_hh1, pch = ".") +
#   geom_sf(data = rur_hh1, col = "blue", pch = ".")
# 
# #### Assign these coordinates to IPUMS households
# ipums_hh <- ipums %>%
#   select(SERIAL, HHWT, URBAN, CVE_MUN) %>%
#   distinct()
# gen_hh <- data.frame()
# for (m in (unique(munshp$CVE_MUN) %>% sort)) {
#   print(m)
#   sub_ipums_hh <- ipums_hh %>%
#     filter(CVE_MUN == m)
#   
#   urb_m <- urb_hh1 %>%
#     filter(CVE_MUN == m) %>%
#     st_coordinates()
#   rur_m <- rur_hh %>%
#     filter(CVE_MUN == m)
#   
#   if (sub_ipums_hh %>% filter(URBAN == 2) %>% nrow == 0) {
#     urb_samp_hh <- sub_ipums_hh %>%
#       sample_n(nrow(urb_m), replace = T, weight = HHWT)
#   } else {
#     urb_samp_hh <- sub_ipums_hh %>%
#       filter(URBAN == 2) %>%
#       sample_n(nrow(urb_m), replace = T, weight = HHWT)
#   }
#   urb_samp_hh$x <- urb_m[,1]
#   urb_samp_hh$y <- urb_m[,2]
#   
#   rur_samp_hh <- sub_ipums_hh %>%
#     filter(URBAN == 1) %>%
#     sample_n(nrow(rur_m), replace = T, weight = HHWT)
#   rur_samp_hh$x <- rur_m[,1]
#   rur_samp_hh$y <- rur_m[,2]
#   
#   gen_hh <- bind_rows(gen_hh, urb_samp_hh, rur_samp_hh)
# }
# 
# gen_hh$hid <- 1:nrow(gen_hh)
# gen_hh
# 
# gen_pers <- gen_hh %>%
#   left_join(ipums %>% 
#               select(SERIAL, AGE, SEX, EMPSTAT = MX2015A_EMPSTAT, 
#                      SCHSTAT = MX2015A_SCHSTAT,
#                      SCHMUN = MX2015A_SCHMUN,
#                      TRANTIMWORK = MX2015A_TRANTIMWORK,
#                      WORKMUN = MX2015A_WORKMUN), 
#             by = "SERIAL")
# gen_pers$pid <- 1:nrow(gen_pers)
# 
# if (!dir.exists("output")) dir.create("output")
# write_csv(gen_hh, "output/gen_hh.csv")
# write_csv(gen_pers, "output/gen_pers.csv")
