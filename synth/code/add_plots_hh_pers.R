rm(list = ls()[ls() != "cfg"])

library(raster)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(rgeos)
library(ipumsr)

ddi <- read_ipums_ddi(cfg$path_ipums_xml)
ipums <- read_ipums_micro(ddi) # .dat file needs to be in same folder as xml
ipums$CVE_MUN <- as.character(ipums$GEO2_MX2015) %>%
  str_sub(start = 3, end = 5)

tirs <- st_read("shp/TIRS_clusters_final.shp", stringsAsFactors = F)
ipums <- ipums %>%  # Pre-filtering to save time
  filter(CVE_MUN %in% unique(tirs$CVE_MUN), URBAN == 2)

gen_hh <- data.table::fread("synth/output/gen_hh.csv")
gen_pers <- data.table::fread("synth/output/gen_pers.csv")

hh <- st_as_sf(gen_hh[,c("x", "y", "hid")], coords = c("x", "y"))
hh <- st_set_crs(hh, 4326) %>%
  st_transform(st_crs(tirs))

tmp <- st_join(hh, tirs)

hh_del <- tmp %>%
  filter(!is.na(CVE_MUN)) # Up to 29000 hh
hh_keep <- gen_hh %>%
  filter(!hid %in% hh_del$hid)

samp_child <- function (df, n_hh, n_child, age_lo, age_hi) {
  child <- df %>%
    dplyr::filter(AGE >= age_lo, AGE <= age_hi) %>%
    sample_n(n_child, replace = T, weight = PERWT)
  child$hid <- sample(1:n_hh, nrow(child), replace = T)
  
  return(child)
}

samp_child_agedist <- function (df, n_hh, age_vec, age_mat = NULL) {
  if (is.null(age_mat)) {
    age_mat <- matrix(c(0, 3, 6, 12, 15, 2, 5, 11, 14, 17), ncol = 2)
  }
  
  ind <- which(!is.na(age_vec))
  if (length(ind) == 0) return(data.frame())
  age_vec <- age_vec[ind]
  age_mat <- age_mat[ind,,drop=F]
  
  samp <- 1:length(age_vec) %>%
    map_dfr(~ samp_child(df, n_hh, age_vec[.x], age_mat[.x, 1], age_mat[.x, 2]))
  
  return(samp)
}

plot_hh <- plot_pers <- data.frame()
hid_counter <- max(gen_hh$hid)
for (i in 1:nrow(tirs)) {
  if (i %% 100 == 0) print(i)
  sub_tirs <- tirs[i,]
  sub_ipums <- ipums %>%
    filter(CVE_MUN == sub_tirs$CVE_MUN, URBAN == 2)
  n_hh <- sub_tirs$HOGAR1
  
  # Adult sampling
  # If n_hh is -6, assume maximum of 2 hh and min of 0 hh depending of number of adult
  # If n_hh is -8, assume 0 hh
  n_adult <- sub_tirs$Pobtotal - sub_tirs$Pob0a17
  if (n_hh == -6 & n_adult >= 2) n_hh <- 2
  if (n_hh == -6 & n_adult == 1) n_hh <- 1
  if (n_hh == -6 & n_adult == 0) next
  if (n_hh == -8 | n_hh == 0) next
  
  sub_ipums_adult <- sub_ipums %>%
    filter(AGE > 17)
  ad1 <- sub_ipums_adult %>%
    sample_n(n_hh, replace = T, weight = PERWT)
  ad2 <- sub_ipums_adult %>%
    sample_n(n_adult - n_hh, replace = T, weight = PERWT)
  ad1$hid <- 1:n_hh
  ad2$hid <- sample(1:n_hh, nrow(ad2), replace = T)
  
  # Child sampling
  age_vec <- sub_tirs %>% select(dplyr::contains("Pob")) %>% st_drop_geometry() %>% as.matrix
  age_vec <- age_vec[,-c(1, 2)] %>% as.numeric()
  child <- samp_child_agedist(sub_ipums, n_hh, age_vec)
  
  sub_pers <- bind_rows(ad1, ad2, child) %>%
    select(SERIAL, AGE, SEX, EMPSTAT = MX2015A_EMPSTAT, 
           SCHSTAT = MX2015A_SCHSTAT,
           SCHMUN = MX2015A_SCHMUN,
           TRANTIMWORK = MX2015A_TRANTIMWORK,
           WORKMUN = MX2015A_WORKMUN,
           hid)
  sub_pers$hid <- sub_pers$hid + hid_counter
  
  # Household sampling
  sub_xy <- st_sample(sub_tirs, n_hh) %>%
    st_transform(4326) %>%
    st_coordinates()
  sub_hh <- cbind(data.frame(hid = 1:n_hh), sub_xy)
  sub_hh$hid <- sub_hh$hid + hid_counter
  sub_hh$Arm <- sub_tirs$Arm
  sub_hh$MZA3X3 <- sub_tirs$MZA3X3
  sub_hh$CVE_MUN <- sub_tirs$CVE_MUN
  sub_hh$URBAN <- 2
  sub_hh <- sub_hh %>%
    rename(x = X, y = Y)
  
  hid_counter <- hid_counter + n_hh
  
  plot_hh <- bind_rows(plot_hh, sub_hh)
  plot_pers <- bind_rows(plot_pers, sub_pers)
}

head(plot_hh)
plot_hh$Arm <- ifelse(plot_hh$Arm == "A", 1, 2)
plot_hh$MZA3X3 <- as.numeric(plot_hh$MZA3X3)
plot_hh$CVE_MUN <- as.numeric(plot_hh$CVE_MUN)
plot_hh$SERIAL <- NA
hh_keep$Arm <- 0
hh_keep$MZA3X3 <- 0
final_hh <- bind_rows(hh_keep, plot_hh)

plot_pers <- plot_pers %>%
  arrange(hid)
plot_pers$pid <- max(gen_pers$pid) + 1:nrow(plot_pers)
keep_pers <- gen_pers %>%
  select(-c(HHWT, URBAN, CVE_MUN, x, y)) %>%
  filter(!hid %in% hh_del$hid)
keep_pers$SERIAL <- as.numeric(keep_pers$SERIAL)
final_pers <- bind_rows(keep_pers, plot_pers)

#### Fix hid and pid
final_hh$hid2 <- 1:nrow(final_hh)
final_pers <- final_pers %>%
  left_join(final_hh %>% select(hid, hid2)) %>%
  select(-hid) %>%
  rename(hid = hid2)
final_pers$pid <- 1:nrow(final_pers)
final_hh <- final_hh %>%
  select(-hid) %>%
  rename(hid = hid2,
         Center = MZA3X3)

fwrite(final_hh, "synth/output/final_hh.csv")
fwrite(final_pers, "synth/output/final_pers.csv")
