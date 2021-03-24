library(sf)
library(raster)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(future)
library(jsonlite)
library(rgeos)
library(abmgravity)
cfg <- jsonlite::fromJSON("code/config.json")

source("code/create_hh_coord.R")
if (!is.null(cfg$addtnl_hhpers_script) | cfg$addtnl_hhpers_script != "NA") {
  source(cfg$addtnl_hhpers_script)
} else {
  file.copy("output/gen_pers.csv", "output/final_pers.csv")
  file.copy("output/gen_hh.csv", "output/final_hh.csv")
}
if (!is.null(cfg$age_correction_script) | cfg$age_correction_script != "NA") {
  source(cfg$age_correction_script)
}

source("code/assign_schools.R")
source("code/assign_work.R")
source("code/export_output.R")