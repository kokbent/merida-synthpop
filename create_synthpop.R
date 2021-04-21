library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(ipumsr)
library(future)
library(jsonlite)
library(rgeos)
library(abmgravity)
library(RSQLite)
source("synth/code/utils.R")

options(datatable.na.strings=c("", "NA")) # Making sure data.table properly reads NA

#### Getting args from Rscript, and displaying basics
args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 1) {
  cfg_file <- args
} else {
  cfg_file <- "config.json"
  cat("No config file supplied. Using default config.json.")
}

cat(paste0("Building synthpop based on config file from ", cfg_file, "\n"))
cfg <- jsonlite::fromJSON(cfg_file)
cfg <- make_full_path(cfg)

source("synth/code/create_hh_coord.R")
if (is.null(cfg$addtnl_hhpers_script)) {
  file.copy("synth/output/gen_pers.csv", "synth/output/final_pers.csv", overwrite = T)
  file.copy("synth/output/gen_hh.csv", "synth/output/final_hh.csv", overwrite = T)
} else if (cfg$addtnl_hhpers_script == "NA") {
  file.copy("synth/output/gen_pers.csv", "synth/output/final_pers.csv", overwrite = T)
  file.copy("synth/output/gen_hh.csv", "synth/output/final_hh.csv", overwrite = T)
} else {
  source(cfg$addtnl_hhpers_script)
}

if (!is.null(cfg$age_correction_script)) {
  if (cfg$age_correction_script != "NA") source(cfg$age_correction_script)
}

source("synth/code/assign_schools.R")
source("synth/code/assign_work.R")
source("synth/code/export_output.R")