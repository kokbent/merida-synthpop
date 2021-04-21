rm(list=ls())

if (!require("dplyr")) stop("dplyr package is required but not installed.")
if (!require("data.table")) stop("readr package is required but not installed.")
if (!require("RSQLite")) stop("RSQLite package is required but not installed.")
if (!require("stringr")) stop("stringr package is required but not installed.")
version <- "1.1"

#### Getting Rscript args
file_regex <- paste0("sim_pop-(.*)-", version, ".tgz")
arg <- commandArgs(trailingOnly=TRUE)
arg <- trimws(arg)

if (length(arg) < 1) {
  stop("Argument (target tgz file name) is required.")
} else if (length(arg) > 1) {
  stop("Too many argument.")
} else if (str_detect(arg, "/")) {
  stop("Target tgz file should be in the same folder of this script, and you should execute this script
       in its directory, so argument with '/' character is likely inappropriate.")
} else if (!str_detect(arg, file_regex)) {
  stop("Argument is not a valid sim_pop ", version, " tgz file.")
} else {
  tgz_file <- arg
}

#### Splicing args
domain <- str_match(tgz_file, file_regex)[2]
folder <- str_remove(tgz_file, ".tgz")
sqlite_file <- str_replace(tgz_file, ".tgz", ".sqlite")
out_folder <- paste0("sim_pop-", domain, "/")
dir.create(out_folder)

#### Unpack
message("Unpacking files...")
if (!file.exists(tgz_file)) stop("tgz file must be in the same folder as this script.")
untar(tgz_file, exdir=".")

#### Connect
con <- DBI::dbConnect(RSQLite::SQLite(), paste0(folder, "/", sqlite_file))

#### Pulling query using SQL
## Locations
sql <- "SELECT * FROM loc"
loc <- dbGetQuery(con, sql)

## Persons
sql <- 
"SELECT pers.pid, reside.locid AS home_id, pers.sex, pers.age, 
  movement.locid AS day_id
FROM pers
LEFT JOIN reside ON pers.pid = reside.pid
LEFT JOIN movement ON pers.pid = movement.pid"
pers <- dbGetQuery(con, sql)

## Zero indexing loc and pers
loc$locid <- loc$locid - 1
pers[,c("pid", "home_id", "day_id")] <- pers[,c("pid", "home_id", "day_id")] - 1

## Network (This is different from synthpop FL and is likely redundant)
network <- pers %>%
  filter(!is.na(pers$day_id)) %>%
  select(locid1 = home_id, locid2 = day_id)

#### Housekeeping and export
## Reduce decimal points
loc$x <- round(loc$x, 5)
loc$y <- round(loc$y, 5)

## Export
data.table::fwrite(pers, paste0(out_folder, "population-merida.txt"), 
                   sep = " ", scipen = 10)
data.table::fwrite(loc, paste0(out_folder, "locations-merida.txt"), 
                   sep = " ", scipen = 10)
data.table::fwrite(network, paste0(out_folder, "network-merida.txt"), 
                   sep = " ", scipen = 10)

## Visualizing locs
png(paste0(out_folder, "locations-merida.png"), 2400, 2400, res = 300)
with(loc[loc$type == "h",], plot(x, y, pch = ".", col="#0000FF50", asp=1))
with(loc[loc$type == "w",], points(x, y, pch = ".", col="#FF000050"))
with(loc[loc$type == "s",], points(x, y, pch = 20, col="#00FF00", cex = 0.3))
dev.off()


#### Finish and kill the unpacked files
message("Removing unpacked files...")
dbDisconnect(con)
unlink(folder, recursive = T)
message("Done.")
