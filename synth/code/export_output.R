rm(list = ls()[ls() != "cfg"])

source("synth/code/utils.R")
pers <- data.table::fread("synth/output/final_pers.csv")
hh <- data.table::fread("synth/output/final_hh.csv")

work_sch <- data.table::fread("synth/output/work_sch_locations.csv")
work_assignment <- data.table::fread("synth/output/work_assignment.csv")
sch_assignment <- data.table::fread("synth/output/sch_assignment.csv")

# Database stuff
schema <- json_to_sql_schema(cfg$db_schema)
extra <- check_extra(cfg$db_schema)

dirname <- paste0("synth/sim_pop-", cfg$output_name, "-1.1")
sqlitename <- paste0(dirname, paste0("/sim_pop-", cfg$output_name, "-1.1.sqlite"))
dir.create(dirname)
mydb <- dbConnect(RSQLite::SQLite(), sqlitename)

for (i in 1:length(schema)) dbExecute(mydb, schema[i])

# Locations (loc)
work_sch1 <- work_sch %>%
  select(locid = lid, x, y, type, any_of(extra))
hh$locid <- max(work_sch1$locid) + 1:nrow(hh)
hh$type <- "h"
hh1 <- hh %>%
  select(locid, x, y, type, any_of(extra))
loc_db <- bind_rows(work_sch1, hh1)
dbWriteTable(mydb, "loc", loc_db, append = T)

# Persons (pers)
pers_db <- pers %>%
  select(pid, hid, sex = SEX, age = AGE)
dbWriteTable(mydb, "pers", pers_db, append = T)

# Households (hh)
hh_db <- hh %>%
  select(hid, ipums_serial = SERIAL)
dbWriteTable(mydb, "hh", hh_db, append = T)
             
# Schools (sch)
sch_db <- work_sch %>%
  filter(type == "s") %>%
  select(sid, locid = lid, student, worker)
dbWriteTable(mydb, "sch", sch_db, append = T)

# Workplaces (wp)
wp_db <- work_sch %>%
  filter(type == "w") %>%
  select(locid = lid, worker)
dbWriteTable(mydb, "wp", wp_db, append = T)

# Reside (reside)
reside_db <- pers_db %>%
  left_join(hh %>% select(hid, locid, type), by = "hid") %>%
  select(pid, locid, type)
dbWriteTable(mydb, "reside", reside_db, append = T)

# Movement (movement)
movement1 <- sch_assignment %>%
  left_join(sch_db %>% select(sid, locid), by = "sid") %>%
  select(pid, locid) %>%
  mutate(type = "s")

movement2 <- work_assignment %>%
  select(pid, locid = lid) %>%
  mutate(type = "w")

movement_db <- bind_rows(movement1, movement2)
dbWriteTable(mydb, "movement", movement_db, append = T)

# Zip the sqlite up
dbDisconnect(mydb)

tgzname <- paste0("sim_pop-", cfg$output_name, "-1.1.tgz")
filesname <- str_split(dirname, "/")[[1]][2]
setwd("synth/")
tar(tgzname, files = filesname, compression = "gzip")
unlink(filesname, recursive = T)
setwd("../")
