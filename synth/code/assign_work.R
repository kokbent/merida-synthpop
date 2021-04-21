rm(list = ls()[ls() != "cfg"])

set.seed(4342024)

# ext <- extent(-89.75218, -89.50441, 20.84762, 21.07664)
pers <- fread("synth/output/final_pers.csv")
hh <- fread("synth/output/final_hh.csv")

if (cfg$statewide_mode) {
  work_loc <- data.table::fread(cfg$path_workplace)
} else {
  ext <- cfg$target_extent
  work_loc <- data.table::fread(cfg$path_workplace) %>%
    filter(x >= ext[1], x <= ext[2]) %>%
    filter(y >= ext[3], y <= ext[4])
}

work_pers <- pers %>%
  filter(EMPSTAT %/% 10 == 1, WORKMUN >= 31000 & WORKMUN < 32000) %>%
  filter(SCHSTAT == 999)
work_pers <- work_pers %>%
  left_join(hh %>% select(hid, x, y))

sum(work_loc$work_size)
work_loc <- work_loc %>%
  rename(worker = work_size)
work_loc$type = "w"

sch_loc <- fread("synth/output/sch_locations.csv")
sch_loc$type = "s"

locs <- bind_rows(work_loc, sch_loc)
locs$wid2 <- 1:nrow(locs)
sum(locs$worker)

## Scale down workplace sizes to match total number of workers
scale_factor <- nrow(work_pers) / sum(locs$worker) - 0.01 # Further scale down jobs with - 0.01
locs$worker <- round(locs$worker * scale_factor)
(sum(locs$worker) - nrow(work_pers)) / nrow(work_pers) # About 0.6% unfilled vacancy, acceptable

## Assign
tmp <- assign_by_gravity(as.matrix(work_pers[,c("x", "y")]),
                         as.matrix(locs[,c("x", "y")]),
                         locs$worker,
                         1000, 4326, 
                         min_x = -89.753,
                         min_y = 20.84750,
                         steps = 1, use_capacity = T)

pids <- work_pers$pid[tmp[,1]]
wids <- locs$wid2[tmp[,2]]

work_assignment <- data.frame(pid = pids, lid = wids)
locs <- locs %>%
  select(-wid) %>%
  rename(lid = wid2)

data.table::fwrite(work_assignment, "synth/output/work_assignment.csv")
data.table::fwrite(locs, "synth/output/work_sch_locations.csv")
