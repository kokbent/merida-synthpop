rm(list = ls()[ls() != "cfg"])

set.seed(4342024)

ext <- extent(-89.75218, -89.50441, 20.84762, 21.07664)
pers <- data.table::fread("output/final_pers.csv")
hh <- read_csv("output/final_hh.csv")

subpop_hh <- hh %>%
  filter(x >= ext[1], x <= ext[2]) %>%
  filter(y >= ext[3], y <= ext[4])
subpop_pers <- pers %>%
  filter(hid %in% subpop_hh$hid)

work_pers <- subpop_pers %>%
  filter(EMPSTAT %/% 10 == 1, WORKMUN >= 31000 & WORKMUN < 32000) %>%
  filter(SCHSTAT == 999)
work_pers <- work_pers %>%
  left_join(subpop_hh %>% select(hid, x, y))

work_loc <- data.table::fread(cfg$wp_n_sch)
work_loc <- work_loc %>%
  filter(type != "s") %>%
  filter(x >= ext[1], x <= ext[2]) %>%
  filter(y >= ext[3], y <= ext[4])
sum(work_loc$work_size)
work_loc <- work_loc %>%
  rename(worker = work_size, student = sch_size)

sch_loc <- data.table::fread("output/sch_locations.csv")

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

data.table::fwrite(work_assignment, "output/work_assignment.csv")
data.table::fwrite(locs, "output/work_sch_locations.csv")
