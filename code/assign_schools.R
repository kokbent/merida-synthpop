rm(list = ls()[ls() != "cfg"])

# Building working pop vs available jobs data
ext <- extent(-89.75218, -89.50441, 20.84762, 21.07664)
pers <- data.table::fread("output/final_pers.csv")
hh <- data.table::fread("output/final_hh.csv")

subpop_hh <- hh %>%
  filter(x >= ext[1], x <= ext[2]) %>%
  filter(y >= ext[3], y <= ext[4])
subpop_pers <- pers %>%
  filter(hid %in% subpop_hh$hid)

sch_pers <- subpop_pers %>%
  filter(SCHSTAT < 999) %>%
  select(pid, hid)
sch_pers <- sch_pers %>%
  left_join(subpop_hh %>% select(hid, x, y))

sch_loc <- data.table::fread(cfg$wp_n_sch) %>%
  filter(type == "s") %>%
  filter(x >= ext[1], x <= ext[2]) %>%
  filter(y >= ext[3], y <= ext[4])
sch_loc$sid <- 1:nrow(sch_loc)

tmp <- assign_by_gravity(as.matrix(sch_pers[,c("x", "y")]),
                         as.matrix(sch_loc[,c("x", "y")]),
                         rep(1, nrow(sch_loc)),
                         5, 4326, steps = 1)

pids <- sch_pers$pid[tmp[,1]]
sids <- sch_loc$sid[tmp[,2]]
sch_assignment <- data.frame(pid = pids, sid = sids)
count <- table(sids) %>% as.data.frame
colnames(count) <- c("sid", "student")
count$sid <- as.character(count$sid) %>% as.numeric

sch_loc <- sch_loc %>%
  left_join(count) %>%
  mutate(student = ifelse(is.na(student), 0, student))
sch_loc <- sch_loc %>%
  select(type, x, y, sid, student)
sch_loc$worker <- ceiling(sch_loc$student / 10)

write_csv(sch_assignment, "output/sch_assignment.csv")
write_csv(sch_loc, "output/sch_locations.csv")
