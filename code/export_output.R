rm(list = ls()[ls() != "cfg"])

ext <- extent(-89.75218, -89.50441, 20.84762, 21.07664)
pers <- data.table::fread("output/final_pers.csv")
hh <- data.table::fread("output/final_hh.csv")

subpop_hh <- hh %>%
  filter(x >= ext[1], x <= ext[2]) %>%
  filter(y >= ext[3], y <= ext[4])
subpop_pers <- pers %>%
  filter(hid %in% subpop_hh$hid)

work_sch <- data.table::fread("output/work_sch_locations.csv")
work_assignment <- data.table::fread("output/work_assignment.csv")
sch_assignment <- data.table::fread("output/sch_assignment.csv")

# Change sch_assignment to point to lid
sch_assignment <- sch_assignment %>%
  left_join(work_sch)
sch_assignment <- sch_assignment %>%
  select(pid, lid)

# Establish day_id in pers
subpop_pers <- subpop_pers %>%
  left_join(work_assignment) %>%
  left_join(sch_assignment, by = "pid")
subpop_pers %>%
  filter(!is.na(lid.x) & !is.na(lid.y)) # Sanity check make sure no one work and school at the same time
subpop_pers <- subpop_pers %>%
  mutate(day_id = coalesce(lid.x, lid.y),
         day_id = coalesce(day_id, -1L)) %>%
  select(-lid.x, -lid.y)

# Merge hh into locations, change hid in pers accordingly
subpop_hh1 <- subpop_hh %>%
  select(x, y, hid, arm = Arm, center = MZA3X3)
subpop_hh1$lid <- max(work_sch$lid) + 1:nrow(subpop_hh1)
subpop_pers <- subpop_pers %>%
  left_join(subpop_hh1 %>% select(hid, lid))
subpop_pers_fin <- subpop_pers %>%
  select(pid, home_id = lid, sex = SEX, age = AGE, day_id = day_id)

locs <- bind_rows(work_sch, subpop_hh1) %>%
  select(locid = lid, x, y, type, arm, center)
locs_fin <- locs %>%
  mutate(type = ifelse(is.na(type), "h", type),
         arm = ifelse(is.na(arm), 0, arm),
         center = ifelse(is.na(center), 0, center))

# 0 start everything
subpop_pers_fin <- subpop_pers_fin %>%
  mutate(pid = 0:(nrow(subpop_pers_fin)-1),
         home_id = home_id - 1,
         day_id = ifelse(day_id == -1, -1, day_id - 1))

locs_fin <- locs_fin %>%
  mutate(locid = locid - 1,
         type = ifelse(str_detect(type, "w"), "w", type))
locs_fin$type %>% table

# Network
network_fin <- subpop_pers_fin %>%
  filter(day_id != -1) %>%
  select(locid1 = home_id, locid2 = day_id)

# Reduce decimal points
locs_fin$x <- round(locs_fin$x, 5)
locs_fin$y <- round(locs_fin$y, 5)

# Export
data.table::fwrite(subpop_pers_fin, "output/population-merida.txt", sep = " ", scipen = 10)
data.table::fwrite(locs_fin, "output/locations-merida.txt", sep = " ", scipen = 10)
data.table::fwrite(network_fin, "output/network-merida.txt", sep = " ", scipen = 10)

png("fig/locations-merida.png", 2400, 2400, res = 300)
with(locs_fin[locs_fin$type == "h",], plot(x, y, pch = ".", col="#0000FF50", asp=1))
with(locs_fin[locs_fin$type == "w",], points(x, y, pch = ".", col="#FF000050"))
with(locs_fin[locs_fin$type == "s",], points(x, y, pch = 20, col="#00FF00", cex = 0.3))
dev.off()

ggplot(locs_fin) +
  geom_point(aes(x=x, y=y, colour=type, pch=type)) +
  scale_shape_manual(values = c(".", 20, "."))
