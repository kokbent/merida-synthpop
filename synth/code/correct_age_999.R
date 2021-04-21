#### Age correction (shift age 999 to something else!)
rm(list = ls()[ls() != "cfg"])

pers <- data.table::fread("output/final_pers.csv")
age_mat <- with(pers %>% filter(AGE != 999), 
               table(AGE, EMPSTAT))

ind <- which(pers$AGE == 999)
empstat <- pers$EMPSTAT[ind]

samp_age_by_empstat <- function (empstat, age_mat) {
  ind <- match(empstat, colnames(age_mat))
  samp <- sapply(ind, function (x) sample(as.numeric(rownames(age_mat)),
                                          size = 1,
                                          prob = age_mat[,x]))
  return(samp)
}

age <- samp_age_by_empstat(empstat, age_mat)
pers$AGE[ind] <- age

data.table::fwrite(pers, "output/final_pers.csv")
