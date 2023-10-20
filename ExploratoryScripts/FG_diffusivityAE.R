source(file.path("R/MO_Length.R"))
#load in site 30min data
sitecode <- 'KONZ'
load(paste0("data/", sitecode,"_30m.Rdata"))
#filter for good data
Press <- m30.list$Press %>% 
  filter(qfFinl == "0") %>%
  select(timeBgn, mean) %>%
  rename(press = mean)
  
Tair <- m30.list$Tair %>% 
  filter(qfFinl == "0") %>%
  filter(TowerPosition == "4") %>%
  select(timeBgn, mean) %>%
  rename(tair = mean)

F_LE <- m30.list$F_LE %>% 
  filter(turb.qfFinl == "0") %>%
  select(timeBgn, turb) %>%
  rename(F_LE = turb)
F_H <- m30.list$F_H %>% 
  filter(turb.qfFinl == "0") %>%
  select(timeBgn, turb) %>%
  rename(F_H = turb)
uStar <- m30.list$Ufric %>%
  filter(qfFinl == "0") %>%
  select(timeBgn, veloFric) %>%
  rename(ustar = veloFric)
#combine data by time
data.all <- uStar %>%
  left_join(Press, by = c("timeBgn")) %>%
  left_join(Tair, by = c("timeBgn")) %>%
  left_join(F_LE, by = c("timeBgn")) %>%
  left_join(F_H, by = c("timeBgn"))
  
#remove timesteps with NAs
data.na <- na.omit(data.all)
#calculate obukov length
MO.vars <- MOlength(press = data.na$press, temp = data.na$tair, H = data.na$F_H, LE = data.na$F_LE, velofric = data.na$ustar)
#grab canopy height
MO.param <- (as.numeric(attr.df$DistZaxsTow) - as.numeric(attr.df$DistZaxsDisp))/as.numeric(MO.vars$L)

