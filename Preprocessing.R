#Data Processing
#Annegrete Peek
library(dplyr)
library(readxl)
library(zoo)

dt1 <- read_excel("tasutud_maksud_2017_I_kv.xlsx")
dt2 <- read_excel("tasutud_maksud_05.07.2017.xlsx")
dt3 <- read_excel("tasutud_maksud_09.10.2017.xlsx")
dt4 <- read_excel("tasutud_maksud_10.01.2018.xlsx")
dt1 <- dt1 %>% rename(Kaive = Käive, Tootajaid = `Töötajate arv`) %>% select(colnames(dt2)) %>% mutate(aeg = "2017-1")
dt2 <- dt2 %>% mutate(aeg = "2017-2")
dt3 <- dt3 %>% mutate(aeg = "2017-3")
dt4 <- dt4 %>% mutate(aeg = "2017-4")
dt <- dt1 %>% rbind(dt2) %>% rbind(dt3) %>% rbind(dt4) %>% mutate(aeg = as.yearqtr(aeg))
dt[,7:10][is.na(dt[,7:10])] <- 0
names(dt)[c(4,5,7:10)] <- c("KMK", "Tegevusvaldkond", "rmaksud", "tmaksud", "kaive", "tootajad")
dt$rmaksud_kaive <- dt$rmaksud/dt$kaive*100
dt$tmaksud_kaive <- dt$tmaksud/dt$kaive*100
dt$kaive_tootaja <- dt$kaive/dt$tootajad
dt$tmaksud_tootaja <- dt$tmaksud/dt$tootajad
dt[,12:13] <- round(dt[,12:13],1)
dt[,c(7:10,14:15)] <- round(dt[,c(7:10,14:15)])

save(dt, file = "bench_data.RDa")
