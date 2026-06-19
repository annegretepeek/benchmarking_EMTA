#Data Processing
#Annegrete Peek
library(dplyr)
library(readxl)
library(zoo)

dt1 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_2017_I_kv.xlsx")
dt2 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_05.07.2017.xlsx")
dt3 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_09.10.2017.xlsx")
dt4 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_10.01.2018.xlsx")
dt5 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_2018_i_kvartal.xlsx")
dt1 <- dt1 %>% rename(Kaive = Käive, Tootajaid = `Töötajate arv`) %>% select(colnames(dt2)) %>% mutate(aeg = "2017-1")
dt2 <- dt2 %>% mutate(aeg = "2017-2")
dt3 <- dt3 %>% mutate(aeg = "2017-3")
dt4 <- dt4 %>% mutate(aeg = "2017-4")
dt5 <- dt5 %>% mutate(aeg = "2018-1")
dt <- dt1 %>% rbind(dt2) %>% rbind(dt3) %>% rbind(dt4) %>% rbind(dt5) %>% mutate(aeg = as.yearqtr(aeg))
dt[,7:10][is.na(dt[,7:10])] <- 0
names(dt)[c(4,5,7:10)] <- c("KMK", "Tegevusvaldkond", "rmaksud", "tmaksud", "kaive", "tootajad")
dt$rmaksud_kaive <- dt$rmaksud/dt$kaive*100
dt$tmaksud_kaive <- dt$tmaksud/dt$kaive*100
dt$kaive_tootaja <- dt$kaive/dt$tootajad
dt$tmaksud_tootaja <- dt$tmaksud/dt$tootajad
dt[,12:13] <- round(dt[,12:13],1)
dt[,c(7:10,14:15)] <- round(dt[,c(7:10,14:15)])

save(dt, file = "C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")

#Uued andmed
load("C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")
dt6 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_2018_ii_kvartal.xlsx")
dt6 <- dt6 %>% mutate(aeg = "2018-2") %>% mutate(aeg = as.yearqtr(aeg))
dt6[,7:10][is.na(dt6[,7:10])] <- 0
names(dt6)[c(4,5,7:10)] <- c("KMK", "Tegevusvaldkond", "rmaksud", "tmaksud", "kaive", "tootajad")
dt6$rmaksud_kaive <- dt6$rmaksud/dt6$kaive*100
dt6$tmaksud_kaive <- dt6$tmaksud/dt6$kaive*100
dt6$kaive_tootaja <- dt6$kaive/dt6$tootajad
dt6$tmaksud_tootaja <- dt6$tmaksud/dt6$tootajad
dt6[,12:13] <- round(dt6[,12:13],1)
dt6[,c(7:10,14:15)] <- round(dt6[,c(7:10,14:15)])
dt <- dt %>% rbind(dt6)
save(dt, file = "C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")

load("C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")
dt6 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_2018_iii_kvartal.xlsx")
dt6 <- dt6 %>% mutate(aeg = "2018-3") %>% mutate(aeg = as.yearqtr(aeg))
dt6[,7:10][is.na(dt6[,7:10])] <- 0
names(dt6)[c(4,5,7:10)] <- c("KMK", "Tegevusvaldkond", "rmaksud", "tmaksud", "kaive", "tootajad")
dt6$rmaksud_kaive <- dt6$rmaksud/dt6$kaive*100
dt6$tmaksud_kaive <- dt6$tmaksud/dt6$kaive*100
dt6$kaive_tootaja <- dt6$kaive/dt6$tootajad
dt6$tmaksud_tootaja <- dt6$tmaksud/dt6$tootajad
dt6[,12:13] <- round(dt6[,12:13],1)
dt6[,c(7:10,14:15)] <- round(dt6[,c(7:10,14:15)])
dt <- dt %>% rbind(dt6)
save(dt, file = "C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")

load("C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")
dt6 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_2018_iv_kvartal.xlsx")
dt6 <- dt6 %>% mutate(aeg = "2018-4") %>% mutate(aeg = as.yearqtr(aeg))
dt6[,7:10][is.na(dt6[,7:10])] <- 0
names(dt6)[c(4,5,7:10)] <- c("KMK", "Tegevusvaldkond", "rmaksud", "tmaksud", "kaive", "tootajad")
dt6$rmaksud_kaive <- dt6$rmaksud/dt6$kaive*100
dt6$tmaksud_kaive <- dt6$tmaksud/dt6$kaive*100
dt6$kaive_tootaja <- dt6$kaive/dt6$tootajad
dt6$tmaksud_tootaja <- dt6$tmaksud/dt6$tootajad
dt6[,12:13] <- round(dt6[,12:13],1)
dt6[,c(7:10,14:15)] <- round(dt6[,c(7:10,14:15)])
dt <- dt %>% rbind(dt6)
save(dt, file = "C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")

load("C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")
dt6 <- read_excel("C:/Users/annegrete.peek/Documents/EMTA/Company_compare/tasutud_maksud_2019_iv_kvartal.xlsx") %>% 
  arrange(-`Riiklikud Maksud`)
dt6 <- dt6 %>% mutate(aeg = "2019-4") %>% mutate(aeg = as.yearqtr(aeg))
dt6[,7:10][is.na(dt6[,7:10])] <- 0
names(dt6)[c(4,5,7:10)] <- c("KMK", "Tegevusvaldkond", "rmaksud", "tmaksud", "kaive", "tootajad")
dt6$rmaksud_kaive <- dt6$rmaksud/dt6$kaive*100
dt6$tmaksud_kaive <- dt6$tmaksud/dt6$kaive*100
dt6$kaive_tootaja <- dt6$kaive/dt6$tootajad
dt6$tmaksud_tootaja <- dt6$tmaksud/dt6$tootajad
dt6[,12:13] <- round(dt6[,12:13],1)
dt6[,c(7:10,14:15)] <- round(dt6[,c(7:10,14:15)])
dt <- dt %>% rbind(dt6)
save(dt, file = "C:/Users/annegrete.peek/Documents/EMTA/Benchmarking/bench_data.RDa")
