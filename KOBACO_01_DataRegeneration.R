#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KOBACO Project 01. Data Regeneration
##  goal : Import raw KOBACO sets and transform them into analyzable form
##  Data set: KOBACO
##  Notice :
#######################################################################

library(dplyr)
library(magrittr)
library(lubridate)

year <- 2023

### SWD
temp.list <- 
  list.files(paste0("../6.김경외 연구자님/",year,"/t_tv_live_",year,"/SWD/"))

swd.df <- 0
for(i in 1:length(temp.list)){
  temp <- read.table(paste0("../6.김경외 연구자님/",year,"/t_tv_live_",year,"/SWD/",temp.list[i]))
  swd.df <- swd.df %>%
    rbind(temp %>% mutate(HouseholdID = substr(V1,1,28), IndividualID = substr(V1,29,30),
                        Channel = substr(V1,31,34), StartTime = substr(V1, 35,40),
                        EndTime = substr(V1, 41,46), TV= substr(V1, 47,47),
                        AudienceType=substr(V1, 48,48)) %>%
            select(-c("V1")) %>% mutate(Date=ymd(substr(temp.list[i],3,8))) %>% 
            mutate(Year=year(Date), Month=month(Date), Day=day(Date), 
                   Day_label=wday(Date, label=TRUE)) %>% select(-c("Date")))
            # mutate(Year=substr(temp.list[i],3,4),
            #                             Month=substr(temp.list[i],5,6),
            #                             Day=substr(temp.list[i],7,8)))
  print(i)
}
swd.df <- swd.df[2:nrow(swd.df),]
save(swd.df, file=paste0("R file/swd.",year,".RData"))
rm(swd.df)

### WEH
temp.list <- 
  list.files(paste0("../6.김경외 연구자님/",year,"/t_tv_live_",year,"/DEM_WEH/"),
             pattern=".weh")

weh.df <- 0
for(i in 1:length(temp.list)){
  temp <- read.table(paste0("../6.김경외 연구자님/",year,"/t_tv_live_",year,"/DEM_WEH/",temp.list[i]))
  weh.df <- weh.df %>%
    rbind(temp %>% mutate(HouseholdID = substr(V1,1,28), NWeight = substr(V1,29,30),
                          Reg = substr(V1,31,33), Weight = as.numeric(substr(V1, 34,41))) %>%
            select(-c("V1")) %>% mutate(Date=ymd(substr(temp.list[i],3,8))) %>% 
            mutate(Year=year(Date), Month=month(Date), Day=day(Date), 
                   Day_label=wday(Date, label=TRUE)) %>% select(-c("Date")))
  print(i)
}
weh.df <- weh.df[2:nrow(weh.df),]
save(weh.df, file=paste0("R file/weh.",year,".RData"))

### DEM
temp.list <- 
  list.files(paste0("../6.김경외 연구자님/",year,"/t_tv_live_",year,"/DEM_WEH/"),
             pattern=".dem")

dem.df <- 0
for(i in 1:length(temp.list)){
  temp <- read.table(paste0("../6.김경외 연구자님/",year,"/t_tv_live_",year,"/DEM_WEH/",temp.list[i]))
  dem.df <- dem.df %>%
    rbind(temp %>% mutate(HouseholdID = substr(V1,1,28), IndividualID = substr(V1,29,30),
                          IndWeight = as.numeric(substr(V1,31,38)), Reg = substr(V1,39,39)) %>%
            select(-c("V1")) %>% mutate(Date=ymd(substr(temp.list[i],3,8))) %>% 
            mutate(Year=year(Date), Month=month(Date), Day=day(Date), 
                   Day_label=wday(Date, label=TRUE)) %>% select(-c("Date")) %>%  
            rename(Sex=V2, Job=V3, Household=V4, Teen=V6) %>%
            mutate(Machines=substr(V5,1,1), Cable=substr(V5,2,2), 
                   SkyLife=substr(V7,1,1),IPTV=substr(V7,2,2)) %>%
            select(HouseholdID, IndividualID, IndWeight, Reg, Sex, Job,
                   Household, Teen, Machines, Cable, SkyLife, IPTV, 
                   Year, Month, Day, Day_label))
  
  print(i)
}
dem.df <- dem.df[2:nrow(dem.df),]
save(dem.df, file=paste0("R file/dem.",year,".RData"))

### Create integrated weh.df
# year <- 2022
# load(file=paste0("R file/weh.",year,".RData"))
# temp <- weh.df
# year <- 2023
# load(file=paste0("R file/weh.",year,".RData"))
# weh.df.all <- rbind(temp, weh.df)
# rm(weh.df, temp)
# save(weh.df.all, file="R file/weh.df.all.RData")

### Create integrated swd.df
# year <- 2022
# load(file=paste0("R file/swd.",year,".RData"))
# temp <- swd.df
# year <- 2023
# load(file=paste0("R file/swd.",year,".RData"))
# swd.df.all <- rbind(temp, swd.df)
# rm(swd.df, temp)
# swd.df.all %<>% 
#   mutate(StartTime =gsub("(\\d{2})(?!$)", "\\1:", StartTime, perl = TRUE) |> 
#            lubridate::hms()) %>%
#   mutate(EndTime =gsub("(\\d{2})(?!$)", "\\1:", EndTime, perl = TRUE) |> 
#            lubridate::hms()) %>%
#   mutate(WatchTime=EndTime-StartTime)
# library(readxl)
# channel.df <- read_excel('SWD_Channel_2022.xlsx') %>%
#   rename(Channel='SWD 채널코드', Channel.label='채널명') %>% 
#   mutate(Year=2022) %>%
#   rbind(read_excel('SWD_Channel_2023.xlsx') %>%
#           rename(Channel='SWD 채널코드', Channel.label='채널명') %>% 
#           mutate(Year=2023))
# swd.df.all %<>% left_join(channel.df)
# save(swd.df.all, file="R file/swd.df.all.RData")

### Create integrated dem.df
# year <- 2022
# load(file=paste0("R file/dem.",year,".RData"))
# temp <- dem.df
# year <- 2023
# load(file=paste0("R file/dem.",year,".RData"))
# dem.df.all <- rbind(temp, dem.df)
# rm(dem.df, temp)
# save(dem.df.all, file="R file/dem.df.all.RData")

