#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KOBACO Project 01. Data Regeneration
##  goal : Data Regeneration
##  Data set: KOBACO
##  Time Span: 
##  Variables
##      Input: 
##      Output:  
##  Methodology: SQL
##  Time-stamp: :  
##  Notice :

library(dplyr)
library(magrittr)

year <- 2022

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
            select(-c("V1")) %>% mutate(Year=substr(temp.list[i],3,4),
                                        Month=substr(temp.list[i],5,6),
                                        Day=substr(temp.list[i],7,8)))
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
                          Reg = substr(V1,31,33), Weight = substr(V1, 34,41)) %>%
            select(-c("V1")) %>% mutate(Year=substr(temp.list[i],3,4),
                                        Month=substr(temp.list[i],5,6),
                                        Day=substr(temp.list[i],7,8)))
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
                          IndWeight = substr(V1,31,38), Reg = substr(V1,39,39)) %>%
            select(-c("V1")) %>% 
            rename(Sex=V2, Job=V3, Household=V4, Teen=V6) %>%
            mutate(Machines=substr(V5,1,1), Cable=substr(V5,2,2), 
                   SkyLife=substr(V7,1,1),IPTV=substr(V7,2,2),
                   Year=substr(temp.list[i],3,4),
                   Month=substr(temp.list[i],5,6),
                   Day=substr(temp.list[i],7,8)) %>%
            select(HouseholdID, IndividualID, IndWeight, Reg, Sex, Job,
                   Household, Teen, Machines, Cable, SkyLife, IPTV, 
                   Year, Month, Day))
  
  print(i)
}
dem.df <- dem.df[2:nrow(dem.df),]
save(dem.df, file=paste0("R file/dem.",year,".RData"))

