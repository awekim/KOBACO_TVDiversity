#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KOBACO Project 02. EDA
##  goal : Data Exploration & Pre-processing
##  Data set: KOBACO
##  Notice :
#######################################################################

# swd: 실시간TV 개인시청기록
# weh: 가구 비가중치
# dem: 개인 가중치

library('dplyr')
library('magrittr')
library('tidyr')
library('ggplot2')
library('igraph')
library('data.table')
'%ni%' <- Negate('%in%')

load(file="R file/swd.df.all.RData")
load(file="R file/weh.df.all.RData")
load(file="R file/dem.df.all.RData")

####################################################################################
### Create Bimonthly household-level Data
####################################################################################

### Find list of Households that appear all months 
weh.df.month <- weh.df.all %>% 
  group_by(Reg,HouseholdID,Year,Month) %>%
  summarize(Weight=sum(Weight))
rm(weh.df.all)

household.list <- weh.df.month$HouseholdID %>% table %>%
  data.frame %>% filter(Freq==24) %>% select(1) %>% pull
save(household.list, file="R file/household.list.RData")

weh.df.month %<>% 
  filter(HouseholdID %in% household.list)

weh.df.month$Month <- str_pad(weh.df.month$Month, 2, pad = "0")
weh.df.month %<>% arrange(Year, Month) %>% 
  mutate(ym=paste0(Year,'-',Month))

weh.df.month %<>% 
  mutate(period = case_when(ym=="2022-01" | ym=="2022-02" ~ "pr01", 
                            ym=="2022-03" | ym=="2022-04" ~ "pr02",
                            ym=="2022-05" | ym=="2022-06" ~ "pr03",
                            ym=="2022-07" | ym=="2022-08" ~ "pr04",
                            ym=="2022-09" | ym=="2022-10" ~ "pr05",
                            ym=="2022-11" | ym=="2022-12" ~ "pr06",
                            ym=="2023-01" | ym=="2023-02" ~ "pr07", 
                            ym=="2023-03" | ym=="2023-04" ~ "pr08",
                            ym=="2023-05" | ym=="2023-06" ~ "pr09",
                            ym=="2023-07" | ym=="2023-08" ~ "pr10",
                            ym=="2023-09" | ym=="2023-10" ~ "pr11",
                            ym=="2023-11" | ym=="2023-12" ~ "pr12"))
weh.df.month %<>% ungroup
weh.df.month %<>% 
  mutate(Reg.label = case_when(Reg=='001'~'서울',Reg=='003'~'부산',
                               Reg=='004'~'대전',Reg=='005'~'대구',
                               Reg=='006'~'광주',Reg=='007'~'울산',
                               Reg=='008'~'강원',Reg=='009'~'충북',
                               Reg=='010'~'충남',Reg=='011'~'전북',
                               Reg=='012'~'전남',Reg=='013'~'경북',
                               Reg=='014'~'경남',Reg=='015'~'제주',
                               Reg=='016'~'인천',Reg=='017'~'경기',
                               Reg=='018'~'세종'))
save(weh.df.month, file="R file/weh.df.month.RData")

table(weh.df.month$Reg)
table(weh.df.month$Month)

load(file="R file/household.list.RData")
load(file="R file/swd.df.all.RData")

swd.df.all$HouseholdID %>% unique %>% length
swd.df.all$Channel %>% unique %>% length

swd.df.all %<>%  
  filter(HouseholdID %in% household.list)

swd.df.month <- swd.df.all %>% 
  group_by(HouseholdID,Channel,Channel.label,Year,Month) %>%
  summarize(WatchTime=mean(WatchTime)) %>%
  ungroup
rm(swd.df.all)

swd.df.month$Month <- str_pad(swd.df.month$Month, 2, pad = "0")
swd.df.month %<>% arrange(Year, Month) %>% 
  mutate(ym=paste0(Year,'-',Month))

swd.df.month$ym %>% unique %>% sort

swd.df.month %<>% 
  mutate(period = case_when(ym=="2022-01" | ym=="2022-02" ~ "pr01", 
                            ym=="2022-03" | ym=="2022-04" ~ "pr02",
                            ym=="2022-05" | ym=="2022-06" ~ "pr03",
                            ym=="2022-07" | ym=="2022-08" ~ "pr04",
                            ym=="2022-09" | ym=="2022-10" ~ "pr05",
                            ym=="2022-11" | ym=="2022-12" ~ "pr06",
                            ym=="2023-01" | ym=="2023-02" ~ "pr07", 
                            ym=="2023-03" | ym=="2023-04" ~ "pr08",
                            ym=="2023-05" | ym=="2023-06" ~ "pr09",
                            ym=="2023-07" | ym=="2023-08" ~ "pr10",
                            ym=="2023-09" | ym=="2023-10" ~ "pr11",
                            ym=="2023-11" | ym=="2023-12" ~ "pr12"))

save(swd.df.month, file="R file/swd.df.month.RData")  
write.csv(swd.df.month %>% select(Channel) %>% unique, 
          file="R file/Channel_list.csv", row.names=FALSE)

swd.df.month$HouseholdID %>% unique %>% length
swd.df.month$Channel %>% unique %>% length

load(file="R file/swd.df.month.RData")  
load(file="R file/weh.df.month.RData")  

swd.df.month %>% 
  left_join(weh.df.month %>% select(HouseholdID,period,Reg) %>% unique) %>%
  mutate(all.WT = sum(WatchTime)) %>%
  group_by(Channel.label) %>%
  summarize(WatchTime=sum(WatchTime), all.WT=mean(all.WT)) %>% arrange(desc(WatchTime)) %>%
  mutate(prop=WatchTime/all.WT) %>% 
  slice(1:10)

swd.df.month %>% 
  left_join(weh.df.month %>% select(HouseholdID,period,Reg) %>% unique) %>%
  mutate(all.WT = sum(WatchTime)) %>%
  group_by(Channel.label) %>%
  summarize(WatchTime=sum(WatchTime), all.WT=mean(all.WT)) %>% arrange(desc(WatchTime)) %>%
  mutate(prop=WatchTime/all.WT) %>% 
  ggplot(aes(x=WatchTime)) + 
  geom_histogram(aes(y=..density..), color='black',fill='white') +
  ylab("빈도 수") + xlab("채널별 총 시청 시간") + 
  geom_density(alpha=.2, fill="#FF6666")  + 
  geom_vline(aes(xintercept=mean(WatchTime)),
             color='blue', linetype='dashed') + 
  theme_bw() 

swd.df.month %>% 
  left_join(weh.df.month %>% select(HouseholdID,period,Reg,Reg.label) %>% unique) %>%
  mutate(all.WT = sum(WatchTime)) %>%
  group_by(Reg,Reg.label,Channel.label) %>%
  summarize(WatchTime=sum(WatchTime), all.WT=mean(all.WT)) %>% arrange(desc(WatchTime)) %>%
  mutate(prop=WatchTime/all.WT) %>%
  slice(1:4) %>% data.frame

swd.df.month %>% 
  left_join(weh.df.month %>% select(HouseholdID,period,Reg,Reg.label) %>% unique) %>%
  filter(Reg.label %in% c("서울","경기","세종","대전","대구","광주")) %>%
  group_by(period,Reg,Reg.label) %>%
  summarize(Channel.count=length(unique(Channel.label))) %>% 
  data.frame %>%
  ggplot(aes(x=period,y=Channel.count,color=Reg.label,group=Reg.label)) +
  geom_line(size=2) + 
  geom_text(data = . %>% group_by(Reg.label) %>% filter(period == max(period)),
            aes(label=Reg.label), size=7, hjust=-0.1, vjust=0.5) +
  ylab("시점") + xlab("고유 시청 채널 수") + 
  theme_bw()


####################################################################################
### Aggregate Individual-level to Household-level data Set
####################################################################################

load(file="R file/dem.df.all.RData")
dem.df.all %>% head

dem.df.all.sum <- 
  # Number of parties
  dem.df.all %>% group_by(HouseholdID) %>%
  summarize(num.ind=length(unique(IndividualID))) %>%
  # Number of males and females
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(sex_male = n_distinct(IndividualID[Sex == 1]),
                        sex_female = n_distinct(IndividualID[Sex == 2]))) %>%
  # Number of Jobs
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(job_h = n_distinct(IndividualID[Job == 'H']),
                        job_i = n_distinct(IndividualID[Job == 'I']),
                        job_j = n_distinct(IndividualID[Job == 'J']),
                        job_k = n_distinct(IndividualID[Job == 'K']),
                        job_l = n_distinct(IndividualID[Job == 'L']),
                        job_m = n_distinct(IndividualID[Job == 'M']),
                        job_n = n_distinct(IndividualID[Job == 'N']),
                        job_o = n_distinct(IndividualID[Job == 'O']),
                        job_p = n_distinct(IndividualID[Job == 'P']),
                        job_q = n_distinct(IndividualID[Job == 'Q']),
                        job_r = n_distinct(IndividualID[Job == 'R']))) %>%
  # Number of Ages
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(teen_1 = n_distinct(IndividualID[Teen == '1']),
                        teen_2 = n_distinct(IndividualID[Teen == '2']),
                        teen_3 = n_distinct(IndividualID[Teen == '3']),
                        teen_4 = n_distinct(IndividualID[Teen == '4']),
                        teen_5 = n_distinct(IndividualID[Teen == '5']),
                        teen_6 = n_distinct(IndividualID[Teen == '6']),
                        teen_7 = n_distinct(IndividualID[Teen == '7']),
                        teen_8 = n_distinct(IndividualID[Teen == '8']),
                        teen_9 = n_distinct(IndividualID[Teen == '9']),
                        teen_a = n_distinct(IndividualID[Teen == 'a']),
                        teen_b = n_distinct(IndividualID[Teen == 'b']),
                        teen_c = n_distinct(IndividualID[Teen == 'c']),
                        teen_d = n_distinct(IndividualID[Teen == 'd']),
                        teen_e = n_distinct(IndividualID[Teen == 'e']))) %>%
  # Number of Machines
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(machine_1 = n_distinct(IndividualID[Machines == '1']),
                        machine_2 = n_distinct(IndividualID[Machines == '2']))) %>%
  # Number of Cables
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(cable_9 = n_distinct(IndividualID[Cable == '9']),
                        cable_8 = n_distinct(IndividualID[Cable == '8']),
                        cable_a = n_distinct(IndividualID[Cable == 'a']),
                        cable_b = n_distinct(IndividualID[Cable == 'b']))) %>%
  # Number of SkyLife
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(skylife_f = n_distinct(IndividualID[SkyLife == 'F']),
                        skylife_h = n_distinct(IndividualID[SkyLife == 'H']),
                        skylife_g = n_distinct(IndividualID[SkyLife == 'G']))) %>%
  # Number of IPTV
  left_join(dem.df.all %>% group_by(HouseholdID) %>% 
              summarise(iptv_e = n_distinct(IndividualID[SkyLife == 'E']),
                        iptv_f = n_distinct(IndividualID[SkyLife == 'F']),
                        iptv_g = n_distinct(IndividualID[SkyLife == 'G'])))
rm(dem.df.all)
save(dem.df.all.sum, file="R file/dem.df.all.sum.RData")

####################################################################################
### Clustering Analysis
####################################################################################

library(cluster)

# data_scaled <- scale(dem.df.all.sum %>% select(-c("HouseholdID")))

wss <- sapply(1:20, function(k) {
  kmeans(dem.df.all.sum %>% select(-c("HouseholdID")), 
         centers = k)$tot.withinss
})

plot(1:20, wss, 
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")

k <- 6

set.seed(123)  
kmeans_result <- 
  kmeans(dem.df.all.sum %>% select(-c("HouseholdID")), 
         centers = k)

sil <- silhouette(kmeans_result$cluster, 
                  dist(dem.df.all.sum %>% select(-c("HouseholdID"))))

plot(sil, main = "Silhouette Plot for K-means Clustering", col = 1:k)

dem.df.all.sum$cluster <- as.factor(kmeans_result$cluster)
save(dem.df.all.sum, file="R file/dem.df.all.sum.RData")

cluster_summary <- 
  aggregate(. ~ cluster, 
            data = dem.df.all.sum %>% select(-c("HouseholdID")), 
            FUN = mean)
write.csv(cluster_summary,
          file = "R file/cluster_summary.csv",
          row.names=FALSE)

####################################################################################
### Prompt for cluster summary
####################################################################################

# 아래 컬럼 정보를 참고해서 각 cluster를 묘사하는 레이블을 만들어줘

# cluster: cluster type
# num.ind: 가족 구성원수
# sex_male: 남성 수
# sex_female: 여성 수

# job_h: 관리자	수 
# job_i: 전문가 및 관련종사자	수 
# job_j: 사무종사자	수 
# job_k: 서비스 종사자 수 
# job_l: 판매종사자	수 
# job_m: 농림어업 숙련 종사자	수 
# job_n: 기능원 및 관련 기능 종사자	수 
# job_o: 장치 기계 조작 및 조립 종사자 수 
# job_p: 단순노무 종사자 수 
# job_q: 군인(의무복무 제외) 수 
# job_r: 기타(학생,전업주부,연금생활자,무직) 수 

# teen_1: 4-9세 남자 수
# teen_2: 4-9세 여자 수
# teen_3: 10대 남자	수 
# teen_4: 10대 여자	수 
# teen_5: 20대 남자	수 
# teen_6: 20대 여자	수 
# teen_7: 30대 남자	수 
# teen_8: 30대 여자	수 
# teen_9: 40대 남자	수 
# teen_1: 40대 여자	수 
# teen_b: 50대 남자	수 
# teen_c: 50대 여자	수 
# teen_d: 60세이상 남자	수 
# teen_e: 60세이상 여자	수 

# machine_1: TV 1대 보유  
# machine_2: TV 2대이상 보유

# cable_8: 케이블 비가입 경우
# cable_9: 디지털케이블 경우
# cable_a: 아날로그케이블 경우
# cable_b: 8VSB케이블 경우

# skylife_f: HD가입 경우
# skylife_g: OTS가입 경우
# skylife_h: 스카이라이프 비가입 경우

# iptv_e: 실시간패키지 경우
# iptv_f: VOD패키지 경우
# iptv_g: IPTV 비가입 경우

####################################################################################
### Descriptive Statistics for clusters
####################################################################################

load(file="R file/weh.df.month.RData")
load(file="R file/dem.df.all.sum.RData")

dem.df.all.sum %>% select(HouseholdID, cluster) %>% unique %>% 
  left_join(weh.df.month %>% select(HouseholdID,period,Reg,Reg.label) %>% unique) %>%
  mutate(cluster.label=case_when(cluster==1 ~ "1-소형 가구, 제한된 직업군",
                                 cluster==2 ~ "2-대가족, 직업 및 미디어 다양성",
                                 cluster==3 ~ "3-중형 가구, 농업 및 숙련직 중심",
                                 cluster==4 ~ "4-대형 가구, 직업 다양성",
                                 cluster==5 ~ "5-중형 가구, 직업 분포 균형",
                                 cluster==6 ~ "6-기술 중심 중형 가구")) %>%
  filter(is.na(Reg.label)==FALSE) %>%
  group_by(Reg, Reg.label, cluster.label) %>%
  count() %>%
  ggplot(aes(x=Reg.label,y=n,fill=cluster.label)) + 
  geom_bar(stat='identity', position='dodge') + theme_bw() + 
  theme(legend.position="bottom")
  
