#######################################################################
##  Made by: Dr. Keungoui Kim
##  Title: KOBACO Project 03. Diversity Analysis
##  goal : Measure TV-Watching Diversity and TV-Channel Similarity
##  Data set: KOBACO
##  Notice :
#######################################################################

library('dplyr')
library('magrittr')
library('tidyr')
library('ggplot2')
library('igraph')
library('data.table')
'%ni%' <- Negate('%in%')

####################################################################################
### TV-Watching Diversity
####################################################################################

library(diverse)

load(file="R file/swd.df.month.RData")  

swd.df.month %>% head

diverse.df <- list()
for(i in 1:length(unique(swd.df.month$period))){
  temp <- swd.df.month %>% 
    mutate(WatchTime=abs(WatchTime)) %>% # convert - to +
    group_by(period, HouseholdID, Channel, Channel.label) %>%
    dplyr::summarise(WatchTime = sum(WatchTime)) %>% ungroup %>%
    filter(period==unique(swd.df.month$period)[i]) %>%
    select(HouseholdID,Channel.label,WatchTime) %>% as.data.frame  
  
  diverse.df[[i]] <- diverse::diversity(temp)
  diverse.df[[i]]$period <- unique(swd.df.month$period)[i]
  diverse.df[[i]]$HouseholdID <- row.names(diverse.df[[i]]) 
}
diverse.df <- do.call(rbind, diverse.df)
save(diverse.df, file="R file/diverse.df.RData")

load(file="R file/diverse.df.RData")

diverse.df$period %>% table
diverse.df %>% head

# Bimonthly trend of Channel numbers
swd.df.month %>% group_by(period) %>%
  summarize(channel.count=length(unique(Channel))) %>% ungroup %>%
  ggplot(aes(x=period, y=channel.count, group=1)) +
  geom_line(size=2) + 
  ylab("채널 수") + xlab("시점") + 
  theme_bw()

swd.df.month %>% 
  filter(period<="pr06") %>% 
  group_by(period) %>%
  summarize(channel.count=length(unique(Channel))) %>% ungroup %>%
  ggplot(aes(x=period, y=channel.count, group=1)) +
  geom_line(size=2) + ylab("채널 수") + xlab("시점") + 
  theme_bw()

swd.df.month %>% 
  filter(period>="pr07") %>% 
  group_by(period) %>%
  summarize(channel.count=length(unique(Channel))) %>% ungroup %>%
  ggplot(aes(x=period, y=channel.count, group=1)) +
  geom_line(size=2) + ylab("채널 수") + xlab("시점") + 
  theme_bw()

# Bimonthly trend of Channel Diversity
diverse.df %>% group_by(period) %>%
  summarize(rao.stirling=mean(rao.stirling)) %>%
  # filter(HouseholdID=="/02vbJrE5wb9kwhhYuKVzsJyMX4=") %>%
  ggplot(aes(x=period, y=rao.stirling, group=1)) +
  geom_line(size=2) + 
  ylab("채널 다양성(Rao-Stirling)") + xlab("시점") + 
  theme_bw()

diverse.df %>% 
  filter(period<="pr06") %>% 
  group_by(period) %>%
  summarize(rao.stirling=mean(rao.stirling)) %>%
  # filter(HouseholdID=="/02vbJrE5wb9kwhhYuKVzsJyMX4=") %>%
  ggplot(aes(x=period, y=rao.stirling, group=1)) +
  geom_line(size=2) + 
  ylab("채널 다양성(Rao-Stirling)") + xlab("시점") + 
  theme_bw()

diverse.df %>% 
  filter(period>="pr07") %>% 
  group_by(period) %>%
  summarize(rao.stirling=mean(rao.stirling)) %>%
  # filter(HouseholdID=="/02vbJrE5wb9kwhhYuKVzsJyMX4=") %>%
  ggplot(aes(x=period, y=rao.stirling, group=1)) +
  geom_line(size=2) + 
  ylab("채널 다양성(Rao-Stirling)") + xlab("시점") + 
  theme_bw()

# Bimonthly trend by cluster groups 
load(file="R file/dem.df.all.sum.RData")
load(file="R file/swd.df.month.RData")
load(file="R file/diverse.df.RData")

dem.df.all.sum %>% head
swd.df.month %>% head

swd.df.month %>% 
  left_join(dem.df.all.sum %>% select(HouseholdID,cluster) %>% unique) %>%
  mutate(cluster.label=case_when(cluster==1 ~ "1-소형 가구, 제한된 직업군",
                                 cluster==2 ~ "2-대가족, 직업 및 미디어 다양성",
                                 cluster==3 ~ "3-중형 가구, 농업 및 숙련직 중심",
                                 cluster==4 ~ "4-대형 가구, 직업 다양성",
                                 cluster==5 ~ "5-중형 가구, 직업 분포 균형",
                                 cluster==6 ~ "6-기술 중심 중형 가구")) %>%
  group_by(period,cluster.label) %>%
  summarize(channel.count=length(unique(Channel))) %>% ungroup %>%
  ggplot(aes(x=period, y=channel.count, group=cluster.label, color=cluster.label)) +
  geom_line(size=2) + 
  ylab("채널 수") + xlab("시점") + theme_bw() + 
  theme(legend.position="bottom") 

diverse.df %>%
  left_join(dem.df.all.sum %>% select(HouseholdID,cluster) %>% unique) %>%
  mutate(cluster.label=case_when(cluster==1 ~ "1-소형 가구, 제한된 직업군",
                                 cluster==2 ~ "2-대가족, 직업 및 미디어 다양성",
                                 cluster==3 ~ "3-중형 가구, 농업 및 숙련직 중심",
                                 cluster==4 ~ "4-대형 가구, 직업 다양성",
                                 cluster==5 ~ "5-중형 가구, 직업 분포 균형",
                                 cluster==6 ~ "6-기술 중심 중형 가구")) %>%
  group_by(period,cluster.label) %>%
  summarize(rao.stirling=mean(rao.stirling)) %>% ungroup %>%
  ggplot(aes(x=period, y=rao.stirling, group=cluster.label, color=cluster.label)) +
  geom_line(size=2) + 
  ylab("채널 수") + xlab("시점") + theme_bw() + 
  theme(legend.position="bottom") 

####################################################################################
### Compare Main-stream and Cluster
####################################################################################

### (channel count) Compare main stream changes and Cluster changes  
channel.count.comp <- swd.df.month %>% 
  left_join(dem.df.all.sum %>% select(HouseholdID,cluster) %>% unique) %>%
  mutate(cluster.label=case_when(cluster==1 ~ "1-소형 가구, 제한된 직업군",
                                 cluster==2 ~ "2-대가족, 직업 및 미디어 다양성",
                                 cluster==3 ~ "3-중형 가구, 농업 및 숙련직 중심",
                                 cluster==4 ~ "4-대형 가구, 직업 다양성",
                                 cluster==5 ~ "5-중형 가구, 직업 분포 균형",
                                 cluster==6 ~ "6-기술 중심 중형 가구")) %>%
  group_by(period,cluster.label) %>%
  summarize(channel.count=length(unique(Channel))) %>% ungroup %>%
  rbind(swd.df.month %>% group_by(period) %>%
          summarize(channel.count=length(unique(Channel))) %>%
          mutate(cluster.label='7-ALL') %>% ungroup) %>% data.frame 

### (diversity) Compare main stream changes and Cluster changes  
diversity.count.comp <- diverse.df %>% 
  left_join(dem.df.all.sum %>% select(HouseholdID,cluster) %>% unique) %>%
  mutate(cluster.label=case_when(cluster==1 ~ "1-소형 가구, 제한된 직업군",
                                 cluster==2 ~ "2-대가족, 직업 및 미디어 다양성",
                                 cluster==3 ~ "3-중형 가구, 농업 및 숙련직 중심",
                                 cluster==4 ~ "4-대형 가구, 직업 다양성",
                                 cluster==5 ~ "5-중형 가구, 직업 분포 균형",
                                 cluster==6 ~ "6-기술 중심 중형 가구")) %>%
  group_by(period,cluster.label) %>%
  summarize(rao.stirling=mean(rao.stirling)) %>% ungroup %>%
  rbind(diverse.df %>% group_by(period) %>%
          summarize(rao.stirling=mean(rao.stirling)) %>% 
          mutate(cluster.label='7-ALL') %>% ungroup) %>% data.frame 

### uncomment if analyze channel count
# data_diff <- 
#   channel.count.comp %>%
#   group_by(cluster.label) %>%
#   arrange(period) %>%
#   mutate(change_direction = case_when(
#     channel.count - lag(channel.count) > 0 ~ "increase", channel.count - lag(channel.count) < 0 ~ "decrease",
#     TRUE ~ "stable")) %>%
#   filter(!is.na(change_direction))  
### uncomment if analyze diversity
data_diff <- 
  diversity.count.comp %>%
  group_by(cluster.label) %>%
  arrange(period) %>%
  mutate(change_direction = case_when(
    rao.stirling - lag(rao.stirling) > 0 ~ "increase", rao.stirling - lag(rao.stirling) < 0 ~ "decrease",
    TRUE ~ "stable")) %>%
  filter(!is.na(change_direction))  

all_flow <- data_diff %>%
  filter(cluster.label == "7-ALL") %>%
  pull(change_direction)

similar_flow <- data_diff %>%
  filter(cluster.label != "7-ALL") %>%
  group_by(cluster.label) %>%
  summarize(similarity = sum(change_direction == all_flow) / length(all_flow)) %>%
  arrange(desc(similarity))

similar_flow %>%
  slice(1:3)

channel.count.comp %>% 
  filter(cluster.label == '4-대형 가구, 직업 다양성' | cluster.label == '7-ALL') %>%
  ggplot(aes(x = period, y = channel.count, group = cluster.label, linetype = cluster.label)) +
  geom_line(aes(color = cluster.label), size = 2) +
  scale_color_manual(values = c("7-ALL" = "black", "4-대형 가구, 직업 다양성" = "red")) +
  scale_linetype_manual(values = c("7-ALL" = "solid", "4-대형 가구, 직업 다양성" = "dashed")) +
  ylab("채널 수") + xlab("시점") +
  theme_bw() + 
  theme(legend.position = "bottom")

diversity.count.comp %>% 
  filter(cluster.label == '1-소형 가구, 제한된 직업군' | cluster.label == '7-ALL' |
           cluster.label == '5-중형 가구, 직업 분포 균형') %>%
  ggplot(aes(x = period, y = rao.stirling, group = cluster.label, linetype = cluster.label)) +
  geom_line(aes(color = cluster.label), size = 2) +
  scale_color_manual(values = c("7-ALL" = "black", "1-소형 가구, 제한된 직업군" = "red", 
                                "5-중형 가구, 직업 분포 균형" = "blue" )) +
  scale_linetype_manual(values = c("7-ALL" = "solid", "1-소형 가구, 제한된 직업군" = "dashed",
                                   "5-중형 가구, 직업 분포 균형" = "dashed")) +
  ylab("미디어 시청 다양성") + xlab("시점") +
  theme_bw() + 
  theme(legend.position = "bottom")

####################################################################################
### Channel Similarity
####################################################################################

# Create wide table
swd.df.month.wide <- swd.df.month %>% 
  select(HouseholdID,period, Channel,WatchTime) %>% 
  spread(key=Channel, value=WatchTime)
save(swd.df.month.wide, file="R file/swd.df.month.wide.RData")

unique(swd.df.month[swd.df.month$period=="pr02",]$Channel.label)[unique(swd.df.month[swd.df.month$period=="pr02",]$Channel.label) %ni% unique(swd.df.month[swd.df.month$period=="pr01",]$Channel.label)]
