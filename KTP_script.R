#--CODE INDEX----
#    1.LIBRARIES
#    2.LOADING DATA SET
#    3.EXPLORE DATA SET
#    4.SUB-SETTING AND QUALITY CONTROLLING
#       4.1.Systolic and Diastolic Blood Pressure (SBP and DBP)
#       4.2. Weight
#       4.3.Insulin and Insulin pump users
#       4.4.Cholesterol lowering medications (anti_cholesterol)
#       4.5.BP medications (anti_hypertensives)
#     5.FINAL DATA FRAME CREATION
#     6.ANALYSIS
#       6.1.Descriptive Statistics
#     7.VISUALIZATION


# 1.LIBRARIES----
library(tidyverse)
library(lubridate)
library(data.table)


# 2.LOADING DATA SET----
setwd("E:/JOBS_2022/KTP_dataSet/")
orig_data<-read_csv("Task4_app_data.csv")
orig_demog<-read_csv("Task4_ehr_demographics.csv")


# 3.EXPLORE DATA----

#explore all the variables of both dataframes to findout what QC measures needed to be taken.

#orig_demog
summary(orig_demog)
glimpse(orig_demog)

length(unique(orig_demog$id))
table(orig_demog$dmtype)
table(orig_demog$sex)
hist(orig_demog$height) 

#orig_data
summary(orig_data)
glimpse(orig_data)

length(unique(orig_data$id))
table(orig_data$question)


# To explore more on each variable within the 'question' column filtering by each of them
#         (birthyear,bp medications,cholesterol lowering medications,dbp, dmtype, insulin, 
#         insulin pump,Postcode,sbp, sex, weight)




# 4.SUB-SETTING AND QUALITY CONTROLLING----

# 4.1.Systolic and Diastolic Blood Pressure (SBP and DBP)----
    #   Data type changed to numeric
    #   DBP - 40 - 120 (To keep realistic extreme values and to remove erroneous results these ranges were set)
    #   SBP - 60 - 220 (To keep realistic extreme values and to remove erroneous results these ranges were set)
    #   Remove if SBP is lower than DBP value (Pulse pressure)
    #   Remove if both SBP and DBP records are not available on the same obs_date
    #   Keep last available observation for each individual (filter by max(obs_date))


sub_dbp<-orig_data %>% 
  filter(question=="dbp") %>% 
  mutate(value=as.numeric(value)) %>% 
  filter(value <=120 & value>=40) %>% 
  rename(DBP=value) %>% 
  select(-question) %>% 
  arrange(id,obs_date)

sub_sbp<-orig_data %>% 
  filter(question=="sbp") %>% 
  mutate(value=as.numeric(value)) %>% 
  filter(value <=220 & value>=60) %>% 
  rename(SBP=value) %>% 
  select(-question) %>% 
  arrange(id,obs_date)


sub_BP<-merge(sub_sbp,sub_dbp,by=c("id","obs_date"),all=TRUE) %>% 
  drop_na() %>% 
  mutate(response = if_else(SBP>DBP,"TRUE","FALSE")) %>% 
  filter(response=="TRUE") %>% 
  group_by(id)%>% 
  filter(obs_date==max(obs_date)) %>% slice(1L) %>% 
  select(-c(obs_date,response)) %>% 
  arrange(id)

# 4.2. Weight----
    # Weight range between 32.8 to 172.1 Kg. All of them lies between life compatible range. therefore, no filtering was done.
    # Data type changed to numeric
    # Last weight observation of each person was reatined.

sub_weight<-orig_data %>% 
  filter(question=='weight') %>% 
  mutate(value= as.numeric(value)) %>% 
  drop_na(value) %>% 
  group_by(id) %>% 
  filter(value==max(value)) %>% slice(1L) %>% 
  rename(Weight=value) %>% 
  select(-c(obs_date,question)) %>% 
  arrange(id)

summary(sub_weight$value)

# 4.3.Insulin and Insulin pump users----
    # There were no duplicated ids. length(unique(sub_insulin$id))
    # everyone who are using insulin pump are on insulin.

sub_insulin<-orig_data %>% 
  filter(question %in% c('insulin','insulin pump')) %>% 
  pivot_wider(names_from = question,values_from = value) %>% 
  rename(insulin_pump='insulin pump') %>% 
  select(-obs_date) %>% 
  arrange(id)


# 4.4.Cholesterol lowering medications (anti_cholesterol)----
    # The last observation for each individual was taken.

sub_antichol<-orig_data %>% 
  filter(question=="cholesterol lowering medications") %>% 
  group_by(id) %>% 
  filter(obs_date==max(obs_date)) %>% slice(1L) %>% 
  select(-c(obs_date,question)) %>% 
  rename(anti_cholesterol=value) %>% 
  arrange(id)


# 4.5.BP medications (anti_hypertensives)----
# The last observation for each individual was taken.

sub_antiHTN<-orig_data %>% 
  filter(question=="bp medications") %>% 
  group_by(id) %>% 
  filter(obs_date==max(obs_date)) %>% slice(1L) %>% 
  select(-c(obs_date,question)) %>% 
  rename(anti_hypertensive=value) %>% 
  arrange(id)



# 5. FINAL DATA FRAME CREATION----

data_final<-merge(orig_demog,sub_weight,by="id",all.x = TRUE) %>% 
  mutate(height=height/100,height=round(height,2))

data_final<-merge(data_final,sub_BP,by="id",all.x = TRUE)
data_final<-merge(data_final,sub_antiHTN,by="id",all.x = TRUE)
data_final<-merge(data_final,sub_antichol,by="id",all.x = TRUE)
data_final<-merge(data_final,sub_insulin,by="id",all.x = TRUE)

# add BMI, BMI_group and Age columns
data_final<-data_final %>%  
  mutate(BMI = Weight/(height*height),BMI=round(BMI,2),
         BMI_group=case_when(BMI<18.5 ~"Underweight",
                             BMI<25~ "Normal",
                             BMI<30~"Overweight",
                             BMI>=30 ~ "Obese",
                             TRUE~'NA'))%>% 
  mutate(BMI_group=fct_relevel(BMI_group,"Normal","Underweight","Overweight","Obese")) %>% 
  mutate(Age= 2022-birthyear)




# 6. ANALYSIS----

# 6.1. Descriptive Statistics----

table(data_final$dmtype)

#Age-whole cohort----
data_final %>% 
  filter(Age<25) %>% 
  view() #80

data_final %>% 
  filter(Age>=25 & Age<50) %>% 
  view() #625

data_final %>% 
  filter(Age>=50 & Age<75) %>% 
  view() #154

data_final %>% 
  filter(Age>=75) %>% 
  view() #32

mean(data_final$Age)
sd(data_final$Age)
range(data_final$Age)

#Age-Type 1 DM----
data_final %>% 
  filter(dmtype=='type 1'&Age<25) %>% 
  view() #18

data_final %>% 
  filter(dmtype=='type 1'&(Age>=25 & Age<50)) %>% 
  view() #145

data_final %>% 
  filter(dmtype=='type 1'&(Age>=50 & Age<75)) %>% 
  view() #23

data_final %>% 
  filter(dmtype=='type 1'&Age>=75) %>% 
  view() #0

mean(data_final$Age[data_final$dmtype=='type 1'])
sd(data_final$Age[data_final$dmtype=='type 1'])
range(data_final$Age[data_final$dmtype=='type 1'])

#Age-Type 2 DM----
data_final %>% 
  filter(dmtype=='type 2'&Age<25) %>% 
  view() #23

data_final %>% 
  filter(dmtype=='type 2'&(Age>=25 & Age<50)) %>% 
  view() #226

data_final %>% 
  filter(dmtype=='type 2'&(Age>=50 & Age<75)) %>% 
  view() #101

data_final %>% 
  filter(dmtype=='type 2'&Age>=75) %>% 
  view() #31

mean(data_final$Age[data_final$dmtype=='type 2'])
sd(data_final$Age[data_final$dmtype=='type 2'])
range(data_final$Age[data_final$dmtype=='type 2'])

#Sex----
table(data_final$sex)
table(data_final$sex[data_final$dmtype=='type 1'])
table(data_final$sex[data_final$dmtype=='type 2'])

#BMI----

table(data_final$BMI_group)
table(data_final$BMI_group[data_final$dmtype=='type 1'])
table(data_final$BMI_group[data_final$dmtype=='type 2'])

summary(data_final$BMI)
summary(data_final$BMI[data_final$dmtype=='type 1'])
summary(data_final$BMI[data_final$dmtype=='type 2'])

sd(data_final$BMI[!is.na(data_final$BMI)])
sd(data_final$BMI[data_final$dmtype=='type 1' & !is.na(data_final$BMI)])
sd(data_final$BMI[data_final$dmtype=='type 2' & !is.na(data_final$BMI)])

#Insulin----
table(data_final$insulin)
table(data_final$insulin[data_final$dmtype=='type 1'])
table(data_final$insulin[data_final$dmtype=='type 2'])


# 7. VISUALIZATION----

#Age distribution of Type1 and Type 2----

data_final_stats<- data_final %>% 
  filter(dmtype %in% c('type 1','type 2')) %>%
  mutate(dmtype=recode(dmtype,'type 1'= "Type 1 DM",
                       'type 2'="Type 2 DM")) %>% 
  group_by(dmtype) %>% 
  summarize(Mean=mean(Age),
            Median=median(Age)) %>% 
  pivot_longer(2:3,names_to = "Key",values_to = "Value")

pdf("Agedistribution.pdf", width = 16,height = 10)
data_final %>% 
  filter(dmtype %in% c('type 1','type 2')) %>% 
  mutate(dmtype=recode(dmtype,'type 1'= "Type 1 DM",
                       'type 2'="Type 2 DM")) %>% 
  ggplot(aes(Age,fill=dmtype))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~dmtype)+
  geom_vline(data = data_final_stats,aes(xintercept = Value, color = Key))+
  scale_color_manual(values = c('Mean'="red",'Median'='blue'))+
  scale_fill_brewer(palette = "YlGnBu")+
  scale_x_continuous(breaks = seq(0,100,10))+
  theme_bw()+
  labs(title = "Age Distribution of Type 1 and Type 2 DM",y="Number of People", fill="DM type")+
  theme(plot.title =element_text(size = rel(2),hjust = 0.5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.5)),
        legend.title = element_text(size = rel(1.5)),
        strip.text = element_text(size = rel(1.5)))
dev.off()


#Blood pressure readings and anti hypertensive intake----
data_final %>% 
  filter((!is.na(SBP)&!is.na(DBP))& (dmtype %in% c('type 1', 'type 2'))& !is.na(anti_hypertensive)) %>% 
  ggplot(aes(x=SBP,y=DBP,color=anti_hypertensive))+
  geom_point(alpha=0.75,size=3)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,size = rel(2)), 
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size=rel(1)),
        legend.text = element_text(size = rel(1)))+
  labs(color="Anti hypertensive Intake",x="Systolic Blood Pressure",y="Diastolic Blood Pressure", 
      title ="Blood Pressure Measurements and Antihypertensive Intake")+
  scale_color_discrete(name="Anti hypertensive Intake",labels=c("Don't know","No","Yes"))



