rm(list = ls())
install.packages("knitr","dplyr","tidyr","lubridate")
library(knitr)
library(dplyr)
library(tidyr)
library(lubridate)

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("stringr")
library(stringr)
getwd()
setwd("/Users/amyjiravisitcul/Downloads/States/")
#----------------- NYSED tests ------
# data from https://data.nysed.gov/files/assessment/18-19/3-8-2018-19.zip
# further context on scale score ranges http://www.p12.nysed.gov/irs/pressRelease/20190822/documents/ela-2019-scale-score-performance-level-conversion-charts.pdf
nys <- read_excel("raw_data/NYS_3-8-2018-19/3-8_ELA_AND_MATH_RESEARCHER_FILE_2019.xlsx",
                  col_types = c("date","text","text","text","text","text",
                                "text","text","text","text","text",
                                "text","text","text","text","text",
                                "text","text","text","text","text","text","text"))
names(nys) <- tolower(names(nys))
View(nys)
names(nys)[7] <- "school_name"
nys$school_name <- as.factor(nys$school_name)
levels(nys$school_name)[968:969] <- c("ELMWOOD VILLAGE CHARTER SCHOOL","ELMWOOD VILLAGE CHARTER - HERTEL")
# change to match the BOCES naming convention for EVCS

nys_acad <- nys %>% 
  filter(subgroup_name == "All Students", # aggregated student performance
         is.na(county_code)==FALSE, # within a county
         str_detect(school_name,"DISTRICT")==FALSE, # remove whole-county rows
         str_detect(school_name,"COUNTY")==FALSE) %>% # remove whole-district rows
  mutate(n_tested = as.numeric(as.character(total_tested)),
         subject = as.factor(item_subject_area),
         item_desc = as.factor(item_desc), # Grade + subject variable
         mean_scale_score = as.numeric(as.character(mean_scale_score))) %>% 
  select(bedscode, school_name, subject, item_desc, n_tested,mean_scale_score) %>% na.omit()
levels(nys_acad$subject) <- c("ela","math")
names(nys_acad)

nys_means <- nys %>% # NYS's mean scores for all grades in math and ELA
  filter(subgroup_name == "All Students",
         school_name == "STATEWIDE - ALL DISTRICTS AND CHARTERS") %>% 
  select(bedscode,school_name,item_subject_area,item_desc,total_tested,mean_scale_score)
names(nys_means)[c(3,5)] <- c("subject","n_tested")
str(nys_acad)
nys_wmeans <- nys_acad %>% # weighted means by number of test takers
  group_by(school_name,item_desc) %>% 
  summarize(weighted.mean(mean_scale_score,n_tested),
            n_tested = n_tested)
names(nys_wmeans)[3] <- "wmeans"

nys_memb <- nys_wmeans %>% 
  filter(school_name == "ACADEMY OF THE CITY CHARTER SCHOOL"|
           str_detect(school_name,"BROOKLYN PROSPECT CHARTER SCHOOL")==TRUE|
           school_name == "CENTRAL QUEENS ACADEMY CHARTER SCHOOL"|
           school_name == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school_name == "COMPASS CHARTER SCHOOL"|
           str_detect(school_name,"HEBREW LANGUAGE")==TRUE|
           school_name == "HELLENIC CLASSICAL CHARTER SCHOOL"|
           school_name == "INTERNATIONAL CHARTER SCHOOL OF NEW YORK (THE)"|
           school_name == "NEW YORK FRENCH-AMERICAN CHARTER SCHOOL"|
           school_name == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school_name, "SUCCESS ACADEMY CHARTER SCHOOL",) == TRUE|
           school_name == "ELMWOOD VILLAGE CHARTER SCHOOL"|
           school_name == "ELMWOOD VILLAGE CHARTER - HERTEL"|
           school_name == "GENESEE COMMUNITY CHARTER SCHOOL")

sd <- nys_wmeans %>% 
  group_by(item_desc) %>% 
  summarize(sd_score = sd(wmeans)) # is this how to get the sd of NY scale scores by grade and subject?
nys_memb <- merge(nys_memb,sd,by="item_desc") # add column of sd value for each grade and subject
nys_memb <- merge(nys_memb,nys_means, by="item_desc") # add column of state means by grade and subject
str(nys_memb)

nys_memb<- nys_memb %>% 
  mutate(nys_mean = as.numeric(mean_scale_score),
         z_score = (wmeans - nys_mean)/sd_score,
         n_tested = n_tested.x) %>% 
  select(school_name.x,subject,item_desc,n_tested,wmeans,nys_mean,sd_score,z_score) %>%
  arrange(school_name.x,item_desc) 

write.csv(nys_memb,file = file.path("nys_acad_memb.csv"),row.names = FALSE)

#----- DC schools
dc <- read_excel("raw_data/2019 DC School Report Card Metric Scores Public Data 12.20.19.xlsx", 
                 sheet = "School STAR Metric Scores")
# data from https://drive.google.com/open?id=1y0hjFmXj6b2_mBAlyOwA3hLyLVUltrSq
str(dc)
names(dc) <- tolower(names(dc))
dc <- dc %>% 
  mutate(domain = as.factor(domain),
         school = as.factor(`school name`),
         lea = as.factor(`lea name`),
         subgroup = as.factor(`student group`),
         metric = as.factor(metric))
levels(dc$subgroup)
dc %>% 
  filter(subgroup == "All Students",
         domain == "Academic Achievement"|
           domain == "Academic Growth"|
           domain == "Educational Progress") %>% View()

dc <- read_excel("raw_data/DC_Detailed 2018-19 PARCC And MSAA Performance 2.19.20.Xlsx", sheet = "School Performance")
str(dc)
names(dc) <- tolower(names(dc))
levels(as.factor(dc$`lea name`))
dc <- dc %>% 
  mutate(n_tested = as.numeric(`total number valid test takers`),
         school = as.factor(`school name`),
         lea = as.factor(`lea name`))
