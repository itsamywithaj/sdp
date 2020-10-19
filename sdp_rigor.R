rm(list = ls())
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("data.table")
library(data.table)
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
  separate(item_desc, c("drop","grade","subject"),sep=" ") %>% 
  filter(subgroup_name == "All Students", # aggregated student performance
         is.na(county_code)==FALSE, # within a county
         str_detect(school_name,"DISTRICT")==FALSE, # remove whole-county rows
         str_detect(school_name,"COUNTY")==FALSE) %>% # remove whole-district rows
  mutate(n_tested = as.numeric(as.character(total_tested)),
         mean_scale_score = as.numeric(as.character(mean_scale_score))) %>% 
  select(school_name, subject, grade, n_tested,mean_scale_score) %>% na.omit()
names(nys_acad)

nys_means <- nys %>% # NYS's mean scores for all grades in math and ELA
  separate(item_desc, c("drop","grade","subject"),sep=" ") %>% 
  filter(subgroup_name == "All Students",
         school_name == "STATEWIDE - ALL DISTRICTS AND CHARTERS") %>% 
  select(bedscode,school_name,subject, grade,total_tested,mean_scale_score)
names(nys_means)[5] <- "n_tested"
str(nys_acad)

nys_memb <- nys_acad %>% 
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
nys_wmeans <- nys_acad %>% 
  group_by(school_name,grade,subject) %>% 
  summarize(wmeans = weighted.mean(mean_scale_score,n_tested),
            n_tested = n_tested) %>% na.omit()
sd <- nys_wmeans %>% 
  group_by(grade, subject) %>% 
  summarize(sd = sd(wmeans)) # is this how to get the sd of NY scale scores by grade and subject?
nys_memb <- merge(nys_memb,sd,by=c("grade","subject")) # add column of sd value for each grade and subject
nys_memb <- merge(nys_memb,nys_means[,-c(1:2,5)], by=c("grade","subject")) # add column of state means by grade and subject
str(nys_memb)

nys_memb<- nys_memb %>% 
  mutate(school_mean = as.numeric(mean_scale_score.x),
         nys_mean = as.numeric(mean_scale_score.y),
         z_score = (school_mean - nys_mean)/sd) %>% 
  select(school_name,grade,subject,n_tested,school_mean,nys_mean,sd,z_score) %>%
  arrange(school_name,grade,subject) 

library(plyr)
a <- ddply(nys_memb, .(school_name, subject), summarize, 
      n_students = sum(n_tested), # column for and for the total students tested across all grades
      min_grade = min(grade), # make columns for the grade range
      max_grade = max(grade),
      z_wgt = weighted.mean(z_score,n_tested))  
c <- ddply(nys_memb, .(school_name),summarize,
           n_students = sum(n_tested),
           min_grade = min(grade),
           max_grade = max(grade),
           z_wgt = weighted.mean(z_score, n_tested))
c$subject <- "both math and ELA"
nys_memb <- rbind(a,c) %>% arrange(school_name, subject)

write.csv(nys_memb,file = file.path("nys_acad_memb2.csv"),row.names = FALSE)