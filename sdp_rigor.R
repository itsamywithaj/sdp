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

nys <- read_excel("raw_data/NYS_3-8-2018-19/3-8_ELA_AND_MATH_RESEARCHER_FILE_2019.xlsx")
names(nys) <- tolower(names(nys))
str(nys)
names(nys)[7] <- "school_name"
nys$school_name <- as.factor(nys$school_name)
levels(nys$school_name)[968:969] <- c("ELMWOOD VILLAGE CHARTER SCHOOL","ELMWOOD VILLAGE CHARTER - HERTEL")
# change to match the BOCES naming convention for EVCS
str(nys)
nys$bedscode <- as.factor(nys$bedscode)
levels(as.factor(nys$school_name))

nys_acad <- nys %>% 
  filter(subgroup_name == "All Students",
         is.na(county_code)==FALSE,
         str_detect(school_name,"DISTRICT")==FALSE,
         str_detect(school_name,"COUNTY")==FALSE) %>% 
  mutate(n_tested = total_tested,
         c_lev34 = as.numeric(l3_count)+as.numeric(l4_count),
         p_lev34 = as.numeric(`l3-l4_pct`),
         subject = as.factor(item_subject_area),
         item_desc = as.factor(item_desc)) %>% 
  select(bedscode, school_name, subject, n_tested, item_desc,
         c_lev34, p_lev34) %>% na.omit()
levels(nys_acad$subject) <- c("ela","math")

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

nys_gradesubject <- nys_acad %>% 
  group_by(item_desc) %>% 
  summarize(m_lev34 = mean(p_lev34),
            sd_lev34 = sd(p_lev34),
            n= n())
nys_acad_comp <- merge(nys_gradesubject,nys_memb, by="item_desc")
nys_acad_comp <- nys_acad_comp %>% 
  mutate(sds = (p_lev34 - m_lev34)/sd_lev34) %>% 
  select(school_name, item_desc, n_tested, c_lev34, p_lev34,m_lev34,sds)%>%
  arrange(school_name, item_desc)
write.csv(nys_acad_comp,file = file.path("nys_acad_comp.csv"),row.names = FALSE)
