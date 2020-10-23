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

# ---- NYS demographics and subgroups diversity -----
nys_enrl <- read_excel("raw_data/NYS_enrollment_2019/Demographic Factors.xlsx")
names(nys_enrl) <- tolower(names(nys_enrl))
str(nys_enrl)
nys_enrl <- nys_enrl %>% 
  filter(year == "2019")%>% 
  mutate(school_name = entity_name)
memb_demog <- nys_enrl %>% 
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

# boces = board of cooperative educational services
boces <- read_excel("raw_data/NYS_enrollment_2019/BOCES and N_RC.xlsx")
memb_counties <- boces %>% 
  mutate(school_name = SCHOOL_NAME,
         county = COUNTY_NAME) %>% 
  filter(county=="ERIE"|
           county=="MONROE"|
           county=="NEW YORK"|
           county=="BRONX"|
           county=="KINGS"|
           county=="QUEENS") %>% 
  select(school_name, county)

memb_demog <- merge(memb_demog,memb_counties,by="school_name")
memb_demog <- memb_demog %>% 
  mutate(total = num_am_ind + num_asian + num_black + num_hisp + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_multi = num_multi,
         n_ed = num_ecdis,
         n_ell = num_ell,
         n_swd = num_swd) %>% 
  select(school_name, county, total, n_amind, n_asian, n_black, n_hisp, n_white, n_multi, n_ed, n_ell, n_swd)
write.csv(memb_demog, file = file.path('nys_membdemog.csv'), row.names = FALSE)

nys_enrl_filt <- merge(nys_enrl,memb_counties,by="school_name")
nys_enrl_filt <- nys_enrl_filt %>% # a row for every school in the same county as a member school
  mutate(total = num_am_ind + num_asian + num_black + num_hisp + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_multi = num_multi,
         n_ed = num_ecdis,
         n_ell = num_ell,
         n_swd = num_swd) %>% 
  select(school_name, county, total, n_amind, n_asian, n_black, n_hisp, n_white, n_multi, n_ed, n_ell, n_swd)
write.csv(nys_enrl_filt, file = file.path('nys_demog.csv'), row.names = FALSE) 

nys_entities <- nys_enrl %>% 
  filter(entity_name == "NYC Public Schools"|
           entity_name == "Large Cities"|
           entity_name == "High Need/Resource Urban-Suburban Districts"|
           entity_name == "Average Need Districts"|
           entity_name == "Low Need Districts"|
           entity_name == "Charter Schools"|
           entity_name == "BRONX County"|
           entity_name == "ERIE County"|
           entity_name == "KINGS County"|
           entity_name == "MONROE County"|
           entity_name == "NEW YORK County"|
           entity_name == "QUEENS County") %>% 
  arrange(entity_cd) %>% 
  mutate(total = num_am_ind + num_asian + num_black + num_hisp + num_white + num_multi) %>% 
  select(entity_name, total, num_am_ind, num_asian,num_black,num_hisp, num_white, num_multi,
         num_ecdis, num_ell, num_swd) %>% 
  mutate(count_poc = num_am_ind + num_asian + num_black + num_hisp + num_multi,
         count_white = num_white)
nys_entities <- nys_entities[-9,] #remove an individual building, and keep NYC PS as a district
nys_entities$county <- c("NA","NA","NA","NA","NA","NA",
                         "ERIE","MONROE", "NEW YORK","BRONX","KINGS","QUEENS")
nys_entities <- nys_entities %>% 
  select(entity_name, county, total, count_poc, count_white, num_am_ind, num_asian,num_black,num_hisp, num_white, num_multi,
         num_ecdis, num_ell, num_swd)
write.csv(nys_entities, file = file.path('nys_entities.csv'),row.names = FALSE)

dissim <- memb_demog %>% 
  mutate(n_poc = n_amind + n_black + n_asian + n_hisp + n_multi) %>% 
  select(school_name,county,total, n_poc, n_white)
dissim <- merge(dissim, nys_entities, by="county")
dissim %>% 
  mutate(dis_county = abs((n_poc/count_poc)-(n_white/count_white)),
         dis_largecities = abs((n_poc/as.numeric(nys_entities[2,4]))-
                                 (n_white/as.numeric(nys_entities[2,5]))),
         dis_avgneed = abs((n_poc/as.numeric(nys_entities[4,4]))-
                             (n_white/as.numeric(nys_entities[4,5]))),
         dis_lowneed = abs((n_poc/as.numeric(nys_entities[5,4]))-
                              (n_white/as.numeric(nys_entities[5,5]))),
         dis_example = abs((n_poc/1000)-(n_white/2000))) %>% View()

install.packages('segregation')
library(segregation)
# which schools are most segregated?
load("~/Downloads/schools00.rda")
localseg = mutual_local(schools00, "race", "school",
                         weight = "n", wide = TRUE)
str(schools00)
View(localseg)
schools00 %>% 
  filter(school == "B136_2"|
           school == "A67_2"|
           school == "C7_2") %>% View()
