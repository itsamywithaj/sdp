rm(list = ls())
install.packages("tidyverse") # install tidyverse
library(tidyverse)
install.packages("readxl") # CRAN version
library(readxl)
install.packages("data.table")
library(data.table)
install.packages('segregation')
library(segregation)
getwd()
setwd("/Users/amyjiravisitcul/Downloads/States/")


# ---- STUDENT COUNTS BY RACE AT SCHOOL- COUNTY- BOCES- DISTRICT- LEVELS -----
# zip file of data imported from https://data.nysed.gov/files/enrollment/18-19/enrollment_2019.zip
# open in Access and save as Excel
nys_enrl <- read_excel("raw_data/NYS_enrollment_2019/Demographic Factors.xlsx")
names(nys_enrl) <- tolower(names(nys_enrl))
str(nys_enrl)
nys_enrl <- nys_enrl %>% 
  filter(year == "2019")
memb_demog <- nys_enrl %>% 
  filter(entity_name == "ACADEMY OF THE CITY CHARTER SCHOOL"|
           str_detect(entity_name,"BROOKLYN PROSPECT CHARTER SCHOOL")|
           entity_name == "CENTRAL QUEENS ACADEMY CHARTER SCHOOL"|
           entity_name == "COMMUNITY ROOTS CHARTER SCHOOL"|
           entity_name == "COMPASS CHARTER SCHOOL"|
           str_detect(entity_name,"HEBREW LANGUAGE")|
           entity_name == "HELLENIC CLASSICAL CHARTER SCHOOL"|
           entity_name == "INTERNATIONAL CHARTER SCHOOL OF NEW YORK (THE)"|
           entity_name == "NEW YORK FRENCH-AMERICAN CHARTER SCHOOL"|
           entity_name == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(entity_name, "SUCCESS ACADEMY CHARTER SCHOOL")|
           entity_name == "ELMWOOD VILLAGE CHARTER SCHOOL"|
           entity_name == "ELMWOOD VILLAGE CHARTER - HERTEL"|
           entity_name == "GENESEE COMMUNITY CHARTER SCHOOL")

# boces = board of cooperative educational services
boces <- read_excel("raw_data/NYS_enrollment_2019/BOCES and N_RC.xlsx")
memb_counties <- boces %>% 
  mutate(entity_name = SCHOOL_NAME,
         county = COUNTY_NAME) %>% 
  filter(county=="ERIE"|
           county=="MONROE"|
           county=="NEW YORK"|
           county=="BRONX"|
           county=="KINGS"|
           county=="QUEENS") %>% 
  select(entity_name, county)

memb_demog <- merge(memb_demog,memb_counties,by="entity_name")
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
  select(entity_name, county, total, n_amind, n_asian, n_black, n_hisp, n_white, n_multi, n_ed, n_ell, n_swd)
write.csv(memb_demog, file = file.path('nys_membdemog.csv'), row.names = FALSE)


# ---- BUFFALO AND ROCHESTER CITY DISTRICTS AND COUNTIES ----
nys_enrl <- read_excel("raw_data/NYS_enrollment_2019/Demographic Factors.xlsx")
# zip file of data imported from https://data.nysed.gov/files/enrollment/18-19/enrollment_2019.zip
names(nys_enrl) <- tolower(names(nys_enrl))
str(nys_enrl)
nys_enrl <- nys_enrl %>% 
  filter(year == "2019")
# ----- Rochester City School District = entity_cd beginning with 2616 ----
rcsd <- nys_enrl %>% 
  filter(str_detect(entity_cd,"2616"),
         str_detect(entity_cd, "........0000")==FALSE) # remove the total district as a row
str(rcsd)
rcsd <- rcsd %>% 
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed) %>% 
  select(entity_name, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed)
race_rcsd <- rcsd %>% 
  gather(key = "race", value = "n", n_amind:n_mult, factor_key = TRUE) %>% 
  select(entity_name, year, total, race, n)
rcsd_locseg = mutual_local(race_rcsd, "race","entity_name", weight = "n", wide = TRUE)
names(rcsd_locseg) <- c("school","ls_district","p_district")

# ----- filter out for Monroe County, entity_cd begins with 26 -----
monroe <- nys_enrl %>% 
  filter(str_detect(entity_cd, "^26"),
         str_detect(entity_cd, ".*0000")==FALSE) %>% # remove rows coded as full districts
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed) %>% 
  select(entity_cd, entity_name, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed)
race_monroe <- monroe %>% 
  gather(key = "race", value = "n",n_amind:n_mult,factor_key = TRUE) %>% 
  select(entity_cd, entity_name, year, total, race, n)
mon_locseg = mutual_local(race_monroe,"race","entity_name", weight = "n", wide = TRUE)
names(mon_locseg) <- c("school","ls_county","p_county")

nys_memb_locseg <- rcsd_locseg %>% 
  filter(school == "GENESEE COMMUNITY CHARTER SCHOOL")
a <- mon_locseg %>% 
  filter(school == "GENESEE COMMUNITY CHARTER SCHOOL") 

nys_memb_locseg <- merge(nys_memb_locseg,a,by="school")

# How segregated is each district compared to the whole county?
mon_dist <- nys_enrl %>% 
  filter(str_detect(entity_cd, "^26"),
         str_detect(entity_cd, ".*0000")) %>% 
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed) %>% 
  select(entity_cd, entity_name, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed)
mon_dist_race <- mon_dist %>% 
  gather(key = "race", value = "n",n_amind:n_mult,factor_key = TRUE) %>% 
  select(entity_cd, entity_name, year, total, race, n)
mondist_locseg = mutual_local(mon_dist_race,"race","entity_name", weight = "n", wide = TRUE)
names(mondist_locseg) <- c("district","ls","p")

# --- Buffalo City School District, entity_cd begins with 1406 ----
buffalo <- nys_enrl %>% 
  filter(str_detect(entity_cd,"^1406"),
         str_detect(entity_cd,".*0000")==FALSE) # remove total district as a row
buffalo <- buffalo %>% 
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed) %>% 
  select(entity_name, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed)
race_buff <- buffalo %>% 
  gather(key = "race", value = "n", n_amind:n_mult, factor_key = TRUE) %>% 
  select(entity_name, year, total, race, n)
buff_locseg = mutual_local(race_buff, "race","entity_name", weight = "n", wide = TRUE)
names(buff_locseg) <- c("school","ls_district","p_district")

# --- Erie County entity_cd begins with 14 ----
erie <- nys_enrl %>% 
  filter(str_detect(entity_cd, "^14"),
         str_detect(entity_cd, ".*0000")==FALSE) %>% # remove rows coded as full districts
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed) %>% 
  select(entity_cd, entity_name, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed)
race_erie <- erie %>% 
  gather(key = "race", value = "n",n_amind:n_mult,factor_key = TRUE) %>% 
  select(entity_cd, entity_name, year, total, race, n)
erie_locseg = mutual_local(race_erie,"race","entity_name", weight = "n", wide = TRUE)
names(erie_locseg) <- c("school","ls_county","p_county")

a <- buff_locseg %>% 
  filter(str_detect(school, "ELMWOOD VILLAGE"))
b <- erie_locseg %>% 
  filter(str_detect(school, "ELMWOOD VILLAGE"))
c <- merge(a,b,by="school")
nys_memb_locseg <- rbind(nys_memb_locseg,c)
write.csv(nys_memb_locseg,file = file.path('nys_memb_locseg.csv'), row.names = FALSE)

erie_distlocseg <- nys_enrl %>% filter(str_detect(entity_cd, "^14"),
                         str_detect(entity_cd, ".*0000")==TRUE)
erie_distlocseg <- erie_distlocseg %>% 
  mutate(total = num_am_ind + num_black + num_hisp +
                            num_asian + num_white + num_multi,
                          n_amind = num_am_ind,
                          n_asian = num_asian,
                          n_black = num_black,
                          n_hisp = num_hisp,
                          n_white = num_white,
                          n_mult = num_multi,
                          n_ell = num_ell,
                          not_ell = total - n_ell,
                          n_swd = num_swd,
                          not_swd = total - n_swd,
                          n_ed = num_ecdis,
                          not_ed = total - n_ed) %>% 
  select(entity_cd, entity_name, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed)
erie_distlocseg <- erie_distlocseg %>% 
  gather(key = "race", value = "n",n_amind:n_mult,factor_key = TRUE) %>% 
  select(entity_cd, entity_name, year, total, race, n)
erie_distlocseg = mutual_local(erie_distlocseg,"race","entity_name", weight = "n", wide = TRUE)
View(erie_distlocseg)
# ---- NYC STUDENT COUNTS BY GEOGRAPHIC DISTRICT AND AGGREGATE ----
nyc_enroll <- nys_enrl %>% 
  filter(year == "2019",
         str_detect(entity_cd,"^3"),
         str_detect(entity_cd,"........0000")==FALSE) %>% 
  mutate(split = entity_cd) %>% 
  separate(split, c("other", "district","drop"), # entity_cd's digits 3 and 4 are the district
           sep = c(2,4)) %>% 
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed,
         dist = as.numeric(district)) %>% 
  select(entity_cd, entity_name, dist, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult,
         n_ell, not_ell, n_swd, not_swd, n_ed, not_ed) %>% 
  filter(dist > 1 & dist < 10| # NYC DOE districts where members are situated
           dist > 12 & dist < 18|
           dist > 20 & dist < 33 & dist != 23 &
           dist != 25 & dist != 26 & dist != 28 & dist != 31)

nyc_distlocseg <- nys_enrl %>% 
  filter(year == "2019",
         str_detect(entity_cd,"^3"),
         str_detect(entity_cd,"........0000")==TRUE) %>% # District totals
  mutate(split = entity_cd) %>% 
  separate(split, c("other", "district","drop"), # entity_cd's digits 3 and 4 are the district
           sep = c(2,4)) %>% 
  mutate(total = num_am_ind + num_black + num_hisp +
           num_asian + num_white + num_multi,
         n_amind = num_am_ind,
         n_asian = num_asian,
         n_black = num_black,
         n_hisp = num_hisp,
         n_white = num_white,
         n_mult = num_multi,
         n_ell = num_ell,
         not_ell = total - n_ell,
         n_swd = num_swd,
         not_swd = total - n_swd,
         n_ed = num_ecdis,
         not_ed = total - n_ed,
         dist = as.numeric(district)) %>% 
  select(entity_cd, entity_name, dist, year, total, n_amind, n_asian, n_black, n_hisp, n_white, n_mult)
nyc_distlocseg <- nyc_distlocseg %>% 
  gather(key = "race", value = "n", n_amind:n_mult, factor_key=TRUE)
nyc_distlocseg = mutual_local(nyc_distlocseg,"race","entity_name", weight = "n", wide = TRUE)

# ---- NYC district multi-group segregation measure ----
nyc_pkg <- nyc_enroll %>% 
  gather(key = "race", value = "n", n_amind:n_mult, factor_key = TRUE) %>% 
  mutate(school = entity_name) %>% 
  select(entity_cd, school, dist, year, total, race, n)

district2 <- nyc_pkg %>% 
  filter(dist == 2) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(dist = 2)
district3 <- nyc_pkg %>% 
  filter(dist == 3) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 3)
district4 <- nyc_pkg %>% 
  filter(dist == 4) %>% 
  mutual_local("race","school",weight = "n",wide = TRUE)%>% 
  mutate(dist = 4)
district5 <- nyc_pkg %>% 
  filter(dist == 5) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 5)
district6 <- nyc_pkg %>% 
  filter(dist == 6) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 6)
district7 <- nyc_pkg %>% 
  filter(dist == 7) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 7)
district8 <- nyc_pkg %>% 
  filter(dist == 8) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 8)
district9 <- nyc_pkg %>% 
  filter(dist == 9) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 9)
district13 <- nyc_pkg %>% 
  filter(dist == 13) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 13)
district14 <- nyc_pkg %>% 
  filter(dist == 14) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 14)
district15 <- nyc_pkg %>% 
  filter(dist == 15) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 15)
district17 <- nyc_pkg %>% 
  filter(dist == 17) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 17)
district21 <- nyc_pkg %>% 
  filter(dist == 21) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 21)
district22 <- nyc_pkg %>% 
  filter(dist == 22) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 22)
district24 <- nyc_pkg %>% 
  filter(dist == 24) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 24)
district27 <- nyc_pkg %>% 
  filter(dist == 27) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 27)
district29 <- nyc_pkg %>% 
  filter(dist == 29) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 29)
district30 <- nyc_pkg %>% 
  filter(dist == 30) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 30)
district32 <- nyc_pkg %>% 
  filter(dist == 32) %>% 
  mutual_local("race","school",weight = "n", wide = TRUE)%>% 
  mutate(dist = 32)

nyc_districts <- rbind(district2,district3,district4,district5,district6,district7, district8,
                         district9,district13,district14,district15,district17,district21,district22,
                         district24,district27,district29,district30, district32)
# members ------
names(nyc_districts) <- c("school","ls_district","p_district","district")
nyc_memb_locseg <- nyc_districts %>% 
  filter(school == "ACADEMY OF THE CITY CHARTER SCHOOL"|
           str_detect(school,"BROOKLYN PROSPECT CHARTER")|
           school == "CENTRAL QUEENS ACADEMY CHARTER SCHOOL"|
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           school == "HELLENIC CLASSICAL CHARTER SCHOOL"|
           school == "INTERNATIONAL CHARTER SCHOOL OF NY"|
           school == "NEW YORK FRENCH-AMERICAN CHARTER SCHOOL"|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACADEMY")|
           school == "ELMWOOD VILLAGE CHARTER SCHOOL"|
           school == "ELMWOOD VILLAGE CHARTER - HERTEL"|
           school == "GENESEE COMMUNITY CHARTER SCHOOL") %>% 
  arrange(school)

# ---- COMBINING ENTITIES FOR MEMBER-SPECIFIC SEGREGATION DATA ------
nys_memb_locseg <- nys_memb_locseg %>% 
  mutate(district = c("RCSD","BCSD","BCSD"),
         county = c("Monroe","Erie","Erie")) %>% 
  select(school, district, county, ls_district, p_district, ls_county, p_county)

nyc_locseg <- nyc_pkg %>% 
  mutual_local("race","school",weight = "n", wide = TRUE) %>% 
  mutate(county = "NYC",
         ls_county = ls,
         p_county = p) %>% 
  select(school, county, ls_county, p_county) %>% 
  filter(school == "ACADEMY OF THE CITY CHARTER SCHOOL"|
           str_detect(school,"BROOKLYN PROSPECT CHARTER")|
           school == "CENTRAL QUEENS ACADEMY CHARTER SCHOOL"|
           school == "COMMUNITY ROOTS CHARTER SCHOOL"|
           school == "COMPASS CHARTER SCHOOL"|
           str_detect(school,"HEBREW LANGUAGE")|
           school == "HELLENIC CLASSICAL CHARTER SCHOOL"|
           school == "INTERNATIONAL CHARTER SCHOOL OF NY"|
           school == "NEW YORK FRENCH-AMERICAN CHARTER SCHOOL"|
           school == "BROOKLYN URBAN GARDEN CHARTER SCHOOL"|
           str_detect(school, "SUCCESS ACADEMY")|
           school == "ELMWOOD VILLAGE CHARTER SCHOOL"|
           school == "ELMWOOD VILLAGE CHARTER - HERTEL"|
           school == "GENESEE COMMUNITY CHARTER SCHOOL")
nyc_memb_locseg <- merge(nyc_memb_locseg,nyc_locseg,by="school")
nyc_memb_locseg <- nyc_memb_locseg %>% 
  select(school, district, county, ls_district, p_district, ls_county, p_county)
ny_memb_locseg <- rbind(nyc_memb_locseg,nys_memb_locseg)
write.csv(ny_memb_locseg, file = file.path('ny_memb_locseg.csv'), row.names = FALSE)

# How racially segregated is New YOrk City across districts? ----
# Across schools? Across schools within districts? ----
mutual_total(nyc_pkg, "race","school", weight = "n") # M = 0.41, H = 0.30
mutual_total(nyc_pkg, "race","dist", weight = "n") # M = 0.21, H = 0.15
mutual_total(nyc_pkg, "race","school", within = "dist",weight = "n") # M = 0.21, H = 0.15


# "NYC" as a county, Mutual Info Index gives that this county has an evenness of 0.21 across districts
mutual_dist2 <- nyc_pkg %>% 
  filter(dist == 2) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 2) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)

levels(as.factor(nyc_pkg$dist))
mutual_dist3 <- nyc_pkg %>% 
  filter(dist == 3) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 3) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_dist2,mutual_dist3)

mutual_dist4 <- nyc_pkg %>% 
  filter(dist == 4) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 4) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist4)

mutual_dist5 <- nyc_pkg %>% 
  filter(dist == 5) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 5) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist5)

mutual_dist6 <- nyc_pkg %>% 
  filter(dist == 6) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 6) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist6)

mutual_dist7 <- nyc_pkg %>% 
  filter(dist == 7) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 7) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist7)

mutual_dist8 <- nyc_pkg %>% 
  filter(dist == 8) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 8) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist8)

mutual_dist9 <- nyc_pkg %>% 
  filter(dist == 9) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 9) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist9)

mutual_dist13 <- nyc_pkg %>% 
  filter(dist == 13) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 13) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist13)

mutual_dist14 <- nyc_pkg %>% 
  filter(dist == 14) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 14) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist14)

mutual_dist15 <- nyc_pkg %>% 
  filter(dist == 15) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 15) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist15)

mutual_dist16 <- nyc_pkg %>% 
  filter(dist == 16) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 16) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist16)

mutual_dist17 <- nyc_pkg %>% 
  filter(dist == 17) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 17) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist17)

mutual_dist21 <- nyc_pkg %>% 
  filter(dist == 21) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 21) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist21)

mutual_dist22 <- nyc_pkg %>% 
  filter(dist == 22) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 22) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist22)

mutual_dist24 <- nyc_pkg %>% 
  filter(dist == 24) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 24) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist24)

mutual_dist27 <- nyc_pkg %>% 
  filter(dist == 27) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 27) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist27)

mutual_dist29 <- nyc_pkg %>% 
  filter(dist == 29) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 29) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist29)

mutual_dist30 <- nyc_pkg %>% 
  filter(dist == 30) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 30) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist30)

mutual_dist32 <- nyc_pkg %>% 
  filter(dist == 32) %>% 
  mutual_total("race","school",weight="n") %>% 
  mutate(district = 32) %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_dist32)

# Rochester
View(race_rcsd)
mutual_roch <- race_rcsd %>% 
  mutual_total("race","entity_name", weight = "n") %>% 
  mutate(district = "RCSD") %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_roch)

# Buffalo
View(race_buff)
mutual_buff <- race_buff %>% 
  mutual_total("race","entity_name", weight = "n") %>% 
  mutate(district = "BCSD") %>% 
  select(district, stat, est) %>% 
  spread(stat,est)
mutual_districts <- rbind(mutual_districts,mutual_buff)
write.csv(mutual_districts, file = file.path("mutual_districts.csv"),row.names = FALSE)

# -------- County-level segregation ------
mutual_county <- nyc_pkg %>% 
  mutual_total("race","dist",weight = "n") %>% 
  mutate(county = "NYC") %>% 
  select(county, stat, est) %>% 
  spread(stat,est)

mutual_mon <- mon_dist_race %>% 
  mutual_total("race","entity_name",weight = "n") %>% 
  mutate(county = "Monroe") %>% 
  select(county, stat, est) %>% 
  spread(stat,est)
mutual_county <- rbind(mutual_county,mutual_mon)

mutual_erie <- race_erie %>% 
  mutual_total("race","entity_name",weight = "n") %>% 
  mutate(county = "Erie") %>% 
  select(county, stat,est) %>% 
  spread(stat,est)
mutual_county <- rbind(mutual_county, mutual_erie)  
write.csv(mutual_county, file = file.path('mutual_county.csv'),row.names = FALSE)


#----------------- NYSED ACADEMIC ASSESSMENT ------
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
         str_detect(bedscode, ".......0000")==FALSE, # take out the rows for district and county
         bedscode != 0, # take out the rows for statewide
         bedscode != 1, # take out the rows for NYC
         bedscode != 2, # take out the rows for only Buffalo, Yonkers, Rochester, Syracuse
         bedscode != 3, # take out the rows for urban-suburban high needs
         bedscode != 4, # take out the rows for rural high needs
         bedscode != 5, ## take out the rows for average needs
         bedscode != 6, # take out the rows for low needs
         bedscode !=7) %>% # remove rows for aggregate charters
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
a <- ddply(nys_memb, .(school_name, subject), summarize, # summarize by school and subject
      n_students = sum(n_tested), # column for total students tested across all grades
      min_grade = min(grade), # make columns for the grade range
      max_grade = max(grade),
      z_wgt = weighted.mean(z_score,n_tested))  # calculate z score based on n_tested of each subject
c <- ddply(nys_memb, .(school_name),summarize, # summarize by just the school
           n_students = sum(n_tested), # column for total students tested across all grades
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade),
           z_wgt = weighted.mean(z_score, n_tested)) # calculate z score based on n_tested of both subjects
c$subject <- "both math and ELA"
nys_memb <- rbind(a,c) %>% arrange(school_name, subject)

# ---- Save data on counties and districts ----
write.csv(nys_memb,file = file.path("nys_acad_memb2.csv"),row.names = FALSE)
write.csv(erie_locseg, file.path('erie_locseg.csv'),row.names = FALSE)
write.csv(mon_locseg, file.path('mon_locseg.csv'),row.names = FALSE)


# --- combining academics and demographics -----
nys_test <- nys_memb %>% 
  filter(subject == "both math and ELA") %>% 
  mutate(school = school_name) %>% 
  select(school, subject, n_students, min_grade, max_grade, z_wgt)
nys_merged <- merge(nys_memb_locseg,nys_test, by="school")
names(nys_merged)[9:12] <- c("n_both","min_both","max_both","z_both")
nys_test <- nys_memb %>% 
  filter(subject == "Math") %>% 
  mutate(school = school_name,
         n_math = n_students,
         min_math = min_grade,
         max_math = max_grade,
         z_math = z_wgt) %>% 
  select(school, n_math, min_math, max_math, z_math)
nys_merged <- merge(nys_merged,nys_test,by="school")
nys_test <- nys_memb %>% 
  filter(subject == "ELA") %>% 
  mutate(school = school_name,
         n_ela = n_students,
         min_ela = min_grade,
         max_ela = max_grade,
         z_ela = z_wgt,
         borough = NA,
         ls_bor = NA,
         p_bor = NA) %>% 
  select(school, borough, ls_bor, p_bor, n_ela, min_ela, max_ela, z_ela)
nys_merged <- merge(nys_merged,nys_test,by="school")

names(nys_memb)[1] <- "school"
nys_memb[1]
ny_memb_locseg %>% 
  select(school)
ny_memb_locseg[3,1] <- "BROOKLYN PROSPECT CHARTER SCHOOL-CSD 13"
ny_memb_locseg[2,1] <- "BROOKLYN PROSPECT CHARTER SCHOOL-CSD 15"
ny_memb_locseg[12,1] <- "INTERNATIONAL CHARTER SCHOOL OF NEW YORK (THE)"
ny_memb_locseg[20,1] <- "SUCCESS ACADEMY CHARTER SCHOOL - BED STUY 1"
ny_memb_locseg[43,1] <- "SUCCESS ACADEMY CHARTER SCHOOL - WASHINGTON HEIGHTS"
ny_memb_locseg[18,1] <- "SUCCESS ACADEMY CHARTER SCHOOL - SPRINGFIELD GARDENS"
ny_memb_locseg[14,1] <- "SUCCESS ACADEMY CHARTER SCHOOL-BUSHWICK"
ny_memb_locseg[15,1] <- "SUCCESS ACADEMY CHARTER SCHOOL-SOUTH JAMAICA"
ny_memb_locseg[33,1] <- "SUCCESS ACADEMY CHARTER SCHOOL-FAR ROCKAWAY"
ny_memb_locseg[34,1] <- "SUCCESS ACADEMY CHARTER SCHOOL-FLATBUSH"
ny_memb_locseg[42,1] <- "SUCCESS ACADEMY CHARTER SCHOOL - ROSEDALE"

nys_test <- nys_memb %>% 
  filter(subject == "ELA") %>% 
  mutate(n_ela = n_students,
         min_ela = min_grade,
         max_ela = max_grade,
         z_ela = z_wgt) %>% 
  select(school, n_ela, min_ela, max_ela, z_ela)
z <- merge(ny_memb_locseg,nys_test,by="school")

nys_test <- nys_memb %>% 
  filter(subject == "Math") %>% 
  mutate(n_math = n_students,
         min_math = min_grade,
         max_math = max_grade,
         z_math = z_wgt) %>% 
  select(school, n_math, min_math, max_math, z_math)
y <- merge(z,nys_test,by="school")

nys_test <- nys_memb %>% 
  filter(subject == "both math and ELA") %>% 
  mutate(n_both = n_students,
         min_both = min_grade,
         max_both = max_grade,
         z_both = z_wgt) %>% 
  select(school, n_both, min_both, max_both, z_both)
x <- merge(y,nys_test,by="school")
names(x)
names(nys_merged)
nys_merged <- nys_merged %>% 
  select(names(x))
nys_merged <- rbind(x, nys_merged)

write.csv(nys_merged,file = file.path('nys_merged.csv'), row.names = FALSE)

# ---- test performance by subgroup ----
acad_byrace <- nys %>% 
  separate(item_desc, c("drop","grade","subject"),sep=" ") %>% 
  filter(subgroup_name == "American Indian or Alaska Native"|
           subgroup_name == "Asian or Pacific Islander"|
           subgroup_name == "Black or African American"|
           subgroup_name == "Hispanic or Latino"|
           subgroup_name == "Multiracial"|
           subgroup_name == "White", 
         str_detect(bedscode, ".......0000")==FALSE, # take out the rows for district and county
         bedscode != 0, # take out the rows for statewide
         bedscode != 1, # take out the rows for NYC
         bedscode != 2, # take out the rows for only Buffalo, Yonkers, Rochester, Syracuse
         bedscode != 3, # take out the rows for urban-suburban high needs
         bedscode != 4, # take out the rows for rural high needs
         bedscode != 5, ## take out the rows for average needs
         bedscode != 6, # take out the rows for low needs
         bedscode !=7) %>% # take out the rows for only charters
  mutate(n_tested = as.numeric(as.character(total_tested)),
         mean_scale_score = as.numeric(as.character(mean_scale_score))) %>% 
  select(bedscode,school_name, subgroup_name, subject, grade, n_tested,mean_scale_score) %>% na.omit()

nys_memb <- acad_byrace %>% 
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
library(tidyverse)
nys_wmeans <- acad_byrace %>% 
  group_by(school_name,grade,subject,subgroup_name) %>% 
  summarize(wmeans = weighted.mean(mean_scale_score,n_tested),
            n_tested = n_tested) %>% na.omit()
sd <- nys_wmeans %>% 
  group_by(grade, subject, subgroup_name) %>% 
  summarize(sd = sd(wmeans))
gradesubgroup_means <- nys_wmeans %>% 
  group_by(grade, subject, subgroup_name) %>% 
  summarize(nys_mean = mean(wmeans))
membsub_wmeans <- nys_wmeans %>% 
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
a <- merge(membsub_wmeans,sd,by=c("grade","subject","subgroup_name")) # add column of sd value for each grade, subject, subgroup
nys_memb <- merge(a,gradesubgroup_means, by=c("grade","subject","subgroup_name"))# add column of weighted means by grade subject and subgroup
str(nys_memb)

nys_memb<- nys_memb %>% 
  mutate(z_score = (wmeans - nys_mean)/sd) %>% # standardized measure of student performance among same grade and race
  select(school_name,subgroup_name,grade,subject,n_tested,wmeans,nys_mean,sd,z_score) %>%
  arrange(school_name,grade,subject,subgroup_name) 

library(plyr)
a <- ddply(nys_memb, .(school_name, subject, subgroup_name), summarize, # summarize by school subject and subgroup
           n_students = sum(n_tested), # column for total students tested across all grades
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade),
           z_wgt = weighted.mean(z_score,n_tested))  # calculate z score based on n_tested of each subject
c <- ddply(nys_memb, .(school_name, subgroup_name),summarize, # summarize by just the school
           n_students = sum(n_tested), # column for total students tested across all grades
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade),
           z_wgt = weighted.mean(z_score, n_tested)) # calculate z score based on n_tested of both subjects

d <- ddply(nys_memb, .(school_name, subgroup_name),summarize, # summarize by just the school
           n_students = sum(n_tested), # column for total students tested across all grades
           min_grade = min(grade), # make columns for the grade range
           max_grade = max(grade),
           wmeans = weighted.mean(wmeans, n_tested))

d <- d %>% 
       select(school_name, subgroup_name,wmeans) %>% 
       pivot_wider(names_from = subgroup_name,
                   values_from = wmeans)
       
write.csv(d, file = file.path('nys_acadrace_means.csv'),row.names = FALSE)

c$subject <- "both math and ELA"
nys_memb2 <- rbind(a,c) %>% arrange(school_name, subject)
subgroup_ref <- merge(gradesubgroup_means,sd, by=c("grade","subject","subgroup_name"))
names(sd)
write.csv(nys_memb, file = file.path('nys_memb_acadbyrace.csv'), row.names = FALSE) # saves with weighted mean score for each group
write.csv(nys_memb2, file = file.path('nys_memb_acadbyrace2.csv'), row.names = FALSE)
write.csv(subgroup_ref, file = file.path('nys_subgroupref.csv'),row.names = FALSE)
