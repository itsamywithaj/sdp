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
setwd("/Users/amyjiravisitcul/Downloads/DC deep dive/")


# ---- import the data ----
# file of 2019-2020 attendance data from https://osse.dc.gov/publication/dc-attendance-report-2019-20-school-year
dc_enrl <- read_excel("2020 Attendance - Metric Scores.xlsx", 
                      sheet ="School Metric Scores")
names(dc_enrl) <- tolower(names(dc_enrl))
str(dc_enrl)
dc_enrl <- dc_enrl %>% 
  mutate(lea = as.factor(`lea name`),
         lea_code = as.factor(`lea code`),
         school = as.factor(`school name`),
         school_code = as.factor(`school code`),
         group = as.factor(`student group`),
         n_students = `metric n`) %>% 
  select(lea, lea_code, school, school_code, group,
         metric, `metric score`,n_students)
levels(as.factor(dc_enrl$group))

# ---- DC school-level race demographics -----
dc_race <- dc_enrl %>% 
  mutate(n_suppressed = as.character(n_students), # acknowledge suppressed data
         n_students = as.numeric(n_students)) %>% 
  filter(metric == "In-Seat Attendance", 
         group == "Asian"|group=="Black/African-American"| # filter to only look at race, 
           group == "Hispanic/Latino of any race"| # separate from EL and SWD
           group == "Native Hawaiian/Other Pacific Islander"|
           group == "American Indian/Alaskan Native"|
           group == "White"| group == "Two or more races"|
           group =="All Report Card Students") %>% 
  select(lea, lea_code, school, school_code, group, n_students, n_suppressed)
dc_race <- dc_race %>% 
  select(school, school_code, group, n_students) %>% 
  pivot_wider(names_from = group, values_from = n_students) %>%
  mutate(sum_groups = rowSums(cbind(`American Indian/Alaskan Native`, # sum of counts from all reported race
                               Asian, `Black/African-American`, 
                               `Hispanic/Latino of any race`,
                               `Two or more races`,
                               White), na.rm=TRUE))
names(dc_race)[3:10] <- c("all","amerind","asian","black","hispanic","nhpi","multiple","white")
dc_race <- dc_race %>% 
  select(school_code, school, all, sum_groups,amerind,asian,black,hispanic,nhpi,multiple,white) %>% 
  mutate(diff = all - sum_groups) # how many students' data were suppressed in race counts?

# ---- estimating from suppressed data -----
dc_all <- read_excel("2019 DC School Report Card Aggregate Public Data_.xlsx", sheet = "Enrollment") # 2018-2019 data for ward info and percentages
names(dc_all)<- tolower(names(dc_all))
p_all <- dc_all %>% 
  filter(`school code`=="All",
         `student group` == "Asian"|`student group`=="Black/African-American"| # filter to only look at race, 
           `student group` == "Hispanic/Latino of any race"| # separate from EL and SWD
           `student group` == "Native Hawaiian/Other Pacific Islander"|
           `student group` == "American Indian/Alaskan Native"|
           `student group` == "White"| `student group` == "Two or more races"|
           `student group` =="All Report Card Students",
         grade == "All",
         `entity type`=="All") %>% 
  select(`lea name`,`student group`,`percent enrolled`,`total count of students`) %>% 
  pivot_wider(names_from = `student group`,values_from = `percent enrolled`)
names(p_all)[2:9] <- c("all","amerind","asian","black","hispanic","nhpi","multiple","white")

dc_race <- dc_race %>% 
  mutate(amerind = 0,
         nhpi = 0) # NA estimations based on DC-wide percentages
dc_race$asian[is.na(dc_race$asian)] <- dc_race$diff[is.na(dc_race$asian)] * .01 * as.numeric(p_all$asian)
dc_race$hispanic[is.na(dc_race$hispanic)] <- dc_race$diff[is.na(dc_race$hispanic)] * .01 * as.numeric(p_all$hispanic)
dc_race$multiple[is.na(dc_race$multiple)] <- dc_race$diff[is.na(dc_race$multiple)] * .01 * as.numeric(p_all$multiple)
dc_race$white[is.na(dc_race$white)] <- dc_race$diff[is.na(dc_race$white)] * .01 * as.numeric(p_all$white)

dc_race <- dc_race %>% 
  gather(group, n, all:white) %>% #change from wide to long
  filter(group != "all",
         group != "sum_groups") %>% 
  select(school_code, school, group,n)



dc_locseg = mutual_local(dc_race, "group","school", weight = "n", wide = TRUE)
summary(dc_locseg$ls) # How much does each school represent all of DC?
names(dc_locseg) <- c("school","ls_dc","p_dc")

write.csv(dc_race, file = file.path('dc_race.csv'),row.names = FALSE)
# ---- ward-level demographics ----
dc_wards <- read_excel("2019 DC School Report Card Aggregate Public Data_.xlsx", # 2018-2019 school year data 
                       sheet = "Enrollment") # with ward designations
names(dc_wards) <- tolower(names(dc_wards))
dc_wards <- dc_wards %>% 
  select(`school code`,ward) %>% 
  distinct() %>% 
  mutate(school_code = `school code`)
dc_wards <- merge(dc_race,dc_wards, by="school_code")
str(dc_wards)

write.csv(dc_wards, file = file.path('dc_wards.csv'),row.names = FALSE)
dc_wards$ward <- as.factor(dc_wards$ward)
library(plyr)

a <- ddply(dc_wards, .(ward, group), summarize, # summarize by ward and subgroup
           n = sum(n)) # column for total students in each group

wards_locseg <- mutual_local(a, "group","ward", weight = "n", wide = TRUE) # How representative of DC is each ward?
write.csv(wards_locseg,file = file.path('wards_locseg.csv'),row.names = FALSE)

sch_wards_locseg <- dc_wards %>% 
  filter(ward == "1",
         group != "all")
sch_wards_locseg <- sch_wards_locseg %>% 
  mutual_local("group","school", weight = "n", wide = TRUE)
names(sch_wards_locseg)[2:3] <- c("ls_ward","p_ward")
sch_wards_locseg$ward = "1"

b <- dc_wards %>% 
  filter(ward == "2",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "2") %>% 
  select(school, ls_ward, p_ward, ward)

c <- dc_wards %>% 
  filter(ward == "3",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "3") %>% 
  select(school, ls_ward, p_ward, ward)

d <- dc_wards %>% 
  filter(ward == "4",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "4") %>% 
  select(school, ls_ward, p_ward, ward)

e <- dc_wards %>% 
  filter(ward == "5",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "5") %>% 
  select(school, ls_ward, p_ward, ward)

f <- dc_wards %>% 
  filter(ward == "6",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "6") %>% 
  select(school, ls_ward, p_ward, ward)

g <- dc_wards %>% 
  filter(ward == "7",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "7") %>% 
  select(school, ls_ward, p_ward, ward)

h <- dc_wards %>% 
  filter(ward == "8",
         group != "all") %>% 
  mutual_local("group","school",weight = "n",wide = TRUE) %>% 
  mutate(ls_ward = ls,
         p_ward = p,
         ward = "8") %>% 
  select(school, ls_ward, p_ward, ward)

sch_wards_locseg <- rbind(sch_wards_locseg,b,c,d,e,f,g,h) # ward level local seg measures for each school
rm(a,b,c,d,e,f,g,h)

dc_all_locseg <- merge(sch_wards_locseg,dc_locseg,by="school")

# member as a Y/N column w/ racial demographics------
dc_memb_locseg <- dc_all_locseg %>% 
  filter(school == "District of Columbia International School"|
           str_detect(school,"Capital City PCS")|
           str_detect(school,"E.L. Haynes PCS")|
           str_detect(school,"Stokes")|
           school == "Inspired Teaching Demonstration PCS"|
           school == "Lee Montessori PCS - Brookland"|
           str_detect(school,"Two Rivers PCS")|
           str_detect(school, "Washington Latin PCS")|
           school == "Washington Yu Ying PCS") %>% 
  mutate(member = "yes") %>% 
  arrange(school)
`%out%` <- function(a,b) ! a %in% b
dc_notmembers <- dc_all_locseg %>% 
  subset(school %out% dc_memb_locseg$school) %>% 
  mutate(member = "no")
dc_all_locseg <- rbind(dc_memb_locseg,dc_notmembers)

write.csv(dc_all_locseg,file = file.path('dc_all_locseg.csv'),row.names = FALSE)
write.csv(dc_memb_locseg, file = file.path('dc_memb_locseg.csv'),row.names = FALSE)

# Ward-level racial segregation ----
dc_race1 <- dc_race %>% 
  filter(group != "all")
dc_wards1 <- dc_wards %>% 
  filter(group != "all")
mutual_total(dc_race1, "group","school", weight = "n") # M = 0.39, H = 0.40
mutual_total(dc_wards1, "group","ward", weight = "n") # M = 0.24, H = 0.24
mutual_total(dc_wards1, "group","school", within = "ward",weight = "n") # M = 0.16, H = 0.16

example_data <- data.frame(dist = c("District 1","District 2","District 3"),
                           black = c(1000,0,0),
                           white = c(0,1000,750),
                           asian = c(0,0,250))
example_data <- example_data %>% 
  gather(race, n, black:asian, factor_key = TRUE) 
mutual_total(example_data, "race","dist",weight = "n") # M = 0.70, H = 0.79

example2 <- data.frame(dist = c("District 1","District 2","District 3"),
                       black = c(750,250,0),
                       white = c(250,750,750),
                       asian = c(0,0,250))
example2 <- example2 %>% 
  gather(race, n, black:asian, factor_key = TRUE)
mutual_total(example2, "race","dist",weight = "n") # M = 0.33, H = 0.37
mutual_local(example2,"race","dist",weight = "n", wide = TRUE)

example3 <- data.frame(dist = c("District 1","District 2","District 3"),
                       black = c(750,750,840),
                       white = c(150,150,60),
                       asian = c(50,50,50))
example3 <- example3 %>% 
  gather(race, n, black:asian, factor_key = TRUE)
mutual_total(example3,"race","dist",weight = "n")

# ---- racial segregation scatterplots ----
q <- ggplot(dc_all_locseg,aes(ls_dc,ls_ward))+
  geom_point(alpha = 0.7, aes(color = factor(member)),position = "jitter")+
  scale_color_manual(values = c("#1aacb8","#ee5d3a"),
                     name = "",
                     labels = c("Non-member","DCSC Member"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA),
        panel.border = element_rect(color = "black",fill = NA))+
  labs(title = "Representativeness of all DC public and charter schools",
       x = "DC-level segregation", y = "ward-level segregation")+
  stat_smooth(aes(linetype = "Regression"), method = "lm", 
             formula = y ~ x, se = FALSE, colour = 1, size = 0.1)+
  labs(linetype = "")
q
ggsave("dc_scatter.png",q,width = 10, height = 5)
dc_memb_locseg$abbrev <- c("Capital PCS HS",
                           "Capital PCS LS",
                           "Capital PCS MS",
                           "DC Int'l","EL Haynes Elem",
                           "EL Haynes HS","EL Haynes MS",
                           "EW Stokes Brookland",
                           "EW Stokes East End",
                           "ITDS","Lee Montessori","Two Rivers 4th St",
                           "Two Rivers Young","Latin Middle","Latin Upper",
                           "Yu Ying")
members <- ggplot(dc_memb_locseg, aes(ls_dc,ls_ward))+
  geom_point(alpha=0.7,position = "jitter")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        legend.box.background = element_rect(color="black",fill=NA))+
  labs(title = "Representativeness of DCSC members in DC",
       x = "DC-level segregation", y = "ward-level segregation")+
  geom_text(data = dc_memb_locseg, 
            mapping = aes(label=abbrev,x=ls_dc,y=ls_ward),
            vjust =1.5,hjust=0.5,angle=45,
            size=4,check_overlap = FALSE)

members
ggsave("memberscatter.png",members,width=15,height=10)
#---- wards as bar graphs ---
levels(as.factor(dc_memb_locseg$ward))
wards_locseg$member <- c("yes","no","no","yes","yes",
                           "yes","yes","no")
p <- ggplot(wards_locseg,aes(ward,ls))+
  geom_bar(stat = "identity",
           aes(fill = member))+
  theme_minimal()+
  labs(title = "Segregation between DC wards",
       x = "Ward of DC", y = "Divergence from representativeness of DC proper")+
  theme(plot.title = element_text(hjust=0.5, face = "bold",size = 20),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA))+
  scale_fill_manual(values = c("#dddddd","#1a428a"),
                    name = "DCSC?",
                    label=c("no members","includes members"))
p
ggsave("wards.png",p,width = 10, height = 5)

# ---- At-Risk, EL, SWD -----
dc_subgroups <- dc_enrl %>% 
  mutate(n_suppressed = as.character(n_students),
         n_students = as.numeric(n_students)) %>% 
  filter(metric == "In-Seat Attendance",
         group == "At-Risk"| # filter to only risk, EL, SWD
           group == "English Learners"|
           group == "Students with Disabilities"|
           group =="All Report Card Students") %>% 
  select(lea, lea_code, school, school_code, group, n_students, n_suppressed)
dc_subgroups <- dc_subgroups %>% 
  select(school, school_code, group, n_students) %>% 
  pivot_wider(names_from = group, values_from = n_students)
names(dc_subgroups)[3:6] <- c("all","risk","el","swd")
dc_wards <- read_excel("2019 DC School Report Card Aggregate Public Data_.xlsx", 
                       sheet = "Enrollment")
names(dc_wards) <- tolower(names(dc_wards))
dc_wards <- dc_wards %>% 
  select(`school code`,ward) %>% 
  distinct() %>% 
  mutate(school_code = `school code`)
dc_subgroups <- merge(dc_subgroups,dc_wards,by="school_code") # matched DC subgroup rows with wards

dc_wards <- read_excel("2019 DC School Report Card Aggregate Public Data_.xlsx", 
                       sheet = "Enrollment")
names(dc_wards) <- tolower(names(dc_wards))
p_subgroups <- dc_wards %>% 
  filter(`entity type`=="All",
         `student group` == "At-Risk"| 
           `student group` == "English Learners"|
           `student group` == "Students with Disabilities") # DC percentages for all 

dc_subgroups$risk[is.na(dc_subgroups$risk)] <- dc_subgroups$all[is.na(dc_subgroups$risk)] * .01 *
  as.numeric(p_subgroups$`percent enrolled`[1]) # row 1 is at-risk
dc_subgroups$el[is.na(dc_subgroups$el)] <- dc_subgroups$all[is.na(dc_subgroups$el)] * .01 *
  as.numeric(p_subgroups$`percent enrolled`[2]) # row 2 is english learners
dc_subgroups$swd[is.na(dc_subgroups$swd)] <- dc_subgroups$all[is.na(dc_subgroups$swd)] * .01 *
  as.numeric(p_subgroups$`percent enrolled`[3]) # row 3 is students with disabilities
names(dc_subgroups)
dc_subgroups <- dc_subgroups %>% 
  select(school, ward, all, risk, el, swd) %>% 
  mutate(p_risk = risk/all,
         p_el = el/all,
         p_swd = swd/all)



# ward-level data for each subgroup ----
ward1 <- dc_subgroups %>% 
  filter(ward == "1") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward2 <- dc_subgroups %>% 
  filter(ward == "2") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward3 <- dc_subgroups %>% 
  filter(ward == "3") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward4 <- dc_subgroups %>% 
  filter(ward == "4") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward5 <- dc_subgroups %>% 
  filter(ward == "5") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward6 <- dc_subgroups %>% 
  filter(ward == "6") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward7 <- dc_subgroups %>% 
  filter(ward == "7") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
ward8 <- dc_subgroups %>% 
  filter(ward == "8") %>% 
  mutate(ward_risk = mean(p_risk),
         ward_sdrisk = sd(p_risk))
dc_subgroups <- rbind(ward1,ward2,ward3,ward4,ward5,ward6,ward7,ward8)
dc_subgroups <- dc_subgroups %>% 
  mutate(dc_risk = .47,
         dc_el = .11,
         dc_swd = .17)

ward1 <- dc_subgroups %>% 
  filter(ward == "1") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward2 <- dc_subgroups %>% 
  filter(ward == "2") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward3 <- dc_subgroups %>% 
  filter(ward == "3") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward4 <- dc_subgroups %>% 
  filter(ward == "4") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward5 <- dc_subgroups %>% 
  filter(ward == "5") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward6 <- dc_subgroups %>% 
  filter(ward == "6") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward7 <- dc_subgroups %>% 
  filter(ward == "7") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
ward8 <- dc_subgroups %>% 
  filter(ward == "8") %>% 
  mutate(ward_el = mean(p_el),
         ward_sdel = sd(p_el))
dc_subgroups <- rbind(ward1,ward2,ward3,ward4,ward5,ward6,ward7,ward8)

ward1 <- dc_subgroups %>% 
  filter(ward == "1") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward2 <- dc_subgroups %>% 
  filter(ward == "2") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward3 <- dc_subgroups %>% 
  filter(ward == "3") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward4 <- dc_subgroups %>% 
  filter(ward == "4") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward5 <- dc_subgroups %>% 
  filter(ward == "5") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward6 <- dc_subgroups %>% 
  filter(ward == "6") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward7 <- dc_subgroups %>% 
  filter(ward == "7") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
ward8 <- dc_subgroups %>% 
  filter(ward == "8") %>% 
  mutate(ward_swd = mean(p_swd),
         ward_sdswd = sd(p_swd))
dc_subgroups <- rbind(ward1,ward2,ward3,ward4,ward5,ward6,ward7,ward8)


write.csv(dc_subgroups,file = file.path("wards_subgroup.csv"),row.names = FALSE)

# members only: at-risk, EL, SWD -----
memb_subgroups <- dc_subgroups %>% 
  filter(school == "District of Columbia International School"|
           str_detect(school,"Capital City PCS")|
           str_detect(school,"E.L. Haynes PCS")|
           str_detect(school,"Stokes")|
           school == "Inspired Teaching Demonstration PCS"|
           school == "Lee Montessori PCS - Brookland"|
           str_detect(school,"Two Rivers PCS")|
           str_detect(school, "Washington Latin PCS")|
           school == "Washington Yu Ying PCS") %>% 
  mutate(member = "yes") %>% 
  arrange(school)
`%out%` <- function(a,b) ! a %in% b
notmembers_sub <- dc_subgroups %>% 
  subset(school %out% memb_subgroups$school) %>% 
  mutate(member = "no")
dc_subgroups <- rbind(memb_subgroups,notmembers_sub)
write.csv(dc_subgroups, file = file.path('dc_subgroups.csv'),row.names = FALSE)

# ---- subgroup segregation scatterplots -----
# at-risk
df <- data.frame(x1 = 0, x2 = .75, y1 = 0, y2=.75)
risk_plot <- ggplot(dc_subgroups,aes(ward_risk,p_risk))+
  geom_hline(yintercept = 0.47, linetype = "dashed",color = "#cccccc",size = 1)+
  geom_vline(xintercept = 0.47, linetype = "dashed",color = "#5069B1",size=1)+
  geom_point(alpha = 0.7, aes(color = factor(member)),position = "jitter")+
  scale_color_manual(values = c("#1aacb8","#bbbbbb","#ee5d3a"),
                     name = "",
                     labels = c("Non-member","Perfect representation","DCSC Member"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA),
        panel.border = element_rect(color = "black",fill = NA))+
  labs(title = "Representativeness of At-Risk Students in DC schools",
       x = "Ward-level proportion of at-risk students", 
       y = "School-level proportion of at-risk students")+ # Comparison between school and ward
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"),
               data = df)+
  labs(linetype = "")
risk_plot

ggsave("risk_plot.png",risk_plot,width = 10, height = 5)

dc_subgroups$abbrev <- NA
names(dc_subgroups)
dc_subgroups[1:16,20] <-c("Capital PCS HS","Capital PCS LS","Capital PCS MS",
                        "DC Int'l","EL Haynes Elem","EL Haynes HS","EL Haynes MS",
                        "EW Stokes Brookland","EW Stokes East End",
                        "ITDS","Lee Montessori","Two Rivers 4th St",
                        "Two Rivers Young","Latin Middle","Latin Upper","Yu Ying")

df <- data.frame(x1 = 0.3, x2 = .7, y1 = 0.3, y2=.7)
members <- ggplot(dc_subgroups[1:16,], aes(ward_risk,p_risk))+
  geom_hline(yintercept = 0.47, linetype = "dashed",color = "#cccccc",size = 1)+
  geom_vline(xintercept = 0.47, linetype = "dashed",color = "#5069B1",size=1)+
  geom_point(alpha=0.7,position = "jitter")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        legend.box.background = element_rect(color="black",fill=NA))+
  labs(title = "Representativeness of At-Risk Students in DCSC members",
       x = "Ward-level proportion of at-risk students", 
       y = "School-level proportion of at-risk students")+
  geom_text(data = dc_subgroups, 
            mapping = aes(label=abbrev,x=ward_risk,y=p_risk),
            vjust =1.5,hjust=0,angle=45,
            size=3,check_overlap = FALSE)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               data = df)+
  scale_x_continuous(limits=c(.29,.71))+
  scale_y_continuous(limits=c(0,.71))
members
ggsave("memb_riskplot.png",members,width=15,height=10)

# EL
df <- data.frame(x1 = 0, x2 = .5, y1 = 0, y2=.5)
el_plot <- ggplot(dc_subgroups,aes(ward_el,p_el))+
  geom_hline(yintercept = 0.11, linetype = "dashed",color = "#cccccc",size = 1)+
  geom_vline(xintercept = 0.11, linetype = "dashed",color = "#5069B1",size=1)+
  geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2, color = "segment"),data = df)+
  geom_point(alpha = 0.7, aes(color = factor(member)),position = "jitter")+
  scale_color_manual(values = c("#1aacb8","#bbbbbb","#ee5d3a"),
                     name = "",
                     labels = c("Non-member","Perfect representation","DCSC Member"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA),
        panel.border = element_rect(color = "black",fill = NA))+
  labs(title = "Representativeness of English Learners in DC schools",
       x = "Ward-level proportion of English learners", 
       y = "School-level proportion of English learners")+ # Comparison between school and ward
  labs(linetype = "")+
  scale_x_continuous(limits=c(0,0.5))+
  scale_y_continuous(limits=c(0,0.8))
el_plot

ggsave("el_plot.png",el_plot,width = 10, height = 5)

df <- data.frame(x1 = 0.05, x2 = .5, y1 = 0.05, y2=.5)
members <- ggplot(dc_subgroups[1:16,], aes(ward_el,p_el))+ # EL Members only
  geom_hline(yintercept = 0.11, linetype = "dashed",color = "#cccccc",size = 1)+
  geom_vline(xintercept = 0.11, linetype = "dashed",color = "#5069B1",size=1)+
  geom_point(alpha=0.7,position = "jitter")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        legend.box.background = element_rect(color="black",fill=NA))+
  labs(title = "Representativeness of English Learners in DCSC members",
       x = "Ward-level proportion of English learners", 
       y = "School-level proportion of English learners")+
  geom_text(data = dc_subgroups, 
            mapping = aes(label=abbrev,x=ward_el,y=p_el),
            vjust =-1,hjust=0,angle=45,
            size=2,check_overlap = FALSE)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               data = df)+
  scale_x_continuous(limits=c(0,0.55))+
  scale_y_continuous(limits=c(0.05,0.55))
members
ggsave("memb_elplot.png",members,width=15,height=10)

# SWD
df <- data.frame(x1 = 0.05, x2 = .25, y1 = 0.05, y2=.25)
swd_plot <- ggplot(dc_subgroups,aes(ward_swd,p_swd))+
  geom_hline(yintercept = 0.17, linetype = "dashed",color = "#cccccc",size = 1)+
  geom_vline(xintercept = 0.17, linetype = "dashed",color = "#5069B1",size=1)+
  geom_segment(aes(x=x1,y=y1,xend=x2,yend=y2, color = "segment"),data = df)+
  geom_point(alpha = 0.7, aes(color = factor(member)),position = "jitter")+
  scale_color_manual(values = c("#1aacb8","#bbbbbb","#ee5d3a"),
                     name = "",
                     labels = c("Non-member","Perfect representation","DCSC Member"))+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA),
        panel.border = element_rect(color = "black",fill = NA))+
  labs(title = "Representativeness of Students with Disabilities in DC schools",
       x = "Ward-level proportion of SWDs", 
       y = "School-level proportion of SWDs") # Comparison between school and ward
swd_plot

ggsave("swd_plot.png",swd_plot,width = 10, height = 5)

df <- data.frame(x1 = 0.1, x2 = .25, y1 = 0.1, y2=.25)
members <- ggplot(dc_subgroups[1:16,], aes(ward_swd,p_swd))+ # SWD Members only
  geom_hline(yintercept = 0.17, linetype = "dashed",color = "#cccccc",size = 1)+
  geom_vline(xintercept = 0.17, linetype = "dashed",color = "#5069B1",size=1)+
  geom_point(alpha=0.7,position = "jitter")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top",
        legend.box.background = element_rect(color="black",fill=NA))+
  labs(title = "Representativeness of SWDs in DCSC members",
       x = "Ward-level proportion of SWDs", 
       y = "School-level proportion of SWDs")+
  geom_text(data = dc_subgroups, 
            mapping = aes(label=abbrev,x=ward_swd,y=p_swd),
            vjust =-1,hjust=0,angle=45,
            size=2,check_overlap = FALSE)+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               data = df)+
  scale_x_continuous(limits=c(0.05,0.3))+
  scale_y_continuous(limits=c(0.1,0.25))
members
ggsave("memb_swdplot.png",members,width=15,height=10)


#---- wards as bar graphs of representativeness in subgroups ----
ward_risk <- dc_subgroups %>% 
  select(ward,ward_risk) %>% 
  distinct() %>% 
  arrange(ward)
ward_risk$member <- c("yes","no","no","yes","yes", #contains DCSC members?
                         "yes","yes","no")
p <- ggplot(ward_risk,aes(ward,ward_risk))+
  geom_bar(stat = "identity",
           aes(fill = member))+
  theme_minimal()+
  labs(title = "Proportion of At-Risk Students in each of DC wards",
       x = "Ward of DC", y = "Proportion of at-risk students")+
  theme(plot.title = element_text(hjust=0.5, face = "bold",size = 15),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA))+
  scale_fill_manual(values = c("#dddddd","#1a428a"),
                    name = "DCSC?",
                    label=c("no members","includes members"))+
  geom_hline(yintercept = 0.47, linetype = "dashed",color = "#1aacb8",size = 1)+
  geom_label(label="0.47 = Proportion at-risk in all of DC",
             x = 1.5,y=0.48,hjust=0,vjust=0,
             label.padding=unit(0.55,"lines"),
             label.size = .25,
             color = "#ffffff",
             fill = "#1aacb8")
p
ggsave("risk_bars.png",p,width = 10, height = 5)

ward_el <- dc_subgroups %>% 
  select(ward,ward_el) %>% 
  distinct() %>% 
  arrange(ward)
ward_el$member <- c("yes","no","no","yes","yes", #contains DCSC members?
                      "yes","yes","no")
p <- ggplot(ward_el,aes(ward,ward_el))+
  geom_bar(stat = "identity",
           aes(fill = member))+
  theme_minimal()+
  labs(title = "Proportion of English Learners in each of DC wards",
       x = "Ward of DC", y = "Proportion of English learners")+
  theme(plot.title = element_text(hjust=0.5, face = "bold",size = 15),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA))+
  scale_fill_manual(values = c("#dddddd","#1a428a"),
                    name = "DCSC?",
                    label=c("no members","includes members"))+
  geom_hline(yintercept = 0.11, linetype = "dashed",color = "#1aacb8",size = 1)+
  geom_label(label="0.11 = Proportion English learners in all of DC",
             x = 1.5,y=0.05,hjust=0,vjust=0,
             label.padding=unit(0.55,"lines"),
             label.size = .25,
             color = "#ffffff",
             fill = "#1aacb8")
p
ggsave("el_bars.png",p,width = 10, height = 5)

ward_swd <- dc_subgroups %>% 
  select(ward,ward_swd) %>% 
  distinct() %>% 
  arrange(ward)
ward_swd$member <- c("yes","no","no","yes","yes", #contains DCSC members?
                    "yes","yes","no")
p <- ggplot(ward_swd,aes(ward,ward_swd))+
  geom_bar(stat = "identity",
           aes(fill = member))+
  theme_minimal()+
  labs(title = "Proportion of Students with Disabilities in each of DC wards",
       x = "Ward of DC", y = "Proportion of Students with Disabilities")+
  theme(plot.title = element_text(hjust=0.5, face = "bold",size = 15),
        legend.position = "top",
        legend.box.background = element_rect(color = "black",fill=NA))+
  scale_fill_manual(values = c("#dddddd","#1a428a"),
                    name = "DCSC?",
                    label=c("no members","includes members"))+
  geom_hline(yintercept = 0.17, linetype = "dashed",color = "#1aacb8",size = 1)+
  geom_label(label="0.17 = Proportion of SWDs\nin all of DC",
             x = 5,y=0.15,hjust=0,vjust=.5,
             label.padding=unit(0.55,"lines"),
             label.size = .25,
             color = "#ffffff",
             fill = "#1aacb8")
p
ggsave("swd_bars.png",p,width = 10, height = 5)

