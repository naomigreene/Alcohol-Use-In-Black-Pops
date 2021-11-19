
### ARCHIVED: September 9, 2021

### Postdoc Project: Alcohol Use among Black Populations in the US
### Prgram: AUBP_Program02.R
### Author: Naomi Greene
### Creation Date: May 1, 2021
### Updated: July 7, 2021
### Updated: July 28, 2021
### Updated: September 7, 2021
### Updated: September 10, 2021 


### This program reads in the data that was cleanned in R. Then is creates and saves tables that will be used to make the data visualizations for this project. This script will also provide the code that will be transferred into a RMarkdown document, which will be the final report. 

### NOTE: The update of July 7, 2021 came after having a meeting with Lori and Renee about the results presented in the manuscrpt "GreeneBlackAlcoholUse_2021_05_25". We met on June 29, 2021. In that meeting we decided to remove the regional analyes from the paper because there were no statistically nor meaningful differences in the alcohol use behaviors across regions by population group. Additiionally, we decided to present the state prevalence estimates in the maps on the same scale to highlight the stark differences between Black women and Black men. Finally, we discussed the need to examine age as a potential confounder when comparing prevalence estimates across states. I decided to test this using regression rather than attempting to do direct standardization to a standard population like the US 2000 population. This methods seemed simpler. However, I think it would be helpful to use linear regression so that the estimates will be prevealnce differences (absolute measure) rather than odds ratios (relative measure). No code has been altered for these new analyses. Rather I have annotated when new code has been added. 

### NOTE: It has been a few weeks since my conversation with Renee and Lori about examining age. It looks like for the note above, I considered using linear regression (with binary outcome) to examine whether age is an important factor in explaining differences in the prevalence of alcohol use behvaiors across states. Upon futher consideration, I've decided not use regression methods because I don't think we need them to tell whehter the age distribution differs across states. Instead, I will use simpler hypothesis testing (t-tests, ANOVA) because I want to describe the distribution and conduct a hypothesis test. I want to see 95% CI around the estimates because p-values can be misleading when the sample is large. In other words, something can be statistically signifcant, but not be meaningful. 

### IMPORTANT NOTE ABOUT AGE ANALYSIS: The age variables were not originally included in the subset of BRFSS data. I had to go back into Stata and include these variables and output a new dataset. All other variables are the same as previous datasets. Only the age varibles have been added. 

### NOTE: This code was updated on September 7, 2021. The research team (Renee Johnson, Lori T. Dean, and Kechna Cadet) met on Friday, September 3 to dicuss the August 19th version of the manuscript which included updated figures and tables with age-adjusted prevalence estimates. Renee suggessted a few changes to the figures to make them more readable: 1. Use the census two leter terms for the states in all figures; 2. Remove information about West Virginia for Black women especially since we are not including this information for Black men and therefore cannot calculate a male excess measure; 3. Include a note in the maps that the prevalence has been broken into quartiles. 

# NOTE: On September 8, 2021, I decided to create a new program to output the figures and tables for this analysis as this program had become to long an unweildy. I also had to completely change the package used to create the map figures. So rather than comment out large sections of code, I thought it would be better to create a new program and archive this program. 

# NOTE: On September 10, 2021, I needed to make a change to the suppression rules. We changed the rule to be <60 cases so that we could supress data for West Virginia for both Black women and men. 


# A. Import and Set-Up Survey Data ----------------------------------------

library(haven)

brfss2019_clean <- read_dta("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/brfss2019_clean.dta")

View(brfss2019_clean)

library(tidyverse)

glimpse(brfss2019_clean)

# Rename variables
brfss <- brfss2019_clean %>%
     rename(psu = `_psu`) %>%
     rename(ststr = `_ststr`) %>%
     rename(strwt = `_strwt`) %>%
     rename(imprace = `_imprace`)%>%
     rename(llcpwt2 = `_llcpwt2`) %>%
     rename(llcpwt = `_llcpwt`) %>%
     rename(race = `_race`) %>%
     rename(age80 = `_age80`) %>%
     rename(age_g = `_age_g`) %>%
     rename(rfbing5 = `_rfbing5`)%>%
     rename(drnkwek1 = `_drnkwk1`) %>%
     rename(rfdrhv7 = `_rfdrhv7`)

# Make categorical vars into factor variables

brfss$state <- factor(brfss$state,
                      levels=c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56),
     labels=c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri","Montana", "Nebraska", "Nevada", "New Hampshire", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"))

brfss$region <- factor(brfss$region, 
                      levels=c(1,2,3,4),
                      labels=c("Northeast","Midwest",
                               "South", "West"))

brfss$sexvar <- factor(brfss$sexvar,
                       levels=c(1,2),
                       labels=c("Male", "Female"))


brfss$female <- factor(brfss$female,
                       levels=c(0,1),
                       labels=c("Male", "Female"))

brfss$male <- factor(brfss$male,
                     levels=c(0,1),
                     labels=c("Female", "Male"))

brfss$black <- factor(brfss$black,
                      levels=c(0,1),
                      labels=c("White", "Black"))

# brfss$avedrnkscat <- factor(brfss$avedrnkscat, 
#                             levels=c(1,2,3,4),
#                             labels=c("1drink", "2drinks", "3drinks",
#                                      "4 or more drinks"))

# brfss$bingeocc_cat <- factor(brfss$bingeocc_cat,
#                              levels=c(0,1,2,3),
#                              labels=c("0times", "1-2times",
#                                       "3-4times","5 or more times"))

brfss$age4cat <- factor(brfss$age4cat,
                        levels=c(1,2,3,4),
                        labels=c("18-24", "25-44", 
                                 "45-64", "65+"))

# Create binary indicator variables from binary (1/2 coded) variables
brfss <- brfss %>%
     mutate(temp1 = ifelse(drnkany5 == 1, 1, 0),
            temp2 = ifelse(rfbing5 == 1, 1,0),
            temp3 = ifelse(rfdrhv7 == 1, 1, 0),
            temp4 = ifelse(binge_current == 1,1,0),
            temp5 = ifelse(hvy_current == 1,1,0)) %>%
     select(-drnkany5, -rfbing5, -rfdrhv7, -binge_current, 
            -hvy_current) %>%
     rename(drnkany5 = temp1,
            rfbing5 = temp2,
            rfdrhv7 = temp3,
            binge_current = temp4,
            hvy_current = temp5)

# Save nonweighted dataset
save(brfss, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/brfss.RData")


library(survey)

# Set options for allowing a single observation per stratum
options(survey.lonely.psu = "adjust") 

# Create survey design
brfssdsgn <- svydesign(
     id=~1,
     strata = ~ststr,
     weights = ~llcpwt,
     data = brfss)

library(srvyr)

# Create survey dataset that can be used with tidyverse commands and pipes
brfssdesign <- as_survey_design(brfssdsgn)


# Save weighted data
save(brfssdesign, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/brfssdesign.RData")

rm(list=ls())

# B. State Analyses -------------------------------------------------------

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

load("brfss.RData")
load("brfssdesign.RData")

# Past 30-day drinking (drnkany5)
e1.s <- brfssdesign %>%
     group_by(state, black, sexvar) %>%
     summarize(drnkany = round(survey_mean(drnkany5, na.rm=T,
                                           vartype = "ci")*100,1),
               sample_size = unweighted(n())) %>%
        filter(black == "Black")

# Past 30-day binge drinking among current drinkers (binge_current)
e2.s <- brfssdesign %>%
     group_by(state, black, sexvar) %>%
     summarize(binge = round(survey_mean(binge_current, na.rm=T, 
                                     vartype = "ci")*100,1),
               sample_size = unweighted(n())) %>%
     filter(black == "Black")

# Past 30-day heavy drinking among current drinkers (hvy_current)
e3.s <- brfssdesign %>%
     group_by(state, black, sexvar) %>%
     summarize(heavy = round(survey_mean(hvy_current, na.rm=T, 
                                   vartype = "ci")*100,1),
               sample_size = unweighted(n())) %>%
     filter(black == "Black")

# Average drinks per day in past 30-days (avedrnks)

# The following codes throws an error because there is only 1 observation in Idaho for Black females 

# e4.s <- brfssdesign %>%
#         group_by(state, black, sexvar) %>%
#         summarize(median_avedrnks = survey_median(avedrnks,
#                                                   quantiles = c(0.5),
#                                                   na.rm=T),
#                   iqr = survey_quantile(avedrnks,
#                                         quantiles = c(0.25,0.75),
#                                         na.rm=T)) %>%
#         filter(black == "Black") %>%
#         select(-median_avedrnks_se,-iqr_q25_se,-iqr_q75_se)

# Binge drinking occasions in past 30-days (bing_occ)



### Unweighted counts
n1.s <- brfss %>%
        filter(!is.na(drnkany5)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n2.s <- brfss %>%
        filter(!is.na(binge_current)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n3.s <- brfss %>%
        filter(!is.na(hvy_current)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")


### Merge prevalence estimates and unweighted counts
table01.s <- merge(n1.s, e1.s, by=c("state", "black", "sexvar"))
table02.s <- merge(n2.s, e2.s, by=c("state", "black", "sexvar"))
table03.s <- merge(n3.s, e3.s, by=c("state", "black", "sexvar"))

### Suppress prevalence estimates for populations with fewer than 50 cases
table01.s$n[table01.s$sample_size < 50] <- NA
table01.s$drnkany[table01.s$sample_size < 50] <- NA
table01.s$drnkany_low[table01.s$sample_size < 50] <- NA
table01.s$drnkany_upp[table01.s$sample_size < 50] <- NA

table02.s$n[table02.s$sample_size < 50] <- NA
table02.s$binge[table02.s$sample_size < 50] <- NA
table02.s$binge_low[table02.s$sample_size < 50] <- NA
table02.s$binge_upp[table02.s$sample_size < 50] <- NA

table03.s$n[table03.s$sample_size < 50] <- NA
table03.s$heavy[table03.s$sample_size < 50] <- NA
table03.s$heavy_low[table03.s$sample_size < 50] <- NA
table03.s$heavy_upp[table03.s$sample_size < 50] <- NA

# Order datasets by state
table01.s <- table01.s[order(table01.s$state),]
table02.s <- table02.s[order(table02.s$state),]
table03.s <- table03.s[order(table03.s$state),]



### Save the created tables as datasets for data visualizations 
save(table01.s, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table01_s.RData")

save(table02.s, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table02_s.RData")

save(table03.s, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table03_s.RData")

rm(list=ls())


# C. Region Analyses ------------------------------------------------------

library(survey)
library(srvyr) 

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# unweighted data 
load("brfss.RData")

# weighted data
load("brfssdesign.RData")

### Prevalence estimates

# Past 30-day drinking (drnkany5)
e1.r <- brfssdesign %>%
        group_by(region, black, sexvar) %>%
        summarize(drnkany = round(survey_mean(drnkany5, na.rm=T,
                                        vartype = "ci")*100,1)) %>%
        filter(black == "Black")

# Past 30-day binge drinking among current drinkers (binge_current)
e2.r <- brfssdesign %>%
        group_by(region, black, sexvar) %>%
        summarize(binge = round(survey_mean(binge_current, na.rm=T, 
                                      vartype = "ci")*100,1)) %>%
        filter(black == "Black")

# Past 30-day heavy drinking among current drinkers (hvy_current)
e3.r <- brfssdesign %>%
        group_by(region, black, sexvar) %>%
        summarize(heavy = round(survey_mean(hvy_current, na.rm=T, 
                                      vartype = "ci")*100,1)) %>%
        filter(black == "Black")

# Average drinks per day in past 30-days (avedrnks)
e4.r <- brfssdesign %>%
        group_by(region, black, sexvar) %>%
        summarize(median_avedrnks = survey_median(avedrnks,
                                                quantiles = c(0.5),
                                                na.rm=T),
                  iqr = survey_quantile(avedrnks,
                                        quantiles = c(0.25,0.75),
                                        na.rm=T)) %>%
        filter(black == "Black") %>%
        select(-median_avedrnks_se,-iqr_q25_se,-iqr_q75_se)

# Binge drinking occasions in past 30-days (bing_occ)
e5.r <- brfssdesign %>%
        group_by(region, black, sexvar) %>%
        summarize(median_bing_occ = survey_median(bing_occ,
                                                  quantiles = c(0.5),
                                                  na.rm=T),
                  iqr = survey_quantile(bing_occ,
                                        quantiles = c(0.25, 0.75),
                                        na.rm = T)) %>%
        filter(black == "Black") %>%
        select(-median_bing_occ_se,-iqr_q25_se,-iqr_q75_se)

### Unweighted counts
n1.r <- brfss %>%
        filter(!is.na(drnkany5)) %>%
        group_by(region, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n2.r <- brfss %>%
        filter(!is.na(binge_current)) %>%
        group_by(region, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n3.r <- brfss %>%
        filter(!is.na(hvy_current)) %>%
        group_by(region, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n4.r <- brfss %>%
        filter(!is.na(avedrnks)) %>%
        group_by(region, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black == "Black")

n5.r <- brfss %>%
        filter(!is.na(bing_occ)) %>%
        group_by(region, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black == "Black")


### Merge prevalence estimates and unweighted counts
table01.r <- merge(n1.r, e1.r, by=c("region", "black", "sexvar"))
table02.r <- merge(n2.r, e2.r, by=c("region", "black", "sexvar"))
table03.r <- merge(n3.r, e3.r, by=c("region", "black", "sexvar"))
table04.r <- merge(n4.r, e4.r, by=c("region", "black", "sexvar"))


# Order datasets by region
table01.r <- table01.r[order(table01.r$region),]
table02.r <- table02.r[order(table02.r$region),]
table03.r <- table03.r[order(table03.r$region),]



### Save the created tables as datasets for data visualizations 
save(table01.r, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table01_r.RData")

save(table02.r, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table02_r.RData")

save(table03.r, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table03_r.RData")

rm(list=ls())

# D. Data Visualiization -------------------------------------------------
# D1. Region Level Bar Graphs ---------------------------------------------

library(tidyverse)

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# Load data tables
load("table01_r.RData")
load("table02_r.RData")
load("table03_r.RData")


# Figure 1.1: Past 30-day drinking

# Include footnote: Binge drinking defined as 4+/5+ drinks for female/males on at least 1 occasion in the past 30 days. 

# Include footnote: Denominator is current drinkers only

g01.r <- ggplot(data = table01.r, aes(x=region, y=drnkany, fill=sexvar)) +
        geom_col(position="dodge") +
        geom_errorbar(aes(ymin=drnkany_low, 
                          ymax=drnkany_upp),position="dodge") +
        geom_text(aes(label=drnkany), 
        position=position_dodge(width=0.9),vjust=5)

g01.r +
        labs(x = "U.S. Census Region", 
             y = "Drank alcohol in past 30-days (%)") +
        scale_fill_brewer(type="qual",
                            palette="Paired", direction=-1,
                            aesthetics = "fill",
                            name="Legend",
                            labels=c("Black Male", "Black Female")) +
        theme_classic() +
        theme(legend.position = "top",
              legend.title = element_blank()) 


# Figure 2.1 Past 30-day binge drinking
g02.r <- ggplot(data = table02.r, aes(x=region, y=binge, 
                                      fill=sexvar)) +
        geom_col(position="dodge") +
        geom_errorbar(aes(ymin=binge_low, 
                          ymax=binge_upp),
                      position="dodge") +
        geom_text(aes(label=binge), 
                  position=position_dodge(width=0.9), 
                  vjust=6)

g02.r +
        labs(x = "U.S. Census Region", 
             y = "Binge drank in past 30-days (%)") +
        scale_fill_brewer(type="qual",
                          palette="Paired", direction=-1,
                          aesthetics = "fill",
                          name="Legend",
                          labels=c("Black Male", "Black Female")) +
        theme_classic() +
        theme(legend.position = "top",
              legend.title = element_blank()) 


# Figure 3.1: Past 30-day heavy drinking

# Include footnote: Heavy drinking defined as 8+/15+ drinks per week for women/men in the past 30 days 

# Include footnote: Denominator is current drinkers only
g03.r <- ggplot(data = table03.r, aes(x=region, y=heavy, 
                                      fill=sexvar)) +
        geom_col(position="dodge") +
        geom_errorbar(aes(ymin=heavy_low, 
                          ymax=heavy_upp),
                      position="dodge") +
        geom_text(aes(label=heavy), 
                  position=position_dodge(width=0.9), 
                  vjust=8)

g03.r +
        labs(x = "U.S. Census Region", 
             y = "Drank heavily in past 30-days (%)") +
        scale_fill_brewer(type="qual",
                          palette="Paired", direction=-1,
                          aesthetics = "fill",
                          name="Legend",
                          labels=c("Black Male", "Black Female")) +
        theme_classic() +
        theme(legend.position = "top",
              legend.title = element_blank())


rm(list=ls())

# D2. State Level Maps ----------------------------------------------------

# ## D2.1: Data Set-Up ----------------------------------------------------
library(tidyverse)
library(gtools) #Need for 'quantcut'
library(fiftystater)
library(mapproj)


# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

load("table01_s.RData")
load("table02_s.RData")
load("table03_s.RData")


# Separate data into specific population groups 
# Break into quantiles

# table01.s.bw <- table01.s %>%
#         ungroup() %>%
#         filter(sexvar == "Female") %>%
#         mutate(state = tolower(state)) %>%
#         rename(id = state) %>% 
#         mutate(drnkany_cat = quantcut(drnkany, q=4, na.rm=T))
# 
# table01.s.bm <- table01.s %>%
#         ungroup() %>%
#         filter(sexvar == "Male") %>%
#         mutate(state = tolower(state)) %>%
#         rename(id = state) %>%
#         mutate(drnkany_cat = quantcut(drnkany, q=4, na.rm=T))
#         
# 
# 
# table02.s.bw <- table02.s %>%
#         ungroup() %>%
#         filter(sexvar == "Female") %>%
#         mutate(state = tolower(state)) %>%
#         rename(id = state) %>%
#         mutate(bingecat = quantcut(binge, q=4, na.rm=T))
# 
# table02.s.bm <- table02.s %>%
#         ungroup() %>%
#         filter(sexvar == "Male") %>%
#         mutate(state = tolower(state)) %>%
#         rename(id = state) %>%
#         mutate(bingecat = quantcut(binge, q=4, na.rm=T))
# 
# 
# table03.s.bw <- table03.s %>%
#         ungroup() %>%
#         filter(sexvar == "Female") %>%
#         mutate(state = tolower(state)) %>%
#         rename(id = state) %>%
#         mutate(heavycat = quantcut(heavy, q=4, na.rm=T))
# 
# table03.s.bm <- table03.s %>%
#         ungroup() %>%
#         filter(sexvar == "Male") %>%
#         mutate(state = tolower(state)) %>%
#         rename(id = state) %>%
#         mutate(heavycat = quantcut(heavy, q=4, na.rm=T))

# Added July 7, 2021: Break data in quantiles using the whole Black pop

table01.s <- table01.s %>%
        ungroup() %>%
        mutate(state = tolower(state)) %>%
        rename(id = state) %>% 
        mutate(drnkany_cat = quantcut(drnkany, q=4, na.rm=T),
               gender = ifelse(sexvar == "Female", "women", "men"),
               population = paste(black,gender,sep=" "))

table02.s <- table02.s %>%
        ungroup() %>%
        mutate(state = tolower(state)) %>%
        rename(id = state) %>%
        mutate(bingecat = quantcut(binge, q=4, na.rm=T),
               gender = ifelse(sexvar == "Female", "women", "men"),
               population = paste(black,gender,sep=" "))

table03.s <- table03.s %>%
        ungroup() %>%
        mutate(state = tolower(state)) %>%
        rename(id = state) %>%
        mutate(heavycat = quantcut(heavy, q=4, na.rm=T),
               gender = ifelse(sexvar == "Female", "women", "men"),
               population = paste(black,gender,sep=" "))

table(table01.s$drnkany_cat)
table(table02.s$bingecat)
table(table03.s$heavycat)
        

# Import map data
data("fifty_states")

str(fifty_states)

# Test map (examine projection)
ggplot(fifty_states, aes(fill = "grey82", map_id = id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map()


# Merge alcohol behavior data with map data
# table01.bw.map <- merge(table01.s.bw, fifty_states, by="id")
# table01.bm.map <- merge(table01.s.bm, fifty_states, by="id")
# 
# table02.bw.map <- merge(table02.s.bw, fifty_states, by="id")
# table02.bm.map <- merge(table02.s.bm, fifty_states, by="id")
# 
# table03.bw.map <- merge(table03.s.bw, fifty_states, by="id")
# table03.bm.map <- merge(table03.s.bm, fifty_states, by="id")


# Added July 7, 2021
table01.map <- merge(table01.s, fifty_states, by="id")
table02.map <- merge(table02.s, fifty_states, by="id")
table03.map <- merge(table03.s, fifty_states, by="id")

# ## D2.2: Produce Maps ---------------------------------------------------

library(RColorBrewer)

# Create palette
my_YlGnBu <- brewer.pal(n = 5, "YlGnBu")[2:5]
my_RdPu <- brewer.pal(n=5, "RdPu")[2:5]

### Figure 1.2: Maps: Black women
# map01.bw <- ggplot(data=table01.bw.map,aes(fill=drnkany_cat,map_id=id)) +
#         geom_map(map=fifty_states, color="black") +
#         expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#         coord_map()
# 
# map01.bw +
#         scale_fill_manual(
#                 values=my_RdPu,
#                 aesthetics = "fill",
#                 na.value="ghostwhite",
#                 name="Legend",
#                 labels=c("34.2-39.9","40.0-44.1",
#                          "44.2-47.5","47.6-55.5",
#                          "No Data")) +
#         theme_void() +
#         theme(legend.position=c(.9,.3), 
#               plot.title = element_text(hjust = 0.5))


### Figure 1.3: Maps: Black Men
# map01.bm <- ggplot(data=table01.bm.map,aes(fill=drnkany_cat,map_id=id)) +
#         geom_map(map=fifty_states, color="black") +
#         expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#         coord_map()
# 
# map01.bw +
#         scale_fill_manual(
#                 values=my_RdPu,
#                 aesthetics = "fill",
#                 na.value="ghostwhite",
#                 name="Legend",
#                 labels=c("42.0-47.8","47.9-52.8",
#                          "52.9-56.2","56.3-63.7",
#                          "No Data")) +
#         theme_void() +
#         theme(legend.position=c(.9,.3), 
#               plot.title = element_text(hjust = 0.5))



### Figure 2.2: Maps: Black women
# map02.bw <- ggplot(data=table02.bw.map,aes(fill=bingecat,map_id=id)) +
#         geom_map(map=fifty_states, color="black") +
#         expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#         coord_map()
# 
# map02.bw +
#         scale_fill_manual(values=my_RdPu,
#                           aesthetics = "fill",
#                           na.value="ghostwhite",
#                           name="Legend",
#                           labels=c("15.4-24.4","24.5-26.8",
#                                    "26.9-30.1","30.2-55.2",
#                                    "No Data")) +
#         theme_void() +
#         theme(legend.position=c(.9,.3), 
#               plot.title = element_text(hjust = 0.5))



### Figure 2.3: Maps: Black men
# map02.bm <- ggplot(data=table02.bm.map,aes(fill=bingecat,map_id=id)) +
#         geom_map(map=fifty_states, color="black") +
#         expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#         coord_map()
# 
# map02.bm +
#         scale_fill_manual(values=my_RdPu,
#                           aesthetics = "fill",
#                           na.value="ghostwhite",
#                           name="Legend",
#                           labels=c("17.3-27.8","27.9-32.9",
#                                    "33.0-37.9","38.0-57.6",
#                                    "No Data")) +
#         theme_void() +
#         theme(legend.position=c(.9,.3), 
#               plot.title = element_text(hjust = 0.5))
# 
# 
# 
# ### Figure 3.2: Maps: Black women
# map03.bw <- ggplot(data=table03.bw.map,aes(fill=heavycat,map_id=id)) +
#         geom_map(map=fifty_states, color="black") +
#         expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#         coord_map()
# 
# map03.bw +
#         scale_fill_manual(values=my_RdPu,
#                           aesthetics = "fill",
#                           na.value="ghostwhite",
#                           name="Legend",
#                           labels=c("4.8-8.15","8.16-10.9",
#                                    "11.0-14.1","14.2-16.7",
#                                    "No Data")) +
#         theme_void() +
#         theme(legend.position=c(.9,.3), 
#               plot.title = element_text(hjust = 0.5))
# 
# 
# ### Figure 3.3: Maps: Black men
# map03.bm <- ggplot(data=table03.bm.map,aes(fill=heavycat,map_id=id)) +
#         geom_map(map=fifty_states, color="black") +
#         expand_limits(x = fifty_states$long, y = fifty_states$lat) +
#         coord_map()
# 
# map03.bm +
#         scale_fill_manual(values=my_RdPu,
#                           aesthetics = "fill",
#                           na.value="ghostwhite",
#                           name="Legend",
#                           labels=c("2.8-5.8","5.9-8.6",
#                                    "8.7-12.6","12.7-22.8",
#                                    "No Data")) +
#         theme_void() +
#         theme(legend.position=c(.9,.3), 
#               plot.title = element_text(hjust = 0.5))


# Added July 7, 2021

# Past 30-day alcohol use (current drinking)

map01 <- ggplot(data=table01.map, aes(fill=drnkany_cat, map_id=id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x=fifty_states$long, y=fifty_states$lat) +
        coord_map() +
        facet_wrap(~population, nrow=1, ncol=2)

map01 +
        scale_fill_manual(
                values=my_RdPu,
                aesthetics = "fill",
                na.value="ghostwhite",
                name="Current Drinking Prevalence",
                labels=c("34.2%-43.9%","44.0%-47.4%",
                         "47.5%-53.1%","53.2%-63.7%",
                         "No Data")) +
        theme_void() +
        theme(legend.position=c(0.5,1.3),
              legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin = margin(6, 6, 6, 6))

# Past 30-day binge drinking (binge drinking)

map02 <- ggplot(data=table02.map, aes(fill=bingecat, map_id=id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x=fifty_states$long, y=fifty_states$lat) +
        coord_map() +
        facet_wrap(~population, nrow=1, ncol=2)

map02 +
        scale_fill_manual(
                values=my_RdPu,
                aesthetics = "fill",
                na.value="ghostwhite",
                name="Binge Drinking Prevalence",
                labels=c("15.4%-25.5%", "25.6%-29.7%",
                         "29.8%-34.6%", "34.5%-57.6%",
                         "No Data")) +
        theme_void() +
        theme(legend.position=c(0.5,1.3),
              legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin=margin(6,6,6,6))

# Past 30-day heavy drinking (heavy drinking)

map03 <- ggplot(data=table03.map, aes(fill=heavycat, map_id=id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x=fifty_states$long, y=fifty_states$lat) +
        coord_map() +
        facet_wrap(~population, nrow=1, ncol=2)

map03 +
        scale_fill_manual(
                values=my_RdPu,
                aesthetics = "fill",
                na.value="ghostwhite",
                name="Heavy Drinking Prevalence",
                labels=c("2.8%-7.05%","7.06%-10.2%",
                         "10.3%-13.7%", "13.8%-22.8%",
                         "No Data")) +
        theme_void() +
        theme(legend.position=c(0.5, 1.3),
              legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin=margin(6,6,6,6))

# E. Appendix: Tables -----------------------------------------------------

library(gt)
library(glue)
library(tidyverse)

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# E1. Region Tables -------------------------------------------------------

# Load data tables
load("table01_r.RData")
load("table02_r.RData")
load("table03_r.RData")

# Past 30-day drinking 
table01.r <- table01.r %>%
        ungroup() %>%
        mutate(population = paste(black,sexvar)) %>%
        select(region, population, n, drnkany, drnkany_low, drnkany_upp) 

table01.r %>%
        gt(groupname_col = "region") %>%
        fmt_number(columns = "n",
                   decimals = 0) %>%
        fmt_percent(columns = starts_with("drnk"), 
                    scale_values = F,
                    decimals = 1) %>%
        cols_label(population = "",
                   n = "N",
                  drnkany = "Weighted %",
                  drnkany_low = "Lower",
                  drnkany_upp = "Upper") %>%
        tab_spanner(label = "95% CI", 
                    columns = matches("drnkany_low|drnkany_upp"))

# Past 30-day binge drinking
table02.r <- table02.r %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar)) %>%
        select(region, population, n, binge, binge_low, binge_upp)

table02.r %>%
        gt(groupname_col = "region") %>%
        fmt_number(columns = "n",
                   decimals = 0) %>%
        fmt_percent(columns = starts_with("binge"), 
                    scale_values = F,
                    decimals = 1) %>%
        cols_label(population = "",
                   n = "N",
                   binge = "Weighted %",
                   binge_low = "Lower",
                   binge_upp = "Upper") %>%
        tab_spanner(label = "95% CI", 
                    columns = matches("binge_low|binge_upp"))


# Past 30-day heavy drinking
table03.r <- table03.r %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar)) %>%
        select(region, population, n, heavy, heavy_low, heavy_upp)

table03.r %>%
        gt(groupname_col = "region") %>%
        fmt_number(columns = "n",
                   decimals = 0) %>%
        fmt_percent(columns = starts_with("heavy"), 
                    scale_values = F,
                    decimals = 1) %>%
        cols_label(population = "",
                   n = "N",
                   heavy = "Weighted %",
                   heavy_low = "Lower",
                   heavy_upp = "Upper") %>%
        tab_spanner(label = "95% CI", 
                    columns = matches("heavy_low|heavy_upp"))




# E2. State tables --------------------------------------------------------

setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# Load data tables
load("table01_s.RData")
load("table02_s.RData")
load("table03_s.RData")

options(scipen = 3)

# Past 30-day drinking
table01.s <- table01.s %>%
        ungroup() %>%
        mutate(population = paste(black,sexvar,sep=""),
               ci = ifelse(is.na(drnkany_low),
                           NA,paste(drnkany_low,drnkany_upp,sep="-"))) %>%
        select(state, population, n, drnkany, ci) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, drnkany, ci)) %>%
        select(state, n_BlackFemale, drnkany_BlackFemale, ci_BlackFemale,
               n_BlackMale, drnkany_BlackMale, ci_BlackMale) 

# Past 30-day binge drinking
table02.s <- table02.s %>%
        ungroup() %>%
        mutate(population = paste(black,sexvar,sep=""),
               ci = ifelse(is.na(binge_low),
                           NA,paste(binge_low,binge_upp,sep="-"))) %>%
        select(state, population, n, binge, ci) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, binge, ci)) %>%
        select(state, n_BlackFemale, binge_BlackFemale,ci_BlackFemale,
               n_BlackMale, binge_BlackMale, ci_BlackMale)

# Past 30-day heavy drinking
table03.s <- table03.s %>%
        ungroup() %>%
        mutate(population = paste(black,sexvar,sep=""),
               ci = ifelse(is.na(heavy_low),
                           NA,paste(heavy_low,heavy_upp,sep="-"))) %>%
        select(state, population, n, heavy, ci) %>%
        pivot_wider(names_from = population,
                    values_from = c(n,heavy,ci)) %>%
        select(state, n_BlackFemale, heavy_BlackFemale,ci_BlackFemale,
               n_BlackMale,heavy_BlackMale,ci_BlackMale)


table01.s %>%
        gt() %>%
        fmt_number(columns = c("n_BlackFemale", "n_BlackMale"),
                   decimals = 0) %>%
        fmt_percent(columns = starts_with("drnk"),
                    scale_values = F,
                    decimals = 1) %>%
        fmt_missing(columns = 2:7,
                    missing_text = "*") %>%
        tab_spanner(label = "Black Female", 
                    columns = vars(n_BlackFemale,drnkany_BlackFemale,
                                   ci_BlackFemale)) %>%
        tab_spanner(label = "Black Male",
                    columns = vars(n_BlackMale,drnkany_BlackMale,
                                   ci_BlackMale)) %>%
        cols_label(state = "State",
                   n_BlackFemale = "n",
                   drnkany_BlackFemale = "Weighted %",
                   ci_BlackFemale = "95% CI",
                   n_BlackMale = "n",
                   drnkany_BlackMale = "Weighted %",
                   ci_BlackMale = "95% CI") %>%
        cols_align(align = c("left"), columns = c("state")) %>%
        cols_align(align = c("center"), columns = c("drnkany_BlackFemale",
                                       "drnkany_BlackMale"))


table02.s %>%
        gt() %>%
        fmt_number(columns = c("n_BlackFemale", "n_BlackMale"),
                   decimals = 0) %>%
        fmt_percent(columns = starts_with("binge"),
                    scale_values = F,
                    decimals = 1) %>%
        fmt_missing(columns = 2:7,
                    missing_text = "*") %>%
        tab_spanner(label = "Black Female",
                    columns = vars(n_BlackFemale,binge_BlackFemale,
                                   ci_BlackFemale)) %>%
        tab_spanner(label = "Black Male",
                    columns = vars(n_BlackMale, binge_BlackMale,
                                  ci_BlackMale)) %>%
        cols_label(state = "State",
                   n_BlackFemale = "n",
                   binge_BlackFemale = "Weighted %",
                   ci_BlackFemale = "95% CI",
                   n_BlackMale = "n",
                   binge_BlackMale = "Weighted %",
                   ci_BlackMale = "95% CI") %>%
        cols_align(align = c("left"), columns = c("state")) %>%
        cols_align(align = c("center"), columns = c("binge_BlackFemale",
                                                    "binge_BlackMale"))


table03.s %>%
        gt() %>%
        fmt_number(columns = c("n_BlackFemale", "n_BlackMale"),
                   decimals = 0) %>%
        fmt_percent(columns = starts_with("heavy"),
                    scale_values = F,
                    decimals = 1) %>%
        fmt_missing(columns = 2:7,
                    missing_text = "*") %>%
        tab_spanner(label = "Black Female",
                    columns = vars(n_BlackFemale,heavy_BlackFemale,
                                   ci_BlackFemale)) %>%
        tab_spanner(label = "Black Male",
                    columns = vars(n_BlackMale, heavy_BlackMale,
                                   ci_BlackMale)) %>%
        cols_label(state = "State",
                   n_BlackFemale = "n",
                   heavy_BlackFemale = "Weighted %",
                   ci_BlackFemale = "95% CI",
                   n_BlackMale = "n",
                   heavy_BlackMale = "Weighted %",
                   ci_BlackMale = "95% CI") %>%
        cols_align(align = c("left"), columns = c("state")) %>%
        cols_align(align = c("center"), columns = c("heavy_BlackFemale",
                                                    "heavy_BlackMale"))


# F. Hypothesis Testing ---------------------------------------------------
library(tidyverse)
library(survey)
library(srvyr) 

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# unweighted data 
load("brfss.RData")

# weighted data
load("brfssdesign.RData")


blackfemale <- subset(brfssdesign, black == "Black" & female == "Female")

m1.1 <- svyglm(drnkany5~region, family=quasibinomial, design=blackfemale,
               na.action=na.omit)
summary(m1.1)


# G. Age as a Confounder? -------------------------------------------------

# Added July 28, 2021
# The purpose of this part of the analysis is to determine what role age may be playing in differences in the prevalence of alcohol use behaviors among Black women and men across states using the 2019 BRFSS data. 

# Q1: Is age associated with each of the 3 alcohol use behaviors within each population group? 

# Q2: Is the age distribution significantly different by state within each population group?

# If these answer to both questions is 'Yes,' then we should probably present age-adjusted prevlaence estimates or consider how not adjusting for age may bias these results. Although, there is a public health argument to be made that adjusting for age is not always important if the implication is about where we put resources rather than trying to make etiologic conclusions. 

# Set-Up data
library(tidyverse)
library(survey)
library(srvyr) 

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# unweighted data 
load("brfss.RData")

# weighted data
load("brfssdesign.RData")

# Set options for allowing a single observation per stratum
options(survey.lonely.psu = "adjust") 

# Create individual-level datasets with age
bf <- subset(brfssdesign, black == "Black" & sexvar == "Female")
bm <- subset(brfssdesign, black == "Black" & sexvar == "Male")

# Create state-level datasets with age

# Past 30-day drinking (drnkany5)
e1.s <- brfssdesign %>%
        group_by(state, black, sexvar) %>%
        summarize(drnkany = round(survey_mean(drnkany5, na.rm=T,
                                              vartype = "ci")*100,1),
                  sample_size = unweighted(n()),
                  avg_age = survey_mean(age80,
                                        vartype = "ci")) %>%
        filter(black == "Black")

n1.s <- brfss %>%
        filter(!is.na(drnkany5)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

# Past 30-day binge drinking among current drinkers (binge_current)
e2.s <- brfssdesign %>%
        group_by(state, black, sexvar) %>%
        summarize(binge = round(survey_mean(binge_current, na.rm=T, 
                                            vartype = "ci")*100,1),
                  sample_size = unweighted(n()),
                  avg_age = survey_mean(age80,
                                        vartype = "ci")) %>%
        filter(black == "Black")

n2.s <- brfss %>%
        filter(!is.na(binge_current)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")



# Past 30-day heavy drinking among current drinkers (hvy_current)
e3.s <- brfssdesign %>%
        group_by(state, black, sexvar) %>%
        summarize(heavy = round(survey_mean(hvy_current, na.rm=T, 
                                            vartype = "ci")*100,1),
                  sample_size = unweighted(n()),
                  avg_age = survey_mean(age80,
                                        vartype = "ci")) %>%
        filter(black == "Black")

n3.s <- brfss %>%
        filter(!is.na(hvy_current)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

### Merge prevalence estimates and unweighted counts
table01.s <- merge(n1.s, e1.s, by=c("state", "black", "sexvar"))
table02.s <- merge(n2.s, e2.s, by=c("state", "black", "sexvar"))
table03.s <- merge(n3.s, e3.s, by=c("state", "black", "sexvar"))

### Suppress prevalence estimates for populations with fewer than 50 cases
table01.s$n[table01.s$sample_size < 50] <- NA
table01.s$drnkany[table01.s$sample_size < 50] <- NA
table01.s$drnkany_low[table01.s$sample_size < 50] <- NA
table01.s$drnkany_upp[table01.s$sample_size < 50] <- NA

table02.s$n[table02.s$sample_size < 50] <- NA
table02.s$binge[table02.s$sample_size < 50] <- NA
table02.s$binge_low[table02.s$sample_size < 50] <- NA
table02.s$binge_upp[table02.s$sample_size < 50] <- NA

table03.s$n[table03.s$sample_size < 50] <- NA
table03.s$heavy[table03.s$sample_size < 50] <- NA
table03.s$heavy_low[table03.s$sample_size < 50] <- NA
table03.s$heavy_upp[table03.s$sample_size < 50] <- NA

# Order datasets by state
table01.s <- table01.s[order(table01.s$state),]
table02.s <- table02.s[order(table02.s$state),]
table03.s <- table03.s[order(table03.s$state),]


# Split the data into the 2 population groups
bf01 <- table01.s %>% filter(sexvar == "Female")
bf02 <- table02.s %>% filter(sexvar == "Female")
bf03 <- table03.s %>% filter(sexvar == "Female")

bm01 <- table01.s %>% filter(sexvar == "Male")
bm02 <- table02.s %>% filter(sexvar == "Male")
bm03 <- table03.s %>% filter(sexvar == "Male")


options(scipen=999)

# Q1: Is the average age of the state population associated with the prevalence of alcohol use behaviors among Black women?

cor(bf01$drnkany,bf01$avg_age, use="pairwise.complete.obs")
cor(bf02$binge,bf02$avg_age, use="pairwise.complete.obs")
cor(bf03$heavy,bf03$avg_age, use="pairwise.complete.obs")

plot(bf01$avg_age, bf01$drnkany, type="p")
plot(bf02$avg_age, bf02$binge, type="p")
plot(bf03$avg_age, bf03$heavy, type="p")

summary(lm(drnkany ~ avg_age, data = bf01, na.action = na.omit))
summary(lm(binge ~ avg_age, data = bf02, na.action = na.omit))
summary(lm(heavy ~ avg_age, data = bf03, na.action = na.omit))

# Q2: Is the average age of the Black female population significantly different across states?


ggplot(data = filter(bf01, !is.na(n)), 
                     mapping = aes(x = avg_age,
                                       y = reorder(state, avg_age))) +
        geom_point() +
        geom_pointrange(mapping = aes(xmin = avg_age_low,
                                      xmax = avg_age_upp)) +
        labs(x = "Average Age", y = "State")


bf.m1 <- lm(drnkany ~ avg_age, data = bf01, na.action = na.exclude)
bf01$pred.drnkany <- predict.glm(bf.m1, type="response")

# Past 30-day drinking (drnkany5)
# bf %>%
#         group_by(drnkany5) %>%
#         summarize(avg_age = survey_mean(age80,
#                                         vartype = "ci"))
# svyttest(age80~drnkany5, bf)

# Conclusion: Among Black women, current drinkers are significantly younger than non-drinkers across US states.
# 
# # Past 30-day binge drinking among current drinkers (binge_current)
# bf %>%
#         group_by(binge_current) %>%
#         summarize(avg_age = survey_mean(age80,
#                                         vartype = "ci"))
# svyttest(age80~binge_current, bf)
# 
# # Conclusions: Among Black women, current binge drinkers are significantly younger than non-binge drinkers (among those that drink alcohol).
# 
# 
# # Past 30-day heavy drinking among current drinkers (hvy_current)
# 
# bf %>%
#         group_by(hvy_current) %>%
#         summarize(avg_age = survey_mean(age80,
#                                         vartype = "ci"))
# svyttest(age80~hvy_current, bf)
# 
# # Conclusions: Among Black women, current heavy drinkers are younger (those not as much compared with binge dirnking) than those who do not drink heavily (among those that drink). 
# 
# # Q2: Black female
# 
# state_age <- bf %>%
#         group_by(state) %>%
#         summarize(avg_age = survey_mean(age80, vartype = "ci"))
# 

# 
# summary(lm(avg_age ~ state, data = state_age))
# 
# # kwtest <- svyranktest(age80~state, design = bf, na = TRUE, test=("KruskalWallis"))
# # kwtest
# 
# # summary(svyglm(age80~state, design=bf, na.action = na.omit))


# H. Age-Standardized Prevalence Estimates --------------------------------
# This section updated on September 10, 2021

# Added July 30, 2021

# After thinking about it more, it seems important to present age-adjusted or age-standradized rates in order to compare the prevlance of these outcomes across the states. 

# Packages
library(tidyverse)
library(survey)
library(srvyr) 

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# unweighted data 
load("brfss.RData")

# survey-set data
load("brfssdesign.RData")

options(survey.lonely.psu = "adjust")

### Calculate age-specific prevalence for each alcohol use behavior in the overall Black population

drnk.age.all <- brfssdesign %>%
        group_by(state, black, age4cat) %>%
        summarize(drnkany = round(survey_mean(drnkany5, na.rm=T,
                                              vartype = "ci")*100,1)) %>%
        filter(black == "Black")

binge.age.all <- brfssdesign %>%
        group_by(state, black, age4cat) %>%
        summarize(binge = round(survey_mean(binge_current, na.rm=T, 
                                            vartype = "ci")*100,1)) %>%
        filter(black == "Black")

heavy.age.all <- brfssdesign %>%
        group_by(state, black, age4cat) %>%
        summarize(heavy = round(survey_mean(hvy_current, na.rm=T, 
                                            vartype = "ci")*100,1)) %>%
        filter(black == "Black")

### Calculate age-specifc prevalence for each alcohol use behavior for Black women and Black men within each state

# Past 30-day drinking (drnkany5)
drnk.age <- brfssdesign %>%
        group_by(state, black, sexvar, age4cat) %>%
        summarize(drnkany = round(survey_mean(drnkany5, na.rm=T,
                                              vartype = "ci")*100,1),
                  sample_size = unweighted(n())) %>%
        filter(black == "Black")

# Past 30-day binge drinking among current drinkers (binge_current)
binge.age <- brfssdesign %>%
        group_by(state, black, sexvar, age4cat) %>%
        summarize(binge = round(survey_mean(binge_current, na.rm=T, 
                                            vartype = "ci")*100,1),
                  sample_size = unweighted(n())) %>%
        filter(black == "Black")

# Past 30-day heavy drinking among current drinkers (hvy_current)
heavy.age <- brfssdesign %>%
        group_by(state, black, sexvar, age4cat) %>%
        summarize(heavy = round(survey_mean(hvy_current, na.rm=T, 
                                            vartype = "ci")*100,1),
                  sample_size = unweighted(n())) %>%
        filter(black == "Black")

# Create age standardized weights to link with the age-specific prevalence estimates 
# Source: https://www.cdc.gov/nchs/data/statnt/statnt20.pdf

age4cat <- c(1,2,3,4)
age_weight <- c(0.128810, 0.401725, 0.299194, 0.170271)

stand.pop <- data.frame(cbind(age4cat, age_weight))

stand.pop$age4cat <- factor(stand.pop$age4cat,
                                     levels=c(1,2,3,4),
                                     labels=c("18-24", "25-44", 
                                              "45-64", "65+"))

# Merge the standard population with the age-specific prevalence estimates

drnk.age.stand.all <- merge(drnk.age.all, stand.pop, by="age4cat")
binge.age.stand.all <- merge(binge.age.all, stand.pop, by="age4cat")
heavy.age.stand.all <- merge(heavy.age.all, stand.pop, by="age4cat")

drnk.age.stand <- merge(drnk.age, stand.pop, by="age4cat")
binge.age.stand <- merge(binge.age, stand.pop, by="age4cat")
heavy.age.stand <- merge(heavy.age, stand.pop, by="age4cat")

# Multiply the estimated prevalence by the age weight

drnk.age.stand.all <- drnk.age.stand.all %>%
        mutate(drnkany_a = drnkany*age_weight,
               drnkany_low_a = drnkany_low*age_weight,
               drnkany_upp_a = drnkany_upp*age_weight)

binge.age.stand.all <- binge.age.stand.all %>%
        mutate(binge_a = binge*age_weight,
               binge_low_a = binge_low*age_weight,
               binge_upp_a = binge_upp*age_weight)

heavy.age.stand.all <- heavy.age.stand %>%
        mutate(heavy_a = heavy*age_weight,
               heavy_low_a = heavy_low*age_weight,
               heavy_upp_a = heavy_upp*age_weight)


drnk.age.stand <- drnk.age.stand %>%
        mutate(drnkany_a = drnkany*age_weight,
               drnkany_low_a = drnkany_low*age_weight,
               drnkany_upp_a = drnkany_upp*age_weight)

binge.age.stand <- binge.age.stand %>%
        mutate(binge_a = binge*age_weight,
               binge_low_a = binge_low*age_weight,
               binge_upp_a = binge_upp*age_weight)

heavy.age.stand <- heavy.age.stand %>%
        mutate(heavy_a = heavy*age_weight,
               heavy_low_a = heavy_low*age_weight,
               heavy_upp_a = heavy_upp*age_weight)

# Sum the new prevalences across the age groups within the states and subpopulations (Black women and Black men)

drnk.age.stand.all <- drnk.age.stand.all %>%
        group_by(state) %>%
        summarize(drnkany_a = sum(drnkany_a),
                  drnkany_low_a = sum(drnkany_low_a),
                  drnkany_upp_a = sum(drnkany_upp_a)) %>%
        rename(overall_a = drnkany_a,
               overall_low_a = drnkany_low_a,
               overall_upp_a = drnkany_upp_a)

binge.age.stand.all <- binge.age.stand.all %>%
        group_by(state) %>%
        summarize(binge_a = sum(binge_a),
                  binge_low_a = sum(binge_low_a),
                  binge_upp_a = sum(binge_upp_a)) %>%
        rename(overall_a = binge_a,
               overall_low_a = binge_low_a,
               overall_upp_a = binge_upp_a)

heavy.age.stand.all <- heavy.age.stand.all %>%
        group_by(state) %>%
        summarize(heavy_a = sum(heavy_a),
                  heavy_low_a = sum(heavy_low_a),
                  heavy_upp_a = sum(heavy_upp_a)) %>%
        rename(overall_a = heavy_a,
               overall_low_a = heavy_low_a,
               overall_upp_a = heavy_upp_a)




drnk.age.stand <- drnk.age.stand %>%
        group_by(state, sexvar, black) %>%
        summarize(drnkany_a = sum(drnkany_a),
                  drnkany_low_a = sum(drnkany_low_a),
                  drnkany_upp_a = sum(drnkany_upp_a),
                  sample_size = sum(sample_size)) %>%
        ungroup()


binge.age.stand <- binge.age.stand %>%
        group_by(state, sexvar, black) %>%
        summarize(binge_a = sum(binge_a),
                  binge_low_a = sum(binge_low_a),
                  binge_upp_a = sum(binge_upp_a),
                  sample_size = sum(sample_size)) %>%
        ungroup()

heavy.age.stand <- heavy.age.stand %>%
        group_by(state, sexvar, black) %>%
        summarize(heavy_a = sum(heavy_a),
                  heavy_low_a = sum(heavy_low_a),
                  heavy_upp_a = sum(heavy_upp_a),
                  sample_size = sum(sample_size)) %>%
        ungroup()

# Calculate the unweighted counts of people who answer the alcohol use behavior question

### Unweighted counts
n1.s <- brfss %>%
        filter(!is.na(drnkany5)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n2.s <- brfss %>%
        filter(!is.na(binge_current)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

n3.s <- brfss %>%
        filter(!is.na(hvy_current)) %>%
        group_by(state, black, sexvar) %>%
        summarize(n=n()) %>%
        filter(black=="Black")

# Merge the age-standardized prevalence with unweighted number of denominator cases for that alcohol use behavior
table01.s.adj <- merge(n1.s, drnk.age.stand, 
                       by=c("state", "black", "sexvar"))

table02.s.adj <- merge(n2.s, binge.age.stand, 
                   by=c("state", "black", "sexvar"))

table03.s.adj <- merge(n3.s, heavy.age.stand, 
                   by=c("state", "black", "sexvar"))

### Suppress prevalence estimates for populations with fewer than 50 cases - this was changed on 
table01.s.adj$n[table01.s.adj$sample_size < 60] <- NA
table01.s.adj$drnkany_a[table01.s.adj$sample_size < 60] <- NA
table01.s.adj$drnkany_low_a[table01.s.adj$sample_size < 60] <- NA
table01.s.adj$drnkany_upp_a[table01.s.adj$sample_size < 60] <- NA

table02.s.adj$n[table02.s.adj$sample_size < 60] <- NA
table02.s.adj$binge_a[table02.s.adj$sample_size < 60] <- NA
table02.s.adj$binge_low_a[table02.s.adj$sample_size < 60] <- NA
table02.s.adj$binge_upp_a[table02.s.adj$sample_size < 60] <- NA

table03.s.adj$n[table03.s.adj$sample_size < 60] <- NA
table03.s.adj$heavy_a[table03.s.adj$sample_size < 60] <- NA
table03.s.adj$heavy_low_a[table03.s.adj$sample_size < 60] <- NA
table03.s.adj$heavy_upp_a[table03.s.adj$sample_size < 60] <- NA

# Order datasets by state
table01.s.adj <- table01.s.adj[order(table01.s.adj$state),]
table02.s.adj <- table02.s.adj[order(table02.s.adj$state),]
table03.s.adj <- table03.s.adj[order(table03.s.adj$state),]

# Save tables to be used to created tables output and maps (data visualizations)
save(table01.s.adj, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table01_s_adj.RData")

save(table02.s.adj, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table02_s_adj.RData")

save(table03.s.adj, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table03_s_adj.RData")

save(drnk.age.stand.all, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table01_overall_adj.RData")

save(binge.age.stand.all, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table02_overall_adj.RData")

save(heavy.age.stand.all, file="C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output/table03_overall_adj.RData")

rm(list=ls())


# I. Maps using Age-Standardized Prevalence Estimates (using fifty_states package) ---------------------

library(tidyverse)
library(gtools) #Need for 'quantcut'
library(fiftystater)
library(mapproj)


# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# Load data
load("table01_s_adj.RData")
load("table02_s_adj.RData")
load("table03_s_adj.RData")

# Added September 9, 2021: Import ANSI data with FIPS code and State abbreviation availabe at: https://www.census.gov/library/reference/code-lists/ansi.html#par_textimage_3

library(readr)
ansi <- read_delim("https://www2.census.gov/geo/docs/reference/state.txt", "|", escape_double = FALSE, trim_ws = TRUE)
View(ansi)

# Remove places that are not US states; rename variables, select on these variables for merging with table data 
ansi <- ansi %>%
        filter(STATE != '11') %>%
        filter(STATE != '60') %>%
        filter(STATE != '66') %>% 
        filter(STATE != '69') %>% 
        filter(STATE != '72') %>%
        filter(STATE != '74') %>%
        filter(STATE != '78') %>%
        rename(stusab = STUSAB,
               state = STATE_NAME) %>%
        select(stusab, state)

# Merge table data with state abbreviations
table01.s.adj <- merge(table01.s.adj, ansi, by="state")
table02.s.adj <- merge(table02.s.adj, ansi, by="state")
table03.s.adj <- merge(table03.s.adj, ansi, by="state")

# Examine quartiles
quantile(table01.s.adj$drnkany_a, probs=seq(0,1,0.25), na.rm=T)
quantile(table02.s.adj$binge_a, probs=seq(0,1,0.25), na.rm=T)
quantile(table03.s.adj$heavy_a, probs=seq(0,1,0.25), na.rm=T)

# Break data in quantiles using the whole Black pop

table01.s.adj <- table01.s.adj %>%
        ungroup() %>%
        mutate(state = tolower(state)) %>%
        rename(id = state) %>% 
        mutate(drnkany_cat_a = quantcut(drnkany_a, q=4, na.rm=T),
               gender = ifelse(sexvar == "Female", "women", "men"),
               population = paste(black,gender,sep=" "))

table02.s.adj <- table02.s.adj %>%
        ungroup() %>%
        mutate(state = tolower(state)) %>%
        rename(id = state) %>%
        mutate(bingecat_a = quantcut(binge_a, q=4, na.rm=T),
               gender = ifelse(sexvar == "Female", "women", "men"),
               population = paste(black,gender,sep=" "))

table03.s.adj <- table03.s.adj %>%
        ungroup() %>%
        mutate(state = tolower(state)) %>%
        rename(id = state) %>%
        mutate(heavycat_a = quantcut(heavy_a, q=4, na.rm=T),
               gender = ifelse(sexvar == "Female", "women", "men"),
               population = paste(black,gender,sep=" "))


# Import map data
data("fifty_states")

str(fifty_states)

# Test map (examine projection)
ggplot(fifty_states, aes(fill = "grey82", map_id = id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map()

points <- fifty_states %>%
        group_by(id) %>%
        summarize(avg_x = mean(long),
                  avg_y = mean(lat))

ggplot(fifty_states, aes(fill = "grey82", map_id = id)) +
        geom_map(map=fifty_states, color="black") +
        geom_point(data=points, aes(x=avg_x, y=avg_y)) +
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map()

# Merge prevalence data with map info for visualization
table01.map.a <- merge(table01.s.adj, fifty_states, by="id")
table02.map.a <- merge(table02.s.adj, fifty_states, by="id")
table03.map.a <- merge(table03.s.adj, fifty_states, by="id")

# Produce maps
library(RColorBrewer)

# Create palette
my_YlGnBu <- brewer.pal(n = 5, "YlGnBu")[2:5]
my_RdPu <- brewer.pal(n=5, "RdPu")[2:5]
my_OrRd <- brewer.pal(n=5, "OrRd")[2:5]

# Pattern Geoms
# install.packages("remotes")
# remotes::install_github("coolbutuseless/ggpattern")
# library(ggpattern)

# Note September 7, 2021
# After trying to figure out how to add state abbreviation labels to the maps I already created, it seems that using fifty_stater is too old of a package for this. A new package, 'usmap,' has been created in 2021 that maps it very east to include these types of things. It may also be possible to use a solid line to highligh the US regions. However, many of the labels for small states in New England are larger than the state projection itself and therefore overlap. To make the process quicker, I think I will manually add the state abbreviatons in another program, like Microsoft Paint.
        
        
# Past 30-day age-adjusted alcohol use (current drinking)
png("Map01_CurrentDrinking_AgeAdjusted_20210907.png", width = 7.7, height = 4, units = 'in', res = 300)


map01.a <- ggplot(data=table01.map.a,
                  aes(fill=drnkany_cat_a, map_id=id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x=fifty_states$long, y=fifty_states$lat) +
        coord_map() +
        facet_wrap(~population, nrow=1, ncol=2)

map01.a +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("34.5%-44.0%", "44.1%-48.4%",
                         "48.5%-53.0%","53.1%-64.0%",
                         "No Data")) +
        theme_void() +
        theme(legend.position=c(0.5,1.3),
              legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin = margin(6, 6, 6, 6))
dev.off() 

# Past 30-day binge drinking (binge drinking)

map02 <- ggplot(data=table02.map.a, aes(fill=bingecat_a, map_id=id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x=fifty_states$long, y=fifty_states$lat) +
        coord_map() +
        facet_wrap(~population, nrow=1, ncol=2)

map02 +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("14.9%-24.4%", "24.5%-28.7%",
                         "28.8%-33.3%", "33.4%-51.0%",
                         "No Data")) +
        theme_void() +
        theme(legend.position=c(0.5,1.3),
              legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin=margin(6,6,6,6))

# Past 30-day heavy drinking (heavy drinking)

map03 <- ggplot(data=table03.map.a, aes(fill=heavycat_a, map_id=id)) +
        geom_map(map=fifty_states, color="black") +
        expand_limits(x=fifty_states$long, y=fifty_states$lat) +
        coord_map() +
        facet_wrap(~population, nrow=1, ncol=2)

map03 +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("2.5%-6.5%", "6.6%-9.3%",
                         "9.4%-13.0%", "13.1%-25.7%",
                         "No Data")) +
        theme_void() +
        theme(legend.position=c(0.5, 1.3),
              legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin=margin(6,6,6,6))


rm(list=ls())



# J. Tables with Age-adjusted Prevalence ----------------------------------

library(tidyverse)
library(gt)
library(glue)


# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# Load data
load("table01_s_adj.RData")
load("table02_s_adj.RData")
load("table03_s_adj.RData")

load("table01_overall_adj.RData")
load("table02_overall_adj.RData")
load("table03_overall_adj.RData")

options(scipen = 3)


# Set up the data 

# Past 30-day drinking (age-adjusted)
table01.s.adj <- table01.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black,sexvar,sep=""),
               ci = ifelse(is.na(drnkany_low_a),
                           NA,paste(
                                   signif(drnkany_low_a,3),
                                    signif(drnkany_upp_a,3),
                                   sep="-"))) %>%
        select(state, population, n, drnkany_a, ci) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, drnkany_a, ci)) %>%
        select(state, n_BlackFemale, drnkany_a_BlackFemale, 
               ci_BlackFemale,
               n_BlackMale, drnkany_a_BlackMale, ci_BlackMale)

table01.s.adj <- merge(table01.s.adj, drnk.age.stand.all, 
                       by="state")

rm(drnk.age.stand.all)

# Past 30-day binge drinking (age-adjusted)
table02.s.adj <- table02.s.adj %>%
        ungroup() %>%
        mutate(binge_low_a_m = ifelse(binge_low_a < 0,
                                      0,binge_low_a)) %>%
        mutate(population = paste(black,sexvar,sep=""),
               ci = ifelse(is.na(binge_a),
                           NA,paste(
                                   round(binge_low_a_m,1),
                                   round(binge_upp_a,1),
                                   sep="-"))) %>%
        select(state, population, n, binge_a, ci) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, binge_a, ci)) %>%
        select(state, n_BlackFemale, binge_a_BlackFemale,
               ci_BlackFemale,
               n_BlackMale, binge_a_BlackMale, ci_BlackMale)

table02.s.adj <- merge(table02.s.adj, binge.age.stand.all, 
                       by="state")

rm(binge.age.stand.all)


# Past 30-day heavy drinking (age-adjusted)
table03.s.adj <- table03.s.adj %>%
        ungroup() %>%
        mutate(heavy_low_a_m = ifelse(heavy_low_a < 0,
                                      0, heavy_low_a)) %>%
        mutate(population = paste(black,sexvar,sep=""),
               ci = ifelse(is.na(heavy_low_a),
                           NA,paste(
                                   round(heavy_low_a_m,1),
                                   round(heavy_upp_a,1),
                                   sep="-"))) %>%
        select(state, population, n, heavy_a, ci) %>%
        pivot_wider(names_from = population,
                    values_from = c(n,heavy_a,ci)) %>%
        select(state, n_BlackFemale, heavy_a_BlackFemale,
               ci_BlackFemale,
               n_BlackMale,heavy_a_BlackMale,ci_BlackMale)

table03.s.adj <- merge(table03.s.adj, heavy.age.stand.all, 
                       by="state")

rm(heavy.age.stand.all)

# Suppress Overall numbers where sample size low

table01.s.adj <-table01.s.adj %>%
        mutate(overall_low_a = ifelse(overall_low_a < 0,
                                      0, overall_low_a)) %>%
        mutate(overall_a = ifelse(is.na(n_BlackFemale) | 
                                          is.na(n_BlackMale),NA,
                                  overall_a),
               overall_low_a = ifelse(is.na(n_BlackFemale) |
                                              is.na(n_BlackMale),
                                      NA,overall_low_a),
               overall_upp_a = ifelse(is.na(n_BlackFemale) |
                                              is.na(n_BlackMale),
                                      NA,overall_upp_a)) %>%
        mutate(overall_ci = ifelse(is.na(overall_a),
                           NA,paste(
                                   round(overall_low_a,1),
                                   round(overall_upp_a,1),
                                   sep="-"))) %>%
        select(-overall_low_a, -overall_upp_a)


table02.s.adj <-table02.s.adj %>%
        mutate(overall_low_a = ifelse(overall_low_a < 0,
                                      0, overall_low_a)) %>%
        mutate(overall_a = ifelse(is.na(n_BlackFemale) | 
                                          is.na(n_BlackMale),NA,
                                  overall_a),
               overall_low_a = ifelse(is.na(n_BlackFemale) |
                                              is.na(n_BlackMale),
                                      NA,overall_low_a),
               overall_upp_a = ifelse(is.na(n_BlackFemale) |
                                              is.na(n_BlackMale),
                                      NA,overall_upp_a)) %>%
        mutate(overall_ci = ifelse(is.na(overall_a),
                                   NA,paste(
                                           round(overall_low_a,1),
                                           round(overall_upp_a,1),
                                           sep="-"))) %>%
        select(-overall_low_a, -overall_upp_a)



table03.s.adj <-table03.s.adj %>%
        mutate(overall_low_a = ifelse(overall_low_a < 0,
                                      0, overall_low_a)) %>%
        mutate(overall_a = ifelse(is.na(n_BlackFemale) | 
                                          is.na(n_BlackMale),NA,
                                  overall_a),
               overall_low_a = ifelse(is.na(n_BlackFemale) |
                                              is.na(n_BlackMale),
                                      NA,overall_low_a),
               overall_upp_a = ifelse(is.na(n_BlackFemale) |
                                              is.na(n_BlackMale),
                                      NA,overall_upp_a)) %>%
        mutate(overall_ci = ifelse(is.na(overall_a),
                                   NA,paste(
                                           round(overall_low_a,1),
                                           round(overall_upp_a,1),
                                           sep="-"))) %>%
        select(-overall_low_a, -overall_upp_a)

# Print tables

table01.s.adj %>%
        gt() %>%
        fmt_number(columns = c("n_BlackFemale", "n_BlackMale"),
                   decimals = 0) %>%
        fmt_percent(columns = vars(drnkany_a_BlackFemale,
                                   drnkany_a_BlackMale,
                                   overall_a),
                    scale_values = F,
                    decimals = 1) %>%
        fmt_missing(columns = 2:9,
                    missing_text = "*") %>%
        tab_spanner(label = "Black Female", 
                    columns = vars(n_BlackFemale,drnkany_a_BlackFemale,
                                   ci_BlackFemale)) %>%
        tab_spanner(label = "Black Male",
                    columns = vars(n_BlackMale,drnkany_a_BlackMale,
                                   ci_BlackMale)) %>%
        tab_spanner(label = "Overall",
                    columns = vars(overall_a,overall_ci)) %>%
        cols_label(state = "State",
                   n_BlackFemale = "n",
                   drnkany_a_BlackFemale = "Weighted %",
                   ci_BlackFemale = "95% CI",
                   n_BlackMale = "n",
                   drnkany_a_BlackMale = "Weighted %",
                   ci_BlackMale = "95% CI",
                   overall_a = "Weighted %",
                   overall_ci = "95% CI") %>%
        cols_align(align = c("left"), columns = c("state")) %>%
        cols_align(align = c("center"), 
                   columns = c("drnkany_a_BlackFemale",
                               "drnkany_a_BlackMale",
                               "overall_a"))



table02.s.adj %>%
        gt() %>%
        fmt_number(columns = c("n_BlackFemale", "n_BlackMale"),
                   decimals = 0) %>%
        fmt_percent(columns = vars(binge_a_BlackFemale,
                                   binge_a_BlackMale,
                                   overall_a),
                    scale_values = F,
                    decimals = 1) %>%
        fmt_missing(columns = 2:9,
                    missing_text = "*") %>%
        tab_spanner(label = "Black Female",
                    columns = vars(n_BlackFemale,binge_a_BlackFemale,
                                   ci_BlackFemale)) %>%
        tab_spanner(label = "Black Male",
                    columns = vars(n_BlackMale, binge_a_BlackMale,
                                   ci_BlackMale)) %>%
        tab_spanner(label = "Overall",
                    columns = vars(overall_a,overall_ci)) %>%
        cols_label(state = "State",
                   n_BlackFemale = "n",
                   binge_a_BlackFemale = "Weighted %",
                   ci_BlackFemale = "95% CI",
                   n_BlackMale = "n",
                   binge_a_BlackMale = "Weighted %",
                   ci_BlackMale = "95% CI",
                   overall_a = "Weighted %",
                   overall_ci = "95% CI") %>%
        cols_align(align = c("left"), columns = c("state")) %>%
        cols_align(align = c("center"), 
                   columns = c("binge_a_BlackFemale",
                               "binge_a_BlackMale",
                               "overall_a"))


table03.s.adj %>%
        gt() %>%
        fmt_number(columns = c("n_BlackFemale", "n_BlackMale"),
                   decimals = 0) %>%
        fmt_percent(columns = vars(heavy_a_BlackFemale,
                                   heavy_a_BlackMale,
                                   overall_a),
                    scale_values = F,
                    decimals = 1) %>%
        fmt_missing(columns = 2:9,
                    missing_text = "*") %>%
        tab_spanner(label = "Black Female",
                    columns = vars(n_BlackFemale,heavy_a_BlackFemale,
                                   ci_BlackFemale)) %>%
        tab_spanner(label = "Black Male",
                    columns = vars(n_BlackMale, heavy_a_BlackMale,
                                   ci_BlackMale)) %>%
        tab_spanner(label = "Overall",
                    columns = vars(overall_a,
                                   overall_ci)) %>%
        cols_label(state = "State",
                   n_BlackFemale = "n",
                   heavy_a_BlackFemale = "Weighted %",
                   ci_BlackFemale = "95% CI",
                   n_BlackMale = "n",
                   heavy_a_BlackMale = "Weighted %",
                   ci_BlackMale = "95% CI",
                   overall_a = "Weighted %",
                   overall_ci = "95% CI") %>%
        cols_align(align = c("left"), columns = c("state")) %>%
        cols_align(align = c("center"), columns = 
                           c("heavy_a_BlackFemale",
                             "heavy_a_BlackMale",
                             "overall_a"))


# K. Male Excess Charts ---------------------------------------------------

library(tidyverse)
library(gt)
library(glue)
library(ggpattern)
library(ggrepel)


# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# Load data
load("table01_s_adj.RData")
load("table02_s_adj.RData")
load("table03_s_adj.RData")

# Region data
# load("brfss.RData")
# 
# region <- brfss %>%
#         group_by(region, state) %>%
#         summarize()


# Past 30-day drinking (age-adjusted)
male.excess.01 <- table01.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar, sep="")) %>%
        select(state, population, n, drnkany_a, drnkany_low_a,
               drnkany_upp_a) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, drnkany_a, drnkany_low_a,
                                    drnkany_upp_a)) %>%
        mutate(male_excess = drnkany_a_BlackMale-drnkany_a_BlackFemale,
               male_exccess_low = 
                       drnkany_low_a_BlackMale-drnkany_low_a_BlackFemale,
               male_excess_upp = 
                       drnkany_upp_a_BlackMale-drnkany_upp_a_BlackFemale) %>%
        select(state, male_excess, male_exccess_low, male_excess_upp)

# male.excess.01 <- merge(male.excess.01, region, by="state")

# me.plot.01 <- ggplot(data=filter(male.excess.01, !is.na(male_excess)),
#                      aes(x=reorder(state,male_excess),y=male_excess)) +
#         geom_col() +
#         geom_text(aes(label=round(male_excess,1))) +
#         #geom_errorbar(aes(ymin=male_exccess_low,ymax=male_excess_upp))+
#         coord_cartesian(xlim = c(-10, 30)) +
#         coord_flip() #+
#         #facet_wrap(vars(region), nrow=2, ncol=2,scales = "free_y")

me.plot.01 <- ggplot(data=filter(male.excess.01, !is.na(male_excess)),
                     aes(x=male_excess,y=reorder(state, male_excess))) +
        geom_col(colour='black',fill = 'white', width=0.75) +
        coord_cartesian(xlim = c(-5,30)) +
        geom_text(data=filter(male.excess.01, male_excess > 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = 1, nudge_y = 0.1) +
        geom_text(data=filter(male.excess.01, male_excess < 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = -1, nudge_y = 0.1)

me.plot.01 +
        labs(x="Age-adjusted Prevalence Difference in Past 30-day Drinking (%Male - %Female)",
             y="State") +
        theme_classic()




# Past 30-day binge drinking (age-adjusted)
male.excess.02 <- table02.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar, sep="")) %>%
        select(state, population, n, binge_a, binge_low_a,
               binge_upp_a) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, binge_a, binge_low_a,
                                    binge_upp_a)) %>%
        mutate(male_excess = binge_a_BlackMale-binge_a_BlackFemale,
               male_exccess_low = 
                       binge_low_a_BlackMale-binge_low_a_BlackFemale,
               male_excess_upp = 
                       binge_upp_a_BlackMale-binge_upp_a_BlackFemale) %>%
        select(state, male_excess, male_exccess_low, male_excess_upp)



me.plot.02 <- ggplot(data=filter(male.excess.02, !is.na(male_excess)),
                     aes(x=male_excess,y=reorder(state, male_excess))) +
        geom_col_pattern(width=0.75, 
                         pattern='stripe',fill='white',
                         color='black',
                         pattern_size=0.5) +
        coord_cartesian(xlim = c(-20,40)) +
        geom_text(data=filter(male.excess.02, male_excess > 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = 2) +
        geom_text(data=filter(male.excess.02, male_excess < 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = -2)

me.plot.02 +
        labs(x="Age-adjusted Prevalence Difference in Binge Drinking (%Male - %Female)",
             y="State") +
        theme_classic()


# Past 30-day heavy drinking
male.excess.03 <- table03.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar, sep="")) %>%
        select(state, population, n, heavy_a, heavy_low_a,
               heavy_upp_a) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, heavy_a, heavy_low_a,
                                    heavy_upp_a)) %>%
        mutate(male_excess = heavy_a_BlackMale-heavy_a_BlackFemale,
               male_exccess_low = 
                       heavy_low_a_BlackMale-heavy_low_a_BlackFemale,
               male_excess_upp = 
                       heavy_upp_a_BlackMale-heavy_upp_a_BlackFemale) %>%
        select(state, male_excess, male_exccess_low, male_excess_upp)

me.plot.03 <- ggplot(data=filter(male.excess.03, !is.na(male_excess)),
                     aes(x=male_excess,y=reorder(state, male_excess))) +
        geom_col_pattern(width=0.75,
                         pattern='circle', 
                         fill='white',
                         colour='black') +
        coord_cartesian(xlim = c(-15,25)) +
        geom_text(data=filter(male.excess.03, male_excess > 0),
                          aes(label=round(male_excess,1)), size=3,
                          nudge_x = 1.5) +
        geom_text(data=filter(male.excess.03, male_excess < 0),
                          aes(label=round(male_excess,1)), size=3,
                          nudge_x = -1.5)

me.plot.03 +
        labs(x="Age-adjusted Prevalence Difference in Heavy Drinking (%Male - %Female)",
             y="State") +
        theme_classic()
