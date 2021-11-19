### Postdoc Project: Alcohol Use among Black Populations in the US
### Prgram: AUBP_Program03.R
### Author: Naomi Greene
### Creation Date: September 8, 2021

### The purpose of this program is to create the figures and tables for the manuscript, Gender Differences in State Level Alcohol Use among Black Women and Men in the United States. 


# A. Data Management  ----------------------------------------------------
library(dplyr)
library(gtools) #Need for 'quantcut'


# A1. Age-adjusted Alcohol Prevalence Quartiles -------------------------

# Set working directory
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/output")

# Load tables with age-adjusted prevalence estimates by state and pop
load("table01_s_adj.RData")
load("table02_s_adj.RData")
load("table03_s_adj.RData")

# Examine quartiles
quantile(table01.s.adj$drnkany_a, probs=seq(0,1,0.25), na.rm=T)
quantile(table02.s.adj$binge_a, probs=seq(0,1,0.25), na.rm=T)
quantile(table03.s.adj$heavy_a, probs=seq(0,1,0.25), na.rm=T)

# Break data in quartiles using the whole Black pop

table01.s.adj <- table01.s.adj %>%
     ungroup() %>%
     mutate(state = tolower(state)) %>%
     #rename(region = state) %>% 
     mutate(drnkany_cat_a = quantcut(drnkany_a, q=4, na.rm=T),
            gender = ifelse(sexvar == "Female", "women", "men"),
            population = paste(black,gender,sep=" "))

table02.s.adj <- table02.s.adj %>%
     ungroup() %>%
     mutate(state = tolower(state)) %>%
     #rename(region = state) %>%
     mutate(bingecat_a = quantcut(binge_a, q=4, na.rm=T),
            gender = ifelse(sexvar == "Female", "women", "men"),
            population = paste(black,gender,sep=" "))

table03.s.adj <- table03.s.adj %>%
     ungroup() %>%
     mutate(state = tolower(state)) %>%
     #rename(region = state) %>%
     mutate(heavycat_a = quantcut(heavy_a, q=4, na.rm=T),
            gender = ifelse(sexvar == "Female", "women", "men"),
            population = paste(black,gender,sep=" "))


# Create separate datasets for women and men
bw1 <- filter(table01.s.adj, sexvar == "Female")
bw2 <- filter(table02.s.adj, sexvar == "Female")
bw3 <- filter(table03.s.adj, sexvar == "Female")

bm1 <- filter(table01.s.adj, sexvar == "Male")
bm2 <- filter(table02.s.adj, sexvar == "Male")
bm3 <- filter(table03.s.adj, sexvar == "Male")

rm(table01.s.adj, table02.s.adj, table03.s.adj)

# A2. State polygon/map data ---------------------------------------------

library(maps)
library(ggplot2)

state_df <- map_data('state') 

# A3. Polygon centroids -------------------------------------------------

# NOTE: This code was adapted from an answer on StackOverflor available at stackoverflow.com/questions/9441436/ggplot-centered-names-on-a-map 

state_poly <- map("state", plot=FALSE, fill=TRUE)

state_centroids <- maps:::apply.polygon(state_poly, maps:::centroid.polygon)

# Don't think this line of code is doing anything in this case
state_centroids <- state_centroids[!is.na(names(state_centroids))]

centroid_array <- Reduce(rbind, state_centroids)

dimnames(centroid_array) <- list(gsub("[^,]*,", "",
                                      names(state_centroids)),c("long","lat"))

label_df <- as.data.frame(centroid_array)

label_df$state <- rownames(label_df)

label_df <- label_df %>%
     filter(state != "district of columbia") %>% 
     filter(state != "massachusetts:martha's vineyard") %>%
     filter(state != "massachusetts:nantucket") %>%
     filter(state != "michigan:north") %>%
     filter(state != "new york:manhattan") %>%
     filter(state != "new york:staten island") %>%
     filter(state != "new york:long island") %>%
     filter(state != "north carolina:knotts") %>%
     filter(state != "north carolina:spit") %>%
     filter(state != "virginia:chesapeake") %>%
     filter(state != "virginia:chincoteague") %>%
     filter(state != "washington:orcas island") %>%
     filter(state != "washington:san juan island") %>%
     filter(state != "washington:lopez island") %>%
     filter(state != "washington:whidbey island") 

rm(state_poly,state_centroids,centroid_array)

# Change the state names for certain states

label_df$state[label_df$state == "massachusetts:main"] <- "massachusetts"
label_df$state[label_df$state == "north carolina:main"] <- "north carolina"
label_df$state[label_df$state == "washington:main"] <- "washington"
label_df$state[label_df$state == "michigan:south"] <- "michigan"
label_df$state[label_df$state == "new york:main"] <- "new york"
label_df$state[label_df$state == "virginia:main"] <- "virginia"

# A4. State abbreviations  ----------------------------------------------

library(readr)

ansi <- read_delim("https://www2.census.gov/geo/docs/reference/state.txt", "|", escape_double = FALSE, trim_ws = TRUE)

ansi <- ansi %>%
     filter(STATE != '11') %>%
     filter(STATE != '60') %>%
     filter(STATE != '66') %>% 
     filter(STATE != '69') %>% 
     filter(STATE != '72') %>%
     filter(STATE != '74') %>%
     filter(STATE != '78') %>%
     mutate(state = tolower(STATE_NAME)) %>%
     rename(stusab = STUSAB) %>%
     select(state,stusab)

# A5. Merge datasets together for the two layers of the map -------------

# Merge state abbreviations with polygon centroids for map labels
label_df <- merge(label_df, ansi, by="state")

rm(ansi)

# B. Create maps of age-adjusted prevalence of alcohol use --------------

# Create palette
library(RColorBrewer)
my_OrRd <- brewer.pal(n=5, "OrRd")[2:5]

# Use fundtions in these packages to arrange plots side-by-side
library(grid)
library(gridExtra)

# Set working directory of where map figures willbe stored
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/manuscripts/figures")

# B1. Past 30-day age-adjusted alcohol use (current drinking) -----------


m1.bw <- ggplot(data=bw1) +
        geom_map(aes(map_id = state, fill=drnkany_cat_a), 
                 map=state_df, colour="black") +
        expand_limits(x = state_df$long, y = state_df$lat) +
        coord_map() +
        geom_text(data=label_df, aes(label=stusab,x=long,y=lat),
                  size=2, check_overlap = F) +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("34.5%-44.0%", "44.1%-48.4%",
                         "48.5%-53.0%","53.1%-64.0%",
                         "No Data")) +
        labs(title="Black women") +
        theme_void() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust=0.5))
              

m1.bm <- ggplot(data=bm1) +
        geom_map(aes(map_id = state, fill=drnkany_cat_a), 
                 map=state_df, colour="black") +
        expand_limits(x = state_df$long, y = state_df$lat) +
        coord_map() +
        geom_text(data=label_df, aes(label=stusab,x=long,y=lat),
                  size=2, check_overlap = F) +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("34.5%-44.0%", "44.1%-48.4%",
                         "48.5%-53.0%","53.1%-64.0%",
                         "No Data")) +
        labs(title="Black men") +
        theme_void() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust=0.5))

m1.leg <- ggplot(data=bw1) +
        geom_map(aes(map_id = state, fill=drnkany_cat_a), 
                 map=state_df, colour="black") +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("34.5%-44.0%", "44.1%-48.4%",
                         "48.5%-53.0%","53.1%-64.0%",
                         "No Data")) +
        theme(legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin = margin(4, 4, 4, 4))

legend.1 <- cowplot::get_legend(m1.leg)

# Save plots at high-resolution png files
# The legend and the maps will be combined in Microsoft Paint to make one figure
png("1_Maps_CurrentDrinking_AgeAdjusted_20210910.png", width = 7.7, height = 4, units = 'in', res = 300)

grid.arrange(m1.bw, m1.bm,nrow=1, ncol=2)

png("1_Legend_CurrentDrinking_AgeAdjusted_20210910.png", width = 7.7, height = 1, units = 'in', res = 300)

grid.newpage()
grid.draw(legend.1)

dev.off() 
graphics.off()



# B2. Past 30-day age-adjusted binge drinking ---------------------------

m2.bw <- ggplot(data=bw2) +
        geom_map(aes(map_id = state, fill=bingecat_a), 
                 map=state_df, colour="black") +
        expand_limits(x = state_df$long, y = state_df$lat) +
        coord_map() +
        geom_text(data=label_df, aes(label=stusab,x=long,y=lat),
                  size=2, check_overlap = F) +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("14.9%-24.4%", "24.5%-28.7%",
                         "28.8%-33.3%", "33.4%-51.0%",
                         "No Data")) +
        labs(title="Black women") +
        theme_void() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust=0.5))


m2.bm <- ggplot(data=bm2) +
        geom_map(aes(map_id = state, fill=bingecat_a), 
                 map=state_df, colour="black") +
        expand_limits(x = state_df$long, y = state_df$lat) +
        coord_map() +
        geom_text(data=label_df, aes(label=stusab,x=long,y=lat),
                  size=2, check_overlap = F) +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("14.9%-24.4%", "24.5%-28.7%",
                         "28.8%-33.3%", "33.4%-51.0%",
                         "No Data")) +
        labs(title="Black men") +
        theme_void() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust=0.5))


m2.leg <- ggplot(data=bw2) +
        geom_map(aes(map_id = state, fill=bingecat_a), 
                 map=state_df, colour="black") +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("14.9%-24.4%", "24.5%-28.7%",
                         "28.8%-33.3%", "33.4%-51.0%",
                         "No Data")) +
        theme(legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin = margin(4, 4, 4, 4))

legend.2 <- cowplot::get_legend(m2.leg)

png("2_Maps_BingeDrinking_AgeAdjusted_20210910.png", width = 7.7, height = 4, units = 'in', res = 300)

grid.arrange(m2.bw, m2.bm,nrow=1, ncol=2)

png("2_Legend_BingeDrinking_AgeAdjusted_20210910.png", width = 7.7, height = 1, units = 'in', res = 300)

grid.newpage()
grid.draw(legend.2)

dev.off() 
graphics.off()


# B3. Past 30-day age-adjusted heavy drinking ---------------------------

m3.bw <- ggplot(data=bw3) +
        geom_map(aes(map_id = state, fill=heavycat_a), 
                 map=state_df, colour="black") +
        expand_limits(x = state_df$long, y = state_df$lat) +
        coord_map() +
        geom_text(data=label_df, aes(label=stusab,x=long,y=lat),
                  size=2, check_overlap = F) +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("2.5%-6.5%", "6.6%-9.3%",
                         "9.4%-13.0%", "13.1%-25.7%",
                         "No Data")) +
        labs(title="Black women") +
        theme_void() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust=0.5))


m3.bm <- ggplot(data=bm3) +
        geom_map(aes(map_id = state, fill=heavycat_a), 
                 map=state_df, colour="black") +
        expand_limits(x = state_df$long, y = state_df$lat) +
        coord_map() +
        geom_text(data=label_df, aes(label=stusab,x=long,y=lat),
                  size=2, check_overlap = F) +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("2.5%-6.5%", "6.6%-9.3%",
                         "9.4%-13.0%", "13.1%-25.7%",
                         "No Data")) +
        labs(title="Black men") +
        theme_void() +
        theme(legend.position = "none") +
        theme(plot.title = element_text(hjust=0.5))


m3.leg <- ggplot(data=bw3) +
        geom_map(aes(map_id = state, fill=heavycat_a), 
                 map=state_df, colour="black") +
        scale_fill_manual(
                values=my_OrRd,
                aesthetics = "fill",
                na.value="white",
                name="Age-adjusted Prevalence",
                labels=c("2.5%-6.5%", "6.6%-9.3%",
                         "9.4%-13.0%", "13.1%-25.7%",
                         "No Data")) +
        theme(legend.direction="horizontal",
              legend.box.background=element_rect(),
              legend.box.margin = margin(4, 4, 4, 4))


legend.3 <- cowplot::get_legend(m3.leg)


png("3_Maps_HeavyDrinking_AgeAdjusted_20210910.png", width = 7.7, height = 4, units = 'in', res = 300)
 
grid.arrange(m3.bw, m3.bm,nrow=1, ncol=2)

png("3_Legend_HeavyDrinking_AgeAdjusted_20210910.png", width = 7.7, height = 1, units = 'in', res = 300)

grid.newpage()
grid.draw(legend.3)

dev.off() 
graphics.off()


# Clear environment
rm(list=ls())


# C. Male Excess Charts -------------------------------------------------
# C1. Data Management ---------------------------------------------------


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

library(readr)

ansi <- read_delim("https://www2.census.gov/geo/docs/reference/state.txt", "|", escape_double = FALSE, trim_ws = TRUE)

ansi <- ansi %>%
        filter(STATE != '11') %>%
        filter(STATE != '60') %>%
        filter(STATE != '66') %>% 
        filter(STATE != '69') %>% 
        filter(STATE != '72') %>%
        filter(STATE != '74') %>%
        filter(STATE != '78') %>%
        rename(stusab = STUSAB, state = STATE_NAME) %>%
        select(state,stusab)

# Merge table data with state abbreviations
table01.s.adj <- merge(ansi, table01.s.adj, by="state")
table02.s.adj <- merge(ansi, table02.s.adj, by="state")
table03.s.adj <- merge(ansi, table03.s.adj, by="state")



# C2: Calculate Male Excess  --------------------------------------------

# Past 30-day drinking (age-adjusted)
male.excess.01 <- table01.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar, sep="")) %>%
        select(state, stusab, population, n, drnkany_a, drnkany_low_a,
               drnkany_upp_a) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, drnkany_a, drnkany_low_a,
                                    drnkany_upp_a)) %>%
        mutate(male_excess = drnkany_a_BlackMale-drnkany_a_BlackFemale,
               male_exccess_low = 
                       drnkany_low_a_BlackMale-drnkany_low_a_BlackFemale,
               male_excess_upp = 
                       drnkany_upp_a_BlackMale-drnkany_upp_a_BlackFemale) %>%
        select(state, stusab, male_excess, male_exccess_low, male_excess_upp)

# Past 30-day binge drinking (age-adjusted)
male.excess.02 <- table02.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar, sep="")) %>%
        select(state, stusab, population, n, binge_a, binge_low_a,
               binge_upp_a) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, binge_a, binge_low_a,
                                    binge_upp_a)) %>%
        mutate(male_excess = binge_a_BlackMale-binge_a_BlackFemale,
               male_exccess_low = 
                       binge_low_a_BlackMale-binge_low_a_BlackFemale,
               male_excess_upp = 
                       binge_upp_a_BlackMale-binge_upp_a_BlackFemale) %>%
        select(state, stusab, male_excess, male_exccess_low, male_excess_upp)

# Past 30-day heavy drinking
male.excess.03 <- table03.s.adj %>%
        ungroup() %>%
        mutate(population = paste(black, sexvar, sep="")) %>%
        select(state, stusab, population, n, heavy_a, heavy_low_a,
               heavy_upp_a) %>%
        pivot_wider(names_from = population,
                    values_from = c(n, heavy_a, heavy_low_a,
                                    heavy_upp_a)) %>%
        mutate(male_excess = heavy_a_BlackMale-heavy_a_BlackFemale,
               male_exccess_low = 
                       heavy_low_a_BlackMale-heavy_low_a_BlackFemale,
               male_excess_upp = 
                       heavy_upp_a_BlackMale-heavy_upp_a_BlackFemale) %>%
        select(state, stusab, male_excess, male_exccess_low, male_excess_upp)


# C3. Print Male Excess Charts ------------------------------------------

# Set working directory of where map figures willbe stored
setwd("C:/Users/18166/Dropbox/JHSPH/Post Doc/Alcohol Use Black Pops/manuscripts/figures")


# Past 30-day alcohol use (current drinking)
me.plot.01 <- ggplot(data=filter(male.excess.01, !is.na(male_excess)),
                     aes(x=male_excess,y=reorder(stusab, male_excess))) +
        geom_col(colour='black',fill = 'white', width=0.75) +
        coord_cartesian(xlim = c(-5,30)) +
        geom_text(data=filter(male.excess.01, male_excess > 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = 1, nudge_y = 0.1) +
        geom_text(data=filter(male.excess.01, male_excess < 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = -1, nudge_y = 0.1)


png("ChartMaleExcess_CurrentDrinking_AgeAdjusted_20210913.png", width = 7, height = 5, units = 'in', res = 300)

me.plot.01 +
        labs(x="Age-adjusted Prevalence Difference in Past 30-day Drinking (%Male - %Female)",
             y="State") +
        theme_classic()


dev.off()
graphics.off()



# Past 30-day binge drinking (Binge Drinking)
me.plot.02 <- ggplot(data=filter(male.excess.02, !is.na(male_excess)),
                     aes(x=male_excess,y=reorder(stusab, male_excess))) +
        geom_col_pattern(width=0.75, 
                         pattern='stripe',fill='white',
                         color='black',
                         pattern_size=0.5) +
        coord_cartesian(xlim = c(-20,30)) +
        geom_text(data=filter(male.excess.02, male_excess > 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = 2) +
        geom_text(data=filter(male.excess.02, male_excess < 0),
                  aes(label=round(male_excess,1)), size=3,
                  nudge_x = -2)

png("ChartMaleExcess_BingeDrinking_AgeAdjusted_20210913.png", width = 7, height = 5, units = 'in', res = 300)

me.plot.02 +
        labs(x="Age-adjusted Prevalence Difference in Binge Drinking (%Male - %Female)",
             y="State") +
        theme_classic()

dev.off()
graphics.off()


# Past 30-day heavy drinking (Heavy Drinking)
me.plot.03 <- ggplot(data=filter(male.excess.03, !is.na(male_excess)),
                     aes(x=male_excess,y=reorder(stusab, male_excess))) +
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

png("ChartMaleExcess_HeavyDrinking_AgeAdjusted_20210913.png", width = 7, height = 5, units = 'in', res = 300)

me.plot.03 +
        labs(x="Age-adjusted Prevalence Difference in Heavy Drinking (%Male - %Female)",
             y="State") +
        theme_classic()

dev.off()
graphics.off()

# Clear working enviornment
rm(list=ls())

# D. Appendix Tables ------------------------------------------------------

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
# NOTE: The tables were opened as HTML Files and the information copied into a Word document for formatting

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



