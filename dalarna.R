rm(list=ls())

# prioritize your user library but still keep access to system packages
.libPaths(c("C:/NoBackup/sedi/R/x86_64-w64-mingw32-library/4.5",
            "C:/Program Files/R/R-4.5.1/library"))


dir.exists("C:/RprojectsSerena/HIGH5/HIGH5_data")
setwd("C:/RprojectsSerena/HIGH5/HIGH5_data")

#### To save the R environment ####
# press the save button in the "Environment" window here on the right.Or (but noit sure it works as well), run this script at the end of your session:
save.image(file = "my_environment.RData")
# When you reopen your project, you can restore the environment by clicking on that file in the "Files" window here on the bottom
# right, or maybe by using this script (not sure it works well):
load("my_environment.RData")
# Remember to save your script files separately, as the .RData file only contains the objects in your environment, not the code itself.

# Libraries ---------------------------------------------------------------

#library(tidyverse)
library(ggplot2)
#library(plyr)
library(dplyr)
#library(tidyr)
library(gplots)
library(lattice)
library(nlme)
# library(MASS) # potenital name clash problem for function select in dplyr
library(piecewiseSEM)
library(lme4)
library(car)
library(visreg)
library(mgcv)
library(ggeffects)

library(ExcelFunctionsR)

#####
# Read Datasets
#####

# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
#library(readr)
#guess_encoding("SERS_20250522_Jamtland_Filtered.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

all_data <- read.csv2("SERS_20250522_All.csv",fileEncoding="ISO-8859-1", header=TRUE, sep=";", dec=",") 
head(all_data)


##### fix variables and subsets to calculate breakpoints####

# make a column with the coordinates as a character string, and use it as site name:
all_data1 <- all_data %>%
  mutate(site = paste(XKOORLOK, YKOORLOK, sep = "_"))




# substitute values "-9" in Beskuggn with NA:
all_data1$Beskuggn[all_data1$Beskuggn == -9] <- NA

# substitute " " and "?" values in Vandhind with NA: 
all_data1$Vandhind[all_data1$Vandhind == " "] <- NA
all_data1$Vandhind[all_data1$Vandhind == "?"] <- NA

# transform Vandhind in a numeric variable? no need if values per site did not change over the years. Check, after removing NA:
all_data1 %>%
  filter(!is.na(Vandhind)) %>%
  group_by(site) %>%
  summarise(n_unique_vandhind = n_distinct(Vandhind)) %>%
  filter(n_unique_vandhind > 1)
# I'd like to see which combinations of Vandhind values appear together in the same site:
all_data1 %>%
  filter(!is.na(Vandhind)) %>%
  group_by(site) %>%
  summarise(vandhind_values = paste(sort(unique(Vandhind)), collapse = ", ")) %>%
  distinct(vandhind_values) %>%
  arrange(vandhind_values)
# there are 16 combinations 
# create a new variable with 0 for sites where Vand hind was always Inga, 1 if Ned 
# but not Upp Or Båda was found at least once, 1.5 if both Ned and Upp were found,
# and 0.5 if only Upp and inga was recorded:

# count values of "Vandhind" by site:
vandhind_freq_all_data1<-all_data1 %>%
  #filter(!is.na(Vandhind)) %>%
  group_by(site, Vandhind) %>%
  summarise(count = n()) %>%
  arrange(site, Vandhind)

# make a table with the counts:
vandhind_table_all_data1 <- tidyr::pivot_wider(vandhind_freq_all_data1, names_from = Vandhind, values_from = count, values_fill = 0)

# Load necessary library
library(readxl)

# Define a function to classify Vandhind combinations
classify_vandhind_all_data1 <- function(values) {
  values <- unique(na.omit(trimws(values)))
  
  # Case 1: All values are "Inga"
  if (setequal(values, "Inga")) {
    return(0)
    
    # Case 2: Values are "Ned" and "Inga"
  } else if (setequal(values, c("Ned", "Inga"))) {
    return(1)
    
    # Case 2b: Only "Ned"
  } else if (setequal(values, "Ned")) {
    return(1)
    
    # Case 3: Contains both "Ned" and "Upp", or contains "Både"
  } else if ("Både" %in% values || all(c("Ned", "Upp") %in% values)) {
    return(1.5)
    
    # Case 4: Contains "Upp" but not "Ned" or "Både"
  } else if ("Upp" %in% values && !("Ned" %in% values) && !("Både" %in% values)) {
    return(0.5)
    
    # Any other combination
  } else {
    return(NA)
  }
}

# Apply the classification per site
vandhind_scores_all_data1 <- all_data1 %>%
  group_by(site) %>%
  summarise(Vandhind_score = classify_vandhind_all_data1(Vandhind)) %>%
  ungroup()

# Merge later to  data by site! 

# check VTYP_ED and Typavpop
table(all_data1$VTYP_ED)
unique(all_data1$VTYP_ED)
table(all_data1$Typavpop)
unique(all_data1$Typavpop)

# substitute " " values in VTYP_ED and Typavpop with NA: 
all_data1$VTYP_ED[all_data1$VTYP_ED == " "] <- NA
all_data1$Typavpop[all_data1$Typavpop == " "] <- NA

# did it change over the years for the same site? yes
all_data1 %>%
  filter(!is.na(VTYP_ED)) %>%
  group_by(site) %>%
  summarise(n_unique_VTYP_ED = n_distinct(VTYP_ED)) %>%
  filter(n_unique_VTYP_ED > 1)
# I'd like to see which combinations of VTYP_ED values appear together in the same site:
all_data1 %>%
  filter(!is.na(VTYP_ED)) %>%
  group_by(site) %>%
  summarise(VTYP_EDvalues = paste(sort(unique(VTYP_ED)), collapse = ", ")) %>%
  distinct(VTYP_EDvalues) %>%
  arrange(VTYP_EDvalues)

# to be on the safe site I could make two binary numeric variables, one for each of this variables and see if they differ
# however, there are more NAs in Typavpop than VTYP_ED, so this latter is my favorite

# make a binary variable for VTYP_ED where 1 is assigned if at values "Insjö" or "Hav" have been found 
# at least once in that site, and 0 if only "Ström" has been found

# Define a function to classify VTYP_ED combinations
classify_VTYP_ED <- function(values) {
  values <- unique(na.omit(trimws(values)))
  
  if (all(values == "Ström")) {
    return(0)
  } else {
    return(1)
  }
}

# Apply the classification per site
VTYP_ED_scores_all_data1 <- all_data1 %>%
  group_by(site) %>%
  summarise(VTYP_ED_score = classify_VTYP_ED(VTYP_ED)) %>%
  ungroup()
# Merge later to  data by site!




#####
# work flow
#####
# using Dalarna data:
# 1) calculate breakpoints on sites with at least 10 years of samplig
# 2) on this same subset, calculate predcited breakpoint using jämtland model
# 3) compare the two


# select samples in dalarna, collected in july-oct, exclude missing values of trout
# density and -9, and keep only sites with at least 10 year of data
dalarna <- jamtland1 %>% 
  filter(Län = "Dalarna")%>%
  #filter(Year>1989) %>% 
  filter(MÅNAD>6 & MÅNAD<11) %>% # removed only 8 obs
  filter(!is.na(Öring0)) %>% # there is none
  filter(Öring0 != -9) %>% # remove records with -9
  group_by(site) %>% 
  filter(n() > 9)