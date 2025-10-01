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

# check Vattenha
unique(all_data1$Vattenha)
table(all_data1$Vattenha)
# substitute " " values in Vandhind with NA: 
all_data1$Vattenha[all_data1$Vattenha == " "] <- NA
# transform Vattenha in a numeric variable? no need if values per site did not change over the years. Check, after removing NA:
all_data1 %>%
  filter(!is.na(Vattenha)) %>%
  group_by(site) %>%
  summarise(n_unique_Vattenha = n_distinct(Vattenha)) %>%
  filter(n_unique_Vattenha > 1)
# I'd like to see which combinations of Vattenha values appear together in the same site:
all_data1 %>%
  filter(!is.na(Vattenha)) %>%
  group_by(site) %>%
  summarise(Vattenha_values = paste(sort(unique(Vattenha)), collapse = ", ")) %>%
  distinct(Vattenha_values) %>%
  arrange(Vattenha_values)
# there are 7 combinations

# count values of "Vattenha" by site:
Vattenha_freq_all_data1<-all_data1 %>%
  filter(!is.na(Vattenha)) %>%
  group_by(site, Vattenha) %>%
  summarise(count = n()) %>%
  arrange(site, Vattenha)

# make a table with the counts:
Vattenha_table_all_data1 <- tidyr::pivot_wider(Vattenha_freq_all_data1, names_from = Vattenha, values_from = count, values_fill = 0)
# the majority seem to have most obs with one values and few with another. But there are exceptions. 
# 1) I can take the one which is more frequent and merge later 
names(Vattenha_table_all_data1)
Vattenha_table_all_data1$Vattenha_fac <- colnames(Vattenha_table_all_data1[, c(2:4)])[
  max.col(Vattenha_table_all_data1[, c(2:4)], ties.method = "random")]
# To merge later with site level data!

# transform in numeric variable and get the avg later
# 1=Lugn, 2=strömmande, 3=stråkande
# copy vector:
all_data1$Vattenha_num<-all_data1$Vattenha
all_data1$Vattenha_num[all_data1$Vattenha_num == "Lugn"] <- 1
all_data1$Vattenha_num[all_data1$Vattenha_num == "Strö"] <- 2
all_data1$Vattenha_num[all_data1$Vattenha_num == "Strå"] <- 3
unique(all_data1$Vattenha_num)
table(all_data1$Vattenha,all_data1$Vattenha_num)
all_data1$Vattenha_num<-as.numeric(all_data1$Vattenha_num)

# check substrate:
# check Substr1
table(all_data1$Substr1)
unique(all_data1$Substr1)
# slagg????
# substitute " " values in Vandhind with NA: 
all_data1$Substr1[all_data1$Substr1 == " "] <- NA
# substitute "slagg" values in Vandhind with NA until I know more:
all_data1$Substr1[all_data1$Substr1 == "Slagg"] <- NA

# did it change over the years for the same site? yes
all_data1 %>%
  filter(!is.na(Substr1)) %>%
  group_by(site) %>%
  summarise(n_unique_Substr1 = n_distinct(Substr1)) %>%
  filter(n_unique_Substr1 > 1)
# I'd like to see which combinations of substr values appear together in the same site:
all_data1 %>%
  filter(!is.na(Substr1)) %>%
  group_by(site) %>%
  summarise(Substr1values = paste(sort(unique(Substr1)), collapse = ", ")) %>%
  distinct(Substr1values) %>%
  arrange(Substr1values)

# count values of "Substr1" by site:
Substr1_freq_all_data1<-all_data1 %>%
  filter(!is.na(Substr1)) %>%
  group_by(site, Substr1) %>%
  summarise(count = n()) %>%
  arrange(site, Substr1)

# make a table with the counts:
Substr1_table_all_data1 <- tidyr::pivot_wider(Substr1_freq_all_data1, names_from = Substr1, values_from = count, values_fill = 0)
# 1) I can take the one which is more frequent and merge later 
names(Substr1_table_all_data1)
Substr1_table_all_data1$Substr1_fac <- colnames(Substr1_table_all_data1[, c(2:6,8:12)])[
  max.col(Substr1_table_all_data1[, c(2:12)], ties.method = "random")]
# To merge later with site level data!

# 2) I can use an old numeric conversion from SERS(o Erik) and take later the avg by sites:
# 1=Fin 2=sand 3=Grus 4=sten1+Sten2 5=block1+2+3 6=Häll 
all_data1$Substr1_num<-all_data1$Substr1
all_data1$Substr1_num[all_data1$Substr1_num == " "] <- NA
all_data1$Substr1_num[all_data1$Substr1_num == "Fin"] <- 1
all_data1$Substr1_num[all_data1$Substr1_num == "Sand"] <- 2
all_data1$Substr1_num[all_data1$Substr1_num == "Grus"] <- 3
all_data1$Substr1_num[all_data1$Substr1_num %in% c("Sten1", "Sten2", "Sten")] <- 4
all_data1$Substr1_num[all_data1$Substr1_num %in% c("Block1", "Block2","Block3","Block")] <- 5
all_data1$Substr1_num[all_data1$Substr1_num == "Häll"] <- 6
unique(all_data1$Substr1_num)
table(all_data1$Substr1,all_data1$Substr1_num)
all_data1$Substr1_num<-as.numeric(all_data1$Substr1_num)

# retain only variables of interest:
all_data2 <- all_data1 %>% 
  select(Öring0,Län,Hflodomr, Vattendrag,site,Lokal,XKOORLOK, YKOORLOK,WGS84_Dec_N,WGS84_Dec_E,
         Fiskedatum,ÅR,MÅNAD,Bredd, Maxdjup,Medeldju,Substr1,Substr1_num,Vattenha,
         Vattenha_num,Vattente,Beskuggn,Vandhind,VTYP_ED,Typavpop,Hoh,Avstupp,
         Avstner, mindistsj,LUTNING_PROM,MEDTEMPAR, MEDT_JULI,VIX,VIX_klass)
head(all_data2)


# explore hierarchical spatial and temporal structure. (NON NEED TO RUN THE SCRIPT FOR ANALYSES)
# how many years per site? first and last year of sampling?
site_years_all_data2<-all_data2 %>% 
  group_by(Hflodomr,Vattendrag,site) %>% 
  summarise(n_dinstic_years = n_distinct(ÅR),
            first_year = min(ÅR),
            last_year = max(ÅR)) %>% 
  arrange(desc(n_dinstic_years))

#### group variables by site: ####
# make a dataset with only the sites (one row per site), and bring along covaraites for later analyses

df.model.site_all_data <- all_data2 %>%
  group_by(site,Vattendrag,Hflodomr,Län) %>%
  summarise(n_years = n(), 
            mean_trout0 = mean(Öring0, na.rm = TRUE),
            XKOORLOK = mean(XKOORLOK, na.rm = TRUE),
            YKOORLOK = mean(YKOORLOK, na.rm = TRUE),
            WGS84_Dec_N = mean(WGS84_Dec_N, na.rm = TRUE),
            WGS84_Dec_E = mean(WGS84_Dec_E, na.rm = TRUE),
            mean_width = mean(Bredd, na.rm = TRUE),
            mean_maxdepth = mean(Maxdjup, na.rm = TRUE),
            mean_avgdepth = mean(Medeldju, na.rm = TRUE),
            mean_Substr1_num = mean(Substr1_num, na.rm = TRUE),
            mean_Vattenha_num = mean(Vattenha_num, na.rm = TRUE),
            mean_watertemp = mean(Vattente, na.rm = TRUE),
            mean_shade = mean(Beskuggn, na.rm = TRUE),
            mean_Hoh = mean(Hoh, na.rm = TRUE),
            mean_Avstupp = mean(Avstupp, na.rm = TRUE),
            mean_Avstner = mean(Avstner, na.rm = TRUE),
            mean_mindistsj = mean(mindistsj, na.rm = TRUE),
            mean_LUTNING_PROM = mean(LUTNING_PROM, na.rm = TRUE),
            mean_MEDTEMPAR = mean(MEDTEMPAR, na.rm = TRUE),
            mean_MEDT_JULI = mean(MEDT_JULI, na.rm = TRUE),
            mean_VIX = mean(VIX, na.rm = TRUE),
            mean_VIX_klass= mean(VIX_klass, na.rm = TRUE)) %>%
  arrange(site)

# MERGE WITH VANDHIND SCORES AND VTYP_ED SCORES, and vatten and substrate tables
df.model.site_all_data0 <- left_join(df.model.site_all_data, vandhind_scores_all_data1, by = "site")
df.model.site_all_data1 <- left_join(df.model.site_all_data0, VTYP_ED_scores_all_data1, by = "site")
df.model.site_all_data1a <- left_join(df.model.site_all_data1, Substr1_table_all_data1, by = "site")
df.model.site_all_data2 <- left_join(df.model.site_all_data1a, Vattenha_table_all_data1, by = "site")
colnames(df.model.site_all_data2)
# remove column not needed:
df.model.site_all_data2<-df.model.site_all_data2 %>%
  select(-c(Block1 ,Block2 ,Block3 ,Block,Sten1,Sten2,Sten, Grus,Fin, Sand, Häll, Strå,Strö,Lugn)) 

# change names for merging later:
colnames(df.model.site_all_data2)[which(names(df.model.site_all_data2) == "mean_trout0")] <- "Trout0P"
colnames(df.model.site_all_data2)[which(names(df.model.site_all_data2) == "site")] <- "Lokal"
colnames(df.model.site_all_data2)[which(names(df.model.site_all_data2) == "Vattendrag")] <- "Vdrag"

#exploratory plots (all obs, i.e. site*year, as replicate)
ggplot(subset(all_data2, Vattendrag %in% c("Aapuajoki")),
       aes(x = ÅR , y = Öring0)) +
  geom_point()+
  facet_wrap(~site)+
  labs(title="")+
  theme_classic(base_size=13)


#### to divide the data in into ICES subdivisons based on HFLODOMR ####

#### using KM script to assign ICES subdivision based on HFLODOMR2

library(plyr)
all_data2$HFLODOMR2<-all_data2$Hflodomr;unique(all_data2$HFLODOMR2)
all_data2$HFLODOMR2<-as.character(all_data2$HFLODOMR2);unique(all_data2$HFLODOMR2)
all_data2$HFLODOMR2 <- revalue(all_data2$HFLODOMR2,
                               c("301000" = "301","89090" = "89.5","88089" = "88.5","92093"="92.5","93094"="93.5","87088"="87.5","94095"="94.5","86087"="86.5","81082"="81.5",
                                 "79080"="79.5","84085"="84.5","80081"= "80.5","96097"="96.5","78079"="78.5","101102"="101.5","76077"="76.5","75076"="75.5",
                                 "102103"="102.5","103104"="103.5","117118"="117.5","74075"="74.5","73074"="73.5","104105"="104.5","105106"="105.5",
                                 "118117"="118.5", # Gotland särskilj 118117 från 117118
                                 "72073"="72.5","107108"="107.5","71072"="71.5","70071"="70.5","108109"="108.5","69070"="69.5",
                                 "68069"="68.5","109110" ="109.5","67068"="67.5","110111"="110.5","66067"="66.5","63064"="63.5","62063"="62.5","111112"="111.5",
                                 #"15016"="15.5",
                                 #"112113"="112.5",
                                 "61062"="61.5" ,"60061"="60.5",
                                 "59060"="59.5","57058" ="57.5","56057"="56.5","54055"="54.5","53054"="53.5","52053"="52.5","50051" ="50.5","48049" ="48.5","47048"="47.5",
                                 "46047"="46.5","45046"="45.5","44045"="44.5","43044"="43.5","42043"="42.5","41042"="41.5","40041"="40.5", "39040"="39.5", 
                                 "38039"="38.5" ,"37038"="37.5","35036"="35.5",
                                 "34035"="34.5","33034"="33.5","32033"="32.5","30031"="30.5","29030"="29.5","28029"="28.5","114115"="114.5",
                                 "26027"="26.5","22023"="22.5","21022"="21.5",
                                 "19020"="19.5","18019"="18.5","17018"="17.5","16017"="16.5","13016"="13.5","2003"="2.5","23024"="23.5"));unique(all_data2$HFLODOMR2)
all_data2$HFLODOMR2<-as.numeric(all_data2$HFLODOMR2);unique(all_data2$HFLODOMR2)


all_data2$SD<-NA
# SD 31
all_data2$SD[all_data2$HFLODOMR2 <= 30]<-"SD31" # Bottenviken # Sept 2020: förut 28 -> 30
# SD 30
all_data2$SD[all_data2$HFLODOMR2 > 30 & all_data2$HFLODOMR2 <= 54.5]<-"SD30" # Bottenhavet # aug 2022: before 55
# SD 29
all_data2$SD[(all_data2$HFLODOMR2 > 54.5 & all_data2$HFLODOMR2 <= 59)]<-"SD29" # aug 2022: ändrat 55->54.5
#59. Norrtäljeån, 58. Bröströmmen
# SD 28 (East of Gotland below)
all_data2$SD[(all_data2$HFLODOMR2 == 117 | all_data2$HFLODOMR2 == 117.5)]<-"SD28"
# SD 27 (gränsar mot SD 29, SD 28 och SD 25)
all_data2$SD[(all_data2$HFLODOMR2 > 59 & all_data2$HFLODOMR2 <= 78) # 78. Hagbyån
             | (all_data2$HFLODOMR2 >117.5 &  all_data2$HFLODOMR2 <=119) ] <-"SD27" # Öland 119
# SD 28 (East Gotland) (aug 2022)
all_data2$SD[all_data2$HFLODOMR2==118.5 &
               all_data2$VDRAGNAM %in% c("Ajkesån","Hyluån","Hauån","Hultungsån","Gothemsån","Vikeån","Vägumeån","Bångån","Lergravsbäcken","Anerån","Vike kanal")]<-"SD28"
# 79. Bruatorpsån, till mellan 88-89 (närmare 89 än 88). 88. Helgeå, 89. Nybroån
# SD 25
all_data2$SD[all_data2$HFLODOMR2 > 78 & all_data2$HFLODOMR2 < 89]<-"SD25" # ungefär
# 89. Nybroån till innan Sege å
# SD 24
all_data2$SD[all_data2$HFLODOMR2 >= 89 & all_data2$HFLODOMR2 < 90]<-"SD24" # ungefär
# 90. Sege å  94. Råån
# SD 23
all_data2$SD[all_data2$HFLODOMR2 >= 90 & all_data2$HFLODOMR2 < 94.5]<-"SD23" # Aug 2022: Korrigerat från 95
# 95. Vege å till 107. Kungsbackaån
# SD 21
all_data2$SD[all_data2$HFLODOMR2 >= 94.5 & all_data2$HFLODOMR2 <= 107]<-"SD21"
# 107. Kungsbackaån till 116.
# SD 20
all_data2$SD[all_data2$HFLODOMR2 > 107 & all_data2$HFLODOMR2 <= 113]<-"SD20"
# Norge (aug 2022)
all_data2$SD[(all_data2$HFLODOMR2 > 112 & all_data2$HFLODOMR2 <= 116) | all_data2$HFLODOMR2== 301]<-"Norway" # Aug 2022: Norge

# unload plyr:
detach("package:plyr", unload=TRUE)
detach("package:ExcelFunctionsR", unload=TRUE)






#####
# work flow
#####
# using Dalarna data:
# 1) calculate breakpoints on sites with at least 10 years of samplig
# 2) on this same subset, calculate predicted breakpoint using jämtland model
# 3) compare the two


# select samples in dalarna, collected in july-oct, exclude missing values of trout
# density and -9, and keep only sites with at least 10 year of data
dalarna <- all_data2 %>% 
  filter(Län == "Dalarna")%>%
  #filter(Year>1989) %>% 
  filter(MÅNAD>6 & MÅNAD<11) %>% # 
  filter(!is.na(Öring0)) %>% # 
  filter(Öring0 != -9) %>% # 
  group_by(site) %>% 
  filter(n() > 9)

unique(dalarna$site) # 129 sites
