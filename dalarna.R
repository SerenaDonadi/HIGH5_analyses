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


#### group variables by site: ####
# make a dataset with only the sites (one row per site), and bring along covaraites for later analyses

df.model.site_all_data <- all_data2 %>%
  group_by(site,Vattendrag,Hflodomr,Län,SD) %>%
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
# convert VTYP_ED_score into factor:
df.model.site_all_data2$VTYP_ED_score<-as.factor(df.model.site_all_data2$VTYP_ED_score)
table(df.model.site_all_data2$VTYP_ED_score)

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



### breakpoint analysis ####
# using the script from 19 aug 2025 of Katarina Magnusson

# calculating breakpoint values (hocky stick) 
# model selection (quadratic vs linear model) based on criteria and AIC-values
# for sites with no valid clx  -> site q90 fallback value
# Optional: remove q90 for poor sites eg, site mean < mean of ICES subdivision (SD)

# --------- dependencies ----------
#library(dplyr)
library(minpack.lm)
library(nlstools)

# --------- helpers ----------
# Return an (x, y) data.frame of the empirical CDF (cumulative density function) for numeric vector x
ecdf_df <- function(x) {
  x <- x[is.finite(x)] # drop NA/NaN/Inf values
  if (length(x) < 1) return(data.frame(x = numeric(0), y = numeric(0)))
  xs <- sort(unique(x))
  data.frame(x = xs, y = ecdf(x)(xs))
}

# Self-starting linear plateau (3 params)
#   y = a + b*x           for x < clx
#   y = a + b*clx         for x ≥ clx   (flat plateau at the cutoff clx)

if (!exists("SSlinp", mode = "function")) {
  SSlinp <- selfStart(
    
    # The model function used by nls(): piecewise linear with a hard plateau at clx
    function(x, a, b, clx) ifelse(x < clx, a + b * x, a + b * clx),
    
    # The initializer that guesses starting values for (a, b, clx)
    function(mCall, data, LHS) {
      
      # Build a clean (x, y) frame sorted by x from the model call and data
      xy <- stats::sortedXyData(mCall[["x"]], LHS, data)
      
      # Crude linear fit across all x to seed intercept (a) and slope (b)
      lm0 <- lm(y ~ x, xy)
      a   <- coef(lm0)[1]; b <- coef(lm0)[2]
      
      # Start clx at the median x (robust, usually near the "knee")
      clx <- stats::median(xy$x, na.rm = TRUE)
      
      # Return a named vector of initial values, with names aligned to the call
      v <- c(a = a, b = b, clx = clx); names(v) <- mCall[c("a","b","clx")]
      v
    },
    
    # Declare parameter names so nls() knows what to estimate
    parameters = c("a","b","clx")
  )
}

# Self-starting quadratic-to-plateau (3 params), zero slope at clx
# SSquadp3xs with parameter 'jp' for the breakpoint.
# Select either 'clx' or 'jp' when extracting coefs/CI.
if (!exists("SSquadp3xs", mode = "function")) {
  SSquadp3xs <- selfStart(
    function(x, a, b, clx) {
      c <- -b / (2 * clx)
      ifelse(x < clx, a + b * x + c * x^2, a + b * clx + c * clx^2)
    },
    function(mCall, data, LHS) {
      xy <- stats::sortedXyData(mCall[["x"]], LHS, data)
      lm2 <- try(lm(y ~ x + I(x^2), xy), silent = TRUE)
      if (inherits(lm2, "try-error")) {
        a0 <- mean(xy$y, na.rm = TRUE); b0 <- 0
        clx0 <- stats::median(xy$x, na.rm = TRUE)
      } else {
        a_hat  <- coef(lm2)[1]; b1_hat <- coef(lm2)[2]; b2_hat <- coef(lm2)[3]
        clx_v  <- if (is.finite(b2_hat) && abs(b2_hat) > .Machine$double.eps)
          -b1_hat / (2 * b2_hat) else stats::median(xy$x, na.rm = TRUE)
        clx0   <- min(max(clx_v, min(xy$x, na.rm = TRUE)), max(xy$x, na.rm = TRUE))
        a0     <- a_hat; b0 <- b1_hat
      }
      v <- c(a = a0, b = b0, clx = clx0); names(v) <- mCall[c("a","b","clx")]
      v
    },
    parameters = c("a","b","clx")
  )
}

# Extract a breakpoint parameter by name (supports 'clx' or 'jp')
.pick_par <- function(x, candidates = c("clx","jp")) {
  nm <- names(x)
  hit <- intersect(candidates, nm)
  if (length(hit)) unname(x[hit[1]]) else NA_real_
}

# Extract CI rows by name (supports 'clx' or 'jp')
.pick_ci <- function(ci, candidates = c("clx","jp")) {
  rn <- rownames(ci)
  hit <- intersect(candidates, rn)
  if (length(hit)) ci[hit[1], , drop = FALSE] else matrix(NA_real_, nrow = 1, ncol = 2,
                                                          dimnames = list("clx", c("2.5 %","97.5 %")))
}

# QC checker: evaluates three checks for a fitted cutoff (clx)
# - plateau_ok: curve reaches required fraction of asymptote (y_at ≥ min_plateau)
# - ci_ok: confidence interval on clx is reasonably tight and non-negative
# - in_middle: clx lies within an inner quantile band of x (to avoid edge fits)

qc_flags <- function(clx, ci_low, ci_high, y_at, x_vec,
                     min_plateau, max_rel_ci, x_inner) {
  
  # If the cutoff isn't a finite number, fail fast with clear flags
  if (!is.finite(clx)) {
    return(list(ok = FALSE, plateau_ok = FALSE, ci_ok = FALSE, in_middle = FALSE, rel_ci = NA_real_))
  }
  
  # Total spread (range length) of x; used to scale CI width
  xr <- diff(range(x_vec, na.rm = TRUE))
  
  # Relative CI width for clx: (upper - lower) / x-range
  # If CI bounds aren't both finite or x has no spread, set to NA
  rel_ci <- if (is.finite(ci_low) && is.finite(ci_high) && xr > 0) (ci_high - ci_low) / xr else NA_real_
  
  # CI check:
  # - If rel_ci is NA, treat as OK (don't fail due to missing CI)
  # - Otherwise require: relative width ≤ threshold AND lower bound not negative (if provided)
  ci_ok  <- if (is.na(rel_ci)) TRUE else (rel_ci <= max_rel_ci && (is.na(ci_low) || ci_low >= 0))
  
  # Inner quantile band of x (e.g., 5th–95th percentile)
  xq <- stats::quantile(x_vec, probs = x_inner, na.rm = TRUE)
  
  # Check that clx lies inside that inner band
  in_middle  <- clx >= xq[1] && clx <= xq[2]
  
  # Plateau check: y_at must be finite and at least min_plateau (e.g., 0.80)
  plateau_ok <- is.finite(y_at) && (y_at >= min_plateau)
  
  # Aggregate verdict and individual flags
  list(ok = (plateau_ok && ci_ok && in_middle),
       plateau_ok = plateau_ok, ci_ok = ci_ok, in_middle = in_middle, rel_ci = rel_ci)
}

# --------- main ----------
get_clx_all_methods_select_qc <- function(
    df, # data.frame input data (one row per observation/visit)
    
    # character vector of columns that uniquely define a site/series grouping (e.g., pass ID and site) 
    site_vars = c("Vdrag", "Lokal"), # I use XY as Lokal 
    
    # name of the response/metric column to model/select on (e.g., 0+ trout density per 100 m^2)
    density_var = "Trout0P",        
    
    # minimum number of rows in a group (e.g., years) required before attempting model fitting/selection
    min_points  = 8, # not needed if only sites with at least 10 years of data are used               
    
    # minimum number of distinct x-values needed to fit reliably
    min_unique  = 5,               
    
    # maximum iterations allowed for non-linear least squares optimizer (nls) before giving up 
    maxiter_nls = 400,              
    
    # QC thresholds
    # require the fitted curve to reach ≥ this fraction of its asymptote within observed range
    min_plateau = 0.80,            
    
    # maximum acceptable relative CI width for key estimates (CI_width / estimate ≤ 0.25) 
    max_rel_ci  = 0.25,            
    
    # inner quantile range of x kept for fitting/diagnostics to avoid edge effects/outliers
    x_inner = c(0.05, 0.95),     
    
    # fallback options
    fallback_q90_if = "high_mean",   # "never","always","high_mean"
    fallback_when   = "both",        # "no_fit","fail_keep","both","never"
    threshold_by    = NULL,          # e.g. "SD"
    thr_fun         = "mean",         # "mean" or "quantile"
    mean_threshold  = 0.75          # used if thr_fun == "quantile"
) {
  
  stopifnot(density_var %in% names(df))
  nls_ctrl <- nls.control(maxiter = maxiter_nls, warnOnly = TRUE)
  
  # ---------- per-site fits ----------
  site_results <- df %>%
    group_by(across(all_of(site_vars))) %>%
    group_modify(~{
      dens <- .x[[density_var]]
      edf  <- ecdf_df(dens)
      
      q90_val <- quantile(dens, 0.9, na.rm = TRUE)
      n_pts   <- length(dens) # number of data points
      n_uniq  <- length(unique(edf$x)) # number of unique Trout0P values in edf
      
      # defaults
      lin_clx <- lin_ci_low <- lin_ci_high <- lin_y_at <- NA_real_
      quad_clx <- quad_ci_low <- quad_ci_high <- quad_y_at <- NA_real_
      aic_lin <- aic_quad <- Inf
      
      if (n_pts >= min_points && n_uniq >= min_unique) {
        
        # Linear model
        lin_fit <- try(nls(y ~ SSlinp(x, a, b, clx), data = edf, control = nls_ctrl), silent = TRUE)
        if (inherits(lin_fit, "nls")) {
          co <- coef(lin_fit)
          lin_clx  <- .pick_par(co, c("clx","jp"))
          lin_y_at <- as.numeric(predict(lin_fit, newdata = data.frame(x = lin_clx)))
          ci_lin <- try(confint2(lin_fit), silent = TRUE)
          if (!inherits(ci_lin, "try-error")) {
            ci_row <- .pick_ci(ci_lin, c("clx","jp"))
            lin_ci_low <- ci_row[1,1]; lin_ci_high <- ci_row[1,2]
          }
          aic_lin <- AIC(lin_fit)
        }
        
        # Quadratic 3p model
        quad_fit <- try(nls(y ~ SSquadp3xs(x, a, b, jp), data = edf, control = nls_ctrl), silent = TRUE)
        if (!inherits(quad_fit, "nls")) {
          quad_fit <- try(nls(y ~ SSquadp3xs(x, a, b, clx), data = edf, control = nls_ctrl), silent = TRUE)
        }
        if (inherits(quad_fit, "nls")) {
          co <- coef(quad_fit)
          quad_clx  <- .pick_par(co, c("jp","clx"))
          quad_y_at <- as.numeric(predict(quad_fit, newdata = data.frame(x = quad_clx)))
          ci_q <- try(confint2(quad_fit), silent = TRUE)
          if (!inherits(ci_q, "try-error")) {
            ci_row <- .pick_ci(ci_q, c("jp","clx"))
            quad_ci_low <- ci_row[1,1]; quad_ci_high <- ci_row[1,2]
          }
          aic_quad <- AIC(quad_fit)
        }
      }
      
      # QC (quality control) for both candidates
      lin_qc  <- qc_flags(lin_clx,  lin_ci_low,  lin_ci_high,  lin_y_at,  edf$x,
                          min_plateau, max_rel_ci, x_inner)
      quad_qc <- qc_flags(quad_clx, quad_ci_low, quad_ci_high, quad_y_at, edf$x,
                          min_plateau, max_rel_ci, x_inner)
      
      # AIC selection with QC veto/switch (only use AIC if the model meet the critera)
      method_pref <- "no_fit"; clx_pref <- ci_low <- ci_high <- y_at <- NA_real_; keep <- FALSE
      if (is.finite(aic_lin) || is.finite(aic_quad)) {
        if (aic_quad < aic_lin) {
          if (quad_qc$ok) {
            method_pref <- "quadratic_plateau"
            clx_pref <- quad_clx; ci_low <- quad_ci_low; ci_high <- quad_ci_high; y_at <- quad_y_at
            keep <- TRUE
          } else if (lin_qc$ok) {
            method_pref <- "linear_plateau"
            clx_pref <- lin_clx; ci_low <- lin_ci_low; ci_high <- lin_ci_high; y_at <- lin_y_at
            keep <- TRUE
          } else {
            method_pref <- "quadratic_plateau"
            clx_pref <- quad_clx; ci_low <- quad_ci_low; ci_high <- quad_ci_high; y_at <- quad_y_at
            keep <- FALSE
          }
        } else {
          if (lin_qc$ok) {
            method_pref <- "linear_plateau"
            clx_pref <- lin_clx; ci_low <- lin_ci_low; ci_high <- lin_ci_high; y_at <- lin_y_at
            keep <- TRUE
          } else if (quad_qc$ok) {
            method_pref <- "quadratic_plateau"
            clx_pref <- quad_clx; ci_low <- quad_ci_low; ci_high <- quad_ci_high; y_at <- quad_y_at
            keep <- TRUE
          } else {
            method_pref <- "linear_plateau"
            clx_pref <- lin_clx; ci_low <- lin_ci_low; ci_high <- lin_ci_high; y_at <- lin_y_at
            keep <- FALSE
          }
        }
      }
      
      tibble(
        q90 = q90_val,
        n_points = n_pts, n_unique = n_uniq,
        # per-model estimates and QC
        lin_clx = lin_clx, lin_ci_low = lin_ci_low, lin_ci_high = lin_ci_high,
        lin_y_at = lin_y_at, aic_linear = aic_lin,
        quad_clx = quad_clx, quad_ci_low = quad_ci_low, quad_ci_high = quad_ci_high,
        quad_y_at = quad_y_at, aic_quadratic = aic_quad,
        lin_plateau_ok = lin_qc$plateau_ok, lin_ci_ok = lin_qc$ci_ok, lin_in_middle = lin_qc$in_middle,
        quad_plateau_ok = quad_qc$plateau_ok, quad_ci_ok = quad_qc$ci_ok, quad_in_middle = quad_qc$in_middle,
        # chosen
        clx_pref = clx_pref, ci_low = ci_low, ci_high = ci_high, y_at_clx = y_at,
        method_pref = method_pref, keep = keep
      )
    }) %>%
    ungroup()
  
  # ---------- carry threshold_by into site_results & compute site means ----------
  if (!is.null(threshold_by) && threshold_by %in% names(df)) {
    key_map <- df %>%
      distinct(across(all_of(c(site_vars, threshold_by))))
    site_results <- site_results %>%
      left_join(key_map, by = site_vars)
  }
  
  site_means <- df %>%
    group_by(across(all_of(site_vars))) %>%
    summarise(mean_density = mean(get(density_var), na.rm = TRUE), .groups = "drop")
  
  site_results <- site_results %>%
    left_join(site_means, by = site_vars)
  
  # ---------- thresholds (per group or global, from site-level means) ----------
  if (!is.null(threshold_by) && threshold_by %in% names(site_results)) {
    thr_tbl <- site_results %>%
      group_by(.data[[threshold_by]]) %>%
      summarise(
        thr_val = if (thr_fun == "mean") {
          mean(mean_density, na.rm = TRUE)
        } else if (thr_fun == "quantile") {
          stats::quantile(mean_density, probs = mean_threshold, na.rm = TRUE, names = FALSE)
        } else {
          stop("thr_fun must be 'mean' or 'quantile'")
        },
        .groups = "drop"
      )
    site_results <- site_results %>%
      left_join(thr_tbl, by = threshold_by)
  } else {
    thr_val_global <- if (thr_fun == "mean") {
      mean(site_results$mean_density, na.rm = TRUE)
    } else {
      stats::quantile(site_results$mean_density, probs = mean_threshold, na.rm = TRUE, names = FALSE)
    }
    site_results$thr_val <- thr_val_global
  }
  
  # ---------- fallback (q90) ----------
  need_fallback <- function(keep, method_pref, when) {
    (when == "no_fit"    && method_pref == "no_fit") ||
      (when == "fail_keep" && !isTRUE(keep)) ||
      (when == "both"      && (method_pref == "no_fit" || !isTRUE(keep)))
  }
  
  out <- site_results %>%
    mutate(
      need_fb = mapply(need_fallback, keep, method_pref, MoreArgs = list(when = fallback_when)),
      allow_fb = case_when(
        fallback_q90_if == "always" ~ TRUE,
        fallback_q90_if == "high_mean" ~ is.finite(mean_density) & is.finite(thr_val) & (mean_density >= thr_val),
        TRUE ~ FALSE
      ),
      use_fallback = need_fb & allow_fb,
      clx_final    = ifelse(use_fallback, q90, clx_pref),
      method_final = ifelse(use_fallback, paste0("q90_fallback_", fallback_q90_if), method_pref),
      fallback_used = use_fallback,
      keep_final   = keep | use_fallback,
      usable       = keep_final
    )
  
  return(out)
}



### RUN MODEL ON DATA 

# select samples collected in july-oct, exclude missing values of trout density 
# and -9, and keep only sites with at least 10 year of data
summer_data <- all_data2 %>% 
  #filter(Län == "Dalarna")%>%
  #filter(Year>1989) %>% 
  filter(MÅNAD>6 & MÅNAD<11) %>% # 
  filter(!is.na(Öring0)) %>% # 
  filter(Öring0 != -9) %>% # 
  group_by(site) %>% 
  filter(n() > 9)

is.data.frame(summer_data)


### 1) using avg density of trout per catchment as threshold value for the area
df_summer_data1 <- as.data.frame(summer_data[,c("site","Hflodomr","Vattendrag","Öring0")])
colnames(df_summer_data1) <- c("Lokal","Hflodomr","Vdrag","Trout0P")

results.clx_all_data <- get_clx_all_methods_select_qc(df_summer_data1,  
                                             # Fallback options:
                                             fallback_when   = "both",  # when fallback should be used if no clx-value: "no_fit","fail_keep","both","never"
                                             fallback_q90_if = "always", # can also specify if poor sites should be excluded: "never","always","high_mean"
                                             threshold_by="Hflodomr",  # area for threshold values (example ICES subdivion "SD")
                                             thr_fun = "mean", # threshold value for area "mean" or "quantile"
                                             mean_threshold = 0.75,  # specifying quantile if thr_fun="quantile"
)

# 2) ### using avg density of trout per SD as threshold value for the area

df_summer_data2 <- as.data.frame(summer_data[,c("site","SD","Vattendrag","Öring0")])
colnames(df_summer_data2) <- c("Lokal","SD","Vdrag","Trout0P")

results.clx2_all_data <- get_clx_all_methods_select_qc(df_summer_data2,  
                                              # Fallback options:
                                              fallback_when   = "both",  # when fallback should be used if no clx-value: "no_fit","fail_keep","both","never"
                                              fallback_q90_if = "always", # can also specify if poor sites should be excluded: "never","always","high_mean"
                                              threshold_by="SD",  # area for threshold values (example ICES subdivion "SD")
                                              thr_fun = "mean", # threshold value for area "mean" or "quantile"
                                              mean_threshold = 0.75,  # specifying quantile if thr_fun="quantile"
)

# plot to check differences between the two methods: breakpoints, "keep final" and "usable" are the same
plot(results.clx_all_data$clx_final,results.clx2_all_data$clx_final) #


#### plots breakpoints script ####
# script from Katarina Magnusson 27 aug 2025

# PLOT function
plot_clx_site <- function(raw_df, site_row,
                          density_var = "Trout0P",
                          site_vars   = "Lokal",
                          nls_maxiter = 400) {
  
  # --- checks ---
  for (nm in site_vars) {
    if (!nm %in% names(raw_df)) stop(sprintf("raw_df is missing column '%s'", nm))
    if (!nm %in% names(site_row)) stop(sprintf("site_row is missing column '%s'", nm))
  }
  if (!density_var %in% names(raw_df)) stop(sprintf("raw_df is missing density_var '%s'", density_var))
  
  # --- subset raw data for this site ---
  cond <- rep(TRUE, nrow(raw_df))
  for (nm in site_vars) cond <- cond & (raw_df[[nm]] == site_row[[nm]][1])
  df <- raw_df[cond, , drop = FALSE]
  if (!nrow(df)) { plot.new(); title(main = "No data for site"); return(invisible(NULL)) }
  
  # --- ECDF ---
  ecdf_df <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 1) return(data.frame(x = numeric(0), y = numeric(0)))
    xs <- sort(unique(x))
    data.frame(x = xs, y = ecdf(x)(xs))
  }
  edf <- ecdf_df(df[[density_var]])
  xvec <- edf$x; yvec <- edf$y
  if (!length(xvec)) { plot.new(); title(main = "No finite densities"); return(invisible(NULL)) }
  
  # --- title: "Vdrag / Lokal" if Vdrag exists, else just Lokal ---
  title_str <- if ("Vdrag" %in% names(raw_df) && "Vdrag" %in% names(site_row)) {
    paste0(site_row[["Vdrag"]][1], "\n", site_row[["Lokal"]][1])
  } else {
    as.character(site_row[["Lokal"]][1])
  }
  
  plot(xvec, yvec,
       xlab = "Density 0+",
       ylab = "Cumulative probability",
       ylim = c(0, 1),
       main = title_str,
       cex.main = 0.9, cex = 0.9, cex.lab = 0.9, cex.axis = 0.9)
  
  # --- q90 ---
  q90_here <- site_row$q90
  if (!is.finite(q90_here)) q90_here <- tryCatch(quantile(df[[density_var]], 0.9, na.rm = TRUE), error = function(e) NA_real_)
  if (is.finite(q90_here)) abline(v = q90_here, lty = 3, lwd = 1, col = "blue")
  
  # --- clx + CI (only for plateau methods that were kept/used) ---
  clx_final <- site_row$clx_final
  ci_low    <- site_row$ci_low
  ci_high   <- site_row$ci_high
  method    <- as.character(site_row$method_final)
  
  if (isTRUE(site_row$usable) && grepl("plateau", method)) {
    if (is.finite(ci_low) && is.finite(ci_high)) {
      rect(xleft = ci_low, xright = ci_high,
           ybottom = par("usr")[3], ytop = par("usr")[4],
           col = rgb(1, 0, 0, 0.2), border = NA)
    }
    if (is.finite(clx_final)) abline(v = clx_final, lty = 2, lwd = 1.2, col = "red")
  }
  
  # --- annotation ---
  ann <- if (isFALSE(site_row$keep_final)) {
    "no fit"
  } else {
    paste0(
      "q90 = ", ifelse(is.finite(q90_here), round(q90_here, 0), "NA"), "\n",
      "clx = ", ifelse(is.finite(clx_final), round(clx_final, 0), "NA"),
      if (is.finite(ci_low) && is.finite(ci_high))
        paste0(" (", round(ci_low, 0), "–", round(ci_high, 0), ")") else "",
      "\n", method
    )
  }
  text(max(xvec, na.rm = TRUE), 0.1, ann, pos = 2, cex = 0.7)
}


#### choose a SUBSAMPLE to plot ####

# the total dataset has 2157 sites with at least 10 year sampling!

# select sites to plot (df.model.site2 contains one row per site, so here we select sites/rows 1:16 for plotting)
site.start <- 1 # first site for plot
df.model.sub_all_data <- summer_data[site.start:(site.start+15),] # plot 16 sites
# select sites 17:32
site.start <- 17 # first site for plot
df.model.sub_all_data <- summer_data[site.start:(site.start+15),] # plot 16 sites
# osv

# Pick clx and q90 values by for selected sites from results.clx
pick <- as.data.frame(subset(results.clx_all_data, Lokal %in% df.model.sub$Lokal))

# run plot function on df1 
par(mfrow = c(4,4), mar = c(2,2,2,2))
for (i in seq_len(nrow(pick))) {
  plot_clx_site(raw_df = df_summer_data2, site_row = pick[i,], density_var = "Trout0P", 
                site_vars = "Lokal" # Lokal is XY here
  )
}
#par(mfrow = c(1,1))


##### merge breakpoints and site level variables ####

# I merge here results coming from the model where SD was used as threshold 
# (and not catchment. However breakpoints are the same, maybe flags are different)
# OBS number of replicate are diufferent bc breakpoints dataset has only those sites
# with at least 10 year of sampling! So I get a lot of NAs
all_sites_all_data<-merge(df.model.site_all_data2, results.clx2_all_data, by = c("Lokal","Vdrag", "SD"), all = T, sort = F) # 
summary(all_sites_all_data)
table(all_sites_all_data$method_final)
table(all_sites_all_data$fallback_used)

# create a variable showing whether the mean trout density per site is above/below 
# the avg at SD - or catchment. Here is avg SD doublecheck: ok
table(all_sites_all_data$thr_val,all_sites_all_data$SD)
table(all_sites_all_data$thr_val,all_sites_all_data$Hflodomr)

plot(all_sites_all_data$mean_density, all_sites_all_data$thr_val)
abline(a = 0, b = 1, col = "red", lty = 2)
all_sites_all_data$good_or_bad <- ifelse(all_sites_all_data$mean_density - all_sites_all_data$thr_val >= 0, "good", "bad")
table(all_sites_all_data$good_or_bad)
table(all_sites_all_data$method_final,all_sites_all_data$good_or_bad)

###### exploratory plots (sites as replicates)####
ggplot(all_sites_all_data, aes(x=Lokal, y=clx_final, col = Vdrag, fill=Vdrag)) +
  geom_bar(stat="identity")+
  #facet_wrap(~Vdrag)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

ggplot(all_sites_all_data, aes(x = mean_width , y = clx_final, col=VTYP_ED_score)) +
  geom_point()+
  theme_bw(base_size=15)

ggplot(all_sites_all_data, aes(x = mean_mindistsj , y = clx_final, col=VTYP_ED_score, size=Vandhind_score)) +
  geom_point()+
  theme_bw(base_size=15)

ggplot(all_sites_all_data, aes(x = mean_VIX, y = clx_final)) +
  geom_point()+
  theme_bw(base_size=15)

#####
# validation of Jämtaland model in Dalarna
#####
# using Dalarna data:
# 1) calculate breakpoints on sites with at least 10 years of samplig
# 2) on this same subset, calculate predicted breakpoints using jämtland model
# 3) compare the two

unique(all_sites_all_data$Län)

# select samples in dalarna: 
dalarna <- all_sites_all_data %>% 
  filter(Län == "Dalarna")

summary(dalarna)
# 2315 sites, 2186 NAs for breakpoints -> 129 sites with breakpoints

#### using models including all possible factors and info on site ####
# (with or without catchment variables, they are not important)

### predictions of pruned model are:
dalarna %>% 
  filter(!is.na(clx_final)) %>% # to get teh correct N used in the calculation of the mean
  filter(mean_density<19.27)%>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 10 vs jamtland predictions of 10
# 115 vs 74 sites

dalarna %>% 
  filter(!is.na(clx_final)) %>%
  filter(mean_density>=19.27)%>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 57 vs jamtland predictions of 66
# 14 vs 10 sites

### predictions of not pruned model are:
dalarna %>% 
  filter(!is.na(clx_final)) %>%
  filter(mean_density<7.112)%>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 5 vs jamtland predictions of 5
# 80 vs 52 sites

dalarna %>% 
  filter(!is.na(clx_final)) %>%
  filter(mean_density<19.27)%>%
  filter (mean_density>= 7.112) %>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 21 vs jamtland predictions of 21
# 35 vs 22 sites

dalarna %>% 
  filter(!is.na(clx_final)) %>%
  filter(mean_density>=19.27)%>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 57 vs jamtland predictions of 66
# 14 vs 10 sites

### using instead surrogate mean_avgdepth
dalarna %>% 
  filter(!is.na(clx_final)) %>%
  filter(mean_avgdepth<0.1119792)%>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 16 vs jamtland predictions of 66
# 10 vs 10 sites

dalarna %>% 
  filter(!is.na(clx_final)) %>%
  filter(mean_avgdepth>=0.1119792)%>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 15 vs jamtland predictions of 10
# 119 vs 64 sites

# using instead surrogate Substr1_fac 
# approximate! jämtland model wants Substr1_fac split as  LLLLLRLL, but in 
# jämtland there is little grus, and no sand or häll. 
# I split as with or without sten + grus + sand:

dalarna %>% 
  filter(!is.na(clx_final)) %>%
  distinct(Substr1_fac)

dalarna %>%
  filter(!is.na(clx_final)) %>%
  filter(Substr1_fac %in% c("Block","Block1","Block2","Block3")) %>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 13 vs jamtland predictions of 10
# 72 vs 74 sites

dalarna %>%
  filter(!is.na(clx_final)) %>%
  filter(!(Substr1_fac %in% c("Block","Block1","Block2","Block3"))) %>%
  summarise(n_sites = n(),
            avg_clx = mean(clx_final, na.rm = TRUE))
# 17 vs jamtland predictions of 66
# 57 vs 10 sites

