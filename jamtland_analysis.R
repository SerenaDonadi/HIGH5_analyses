rm(list=ls())

dir.exists("//storage-dh.slu.se/home$/sedi0002/My Documents/Job/HIGH5")
setwd("G:/My Drive/HIGH5/HIGH5_data")


# Libraries ---------------------------------------------------------------

#library(tidyverse)
library(ggplot2)
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
#library(plyr)


#####
# Read Datasets
#####

# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("SERS_20250522_Jamtland_Filtered.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

jamtland <- read.csv2("SERS_20250522_Jamtland_Filtered.csv",fileEncoding="ISO-8859-1", header=TRUE, sep=";", dec=",") 
head(jamtland)

# check response variable:
summary(jamtland$Öring0)

# check values "-9":
jamtland %>% 
  filter(Öring0 == -9) 
# 2 records. maybe typos. Exlude them:
jamtland2 <- jamtland %>% 
  filter(Öring0 != -9)

# check spatial hierarchy:
unique(jamtland2$Län)
unique(jamtland2$Länskod)
unique(jamtland2$Haro_nr)
unique(jamtland2$Huvudavrinningsområde) #6 levels
unique(jamtland2$Hflodomr) #6
unique(jamtland2$Biflnr)
unique(jamtland2$Vattendrag ) #476
unique(jamtland2$Lokal)#687
unique(jamtland2$Lokalnr)#67

unique(jamtland2$Syfte)
unique(jamtland2$Vandhind)
unique(jamtland2$Typavpop)
unique(jamtland2$Lokalvar)
unique(jamtland2$Lokalvar)

table(jamtland2$VTYP_ED)
table(jamtland2$Typavpop)


head(jamtland)
# retain only variables of interest:
jamtland3 <- jamtland2 %>% 
  select(Öring0, 
         Hflodomr, 
         Vattendrag, 
         Lokal, 
         Langd, 
         XKOORLOK, 
         YKOORLOK,
         WGS84_Dec_N,
         WGS84_Dec_E,
         Fiskedatum,
         ÅR,
         MÅNAD,
         Bredd,
         Maxdjup,
         Medeldju,
         Substr1,
         Vattente,
         Beskuggn,
         Vandhind,
         VTYP_ED,
         Typavpop,
         Hoh,
         Avstupp,
         Avstner,
         mindistsj,
         LUTNING_PROM,
         MEDTEMPAR,
         MEDT_JULI,
         VIX, 
         VIX_klass)

head(jamtland3)

# explore hierarchical spatial and temporal structure.
# how many years per site? first and last year of sampling?
site_years<-jamtland3 %>% 
  group_by(Hflodomr,Vattendrag,Lokal) %>% 
  summarise(n_dinstic_years = n_distinct(ÅR),
            first_year = min(ÅR),
            last_year = max(ÅR)) %>% 
  arrange(desc(n_dinstic_years))

# select only sites with at least 10 years of sampling 
site_years10<-site_years %>%
  filter(n_dinstic_years > 9)

# check that there are no sites with the same name in different rivers
# Count how many unique rivers each site appears in:
jamtland3 %>%
  distinct(Lokal, Vattendrag) %>%         # Remove duplicate site-river pairs
  group_by(Lokal) %>%
  summarise(n_rivers = n()) %>%
  filter(n_rivers > 1)              # Keep only sites that appear in multiple rivers



# select a subset in jamtland3 whose sites match those in the site_years10:
jamtland3_time_series<-jamtland3 %>%
  filter(Lokal %in% unique(site_years10$Lokal) & Vattendrag %in% unique(site_years10$Vattendrag))

unique(site_years10$Lokal) # 
unique(jamtland3_time_series$Lokal) # 
unique(site_years10$Vattendrag) # 
unique(jamtland3_time_series$Vattendrag) # 

# exploratory plots
unique(jamtland3_time_series$Lokal) # 77 sites
unique(jamtland3_time_series$Vattendrag)
table(jamtland3_time_series$Vattendrag, jamtland3_time_series$Lokal) # 

ggplot(subset(jamtland3_time_series, Vattendrag %in% c("Abbåsån")), 
              aes(x = ÅR , y = Öring0)) +
  geom_point()+
  facet_wrap(~Lokal)+
  labs(title="Öring0")+
  theme_classic(base_size=13)

