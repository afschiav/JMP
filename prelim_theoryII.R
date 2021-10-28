####LIBRARIES####
library(readr)
library(dplyr)
library(tidyr)
library(texreg)
library(stargazer)
library("ipumsr")
library(readxl)
library(usmap)
library(ggplot2)
library(mclust)
library(stringr)
library(broom)
library(lavaan)
library(semPlot)
library(xtable)
library(ivreg)
library(corrplot)

#map stuff
library(usmap)
library(maps)
library(maptools)
library(rgeos)

if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

#Regression libraries
library("lmtest")
library("sandwich")
library(plm)
library(systemfit)

######################################DATA FORMATTING#############################################
#####IMPORT BEA DATA####

#import & trim
COMP <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/bea_data/CAINC6N__ALL_AREAS_2001_2019.csv")
COMP<-COMP %>%
  select(starts_with("GeoFIP") | starts_with("IndustryClass") | starts_with("2")) %>%
  filter(nchar(IndustryClassification)==2 | substr(IndustryClassification,3,3)=="-")

PERSONAL_INC <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/bea_data/CAINC5N__ALL_AREAS_2001_2019.csv")
PERSONAL_INC<-PERSONAL_INC %>%
  select(starts_with("GeoFIP") | starts_with("IndustryClass") | starts_with("2")) %>%
  filter(nchar(IndustryClassification)==2 | substr(IndustryClassification,3,3)=="-")


GDP <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/bea_data/CAGDP2__ALL_AREAS_2001_2019.csv")
GDP<-GDP %>% 
  select(starts_with("GeoFIP") | starts_with("IndustryClass") | starts_with("2")) %>%
  filter(nchar(IndustryClassification)==2 | substr(IndustryClassification,3,3)=="-")


RGDP <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/bea_data/CAGDP9__ALL_AREAS_2001_2019.csv")
RGDP<-RGDP %>%
  select(starts_with("GeoFIP") | starts_with("IndustryClass") | starts_with("2")) %>%
  filter(nchar(IndustryClassification)==2 | substr(IndustryClassification,3,3)=="-")


EMPL <- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/bea_data/CAEMP25N__ALL_AREAS_2001_2019.csv")
EMPL<-EMPL %>% 
  select(starts_with("GeoFIP") | starts_with("IndustryClass") | starts_with("2")) %>%
  filter(nchar(IndustryClassification)==2 | substr(IndustryClassification,3,3)=="-")


#wide->long
COMP<-gather(COMP, year, COMP, `2001`:`2019`)
PERSONAL_INC<-gather(PERSONAL_INC, year, PERSONAL_INC, `2001`:`2019`)
GDP<-gather(GDP, year, GDP, `2001`:`2019`)
RGDP<-gather(RGDP, year, RGDP, `2001`:`2019`)
EMPL<-gather(EMPL, year, EMPL, `2001`:`2019`)

BEA<-merge(COMP, GDP, by=c("GeoFIPS", "year", "IndustryClassification"))
BEA<-merge(BEA, PERSONAL_INC, by=c("GeoFIPS", "year", "IndustryClassification"))
BEA<-merge(BEA, RGDP, by=c("GeoFIPS", "year", "IndustryClassification"))
BEA<-merge(BEA, EMPL, by=c("GeoFIPS", "year", "IndustryClassification"))
rm(COMP,PERSONAL_INC,GDP,RGDP, EMPL)

#remove US observations:
BEA<-BEA %>%
  filter(GeoFIPS!="00000")

#####ADD UNION DATA TO BEA####

#format union data
union_membership <- read_excel("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/union_data/State_Union_Membership_Density_1964-2020.xlsx")
union_membership<-gather(union_membership, year, membership, `%Mem20`:`%Mem64`)
union_membership<-union_membership %>%
  mutate(year=substr(year,5,6))%>%
  filter(as.numeric(year)<60) %>%
  mutate(year=as.numeric(paste("20",year, sep="")),
         state_fips=fips(`State Name`)) %>%
  rename(union_memb=membership)

#merge with county_complete data:
BEA$state_fips<-substr(BEA$GeoFIPS,1,2)
BEA<-merge(BEA, union_membership, by=c("state_fips","year") )
rm(union_membership)


#####ADD MIN WAGE DATA (INACTIVE)####
 # min_wage_state <- read_excel("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/min_wage_data/VZ_state_annual.xlsx")
 # min_wage_state$state_fips<-fips((min_wage_state$Name))
 # min_wage_state <- min_wage_state %>% rename(annual_state_minimum=annnual_state_minimum)
 # BEA<-merge(BEA, min_wage_state, by.x=c("state_fips","year"), by.y=c("state_fips","Year") )
 # rm(min_wage_state)

#####ADD UNEMPL. DATA ####
county_unempl <- read_excel("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/unempl_data/county_unempl_complete.xlsx")

county_unempl<-county_unempl %>%
  mutate(var=substr(series_id, 19,20),
         seasonal=substr(series_id,3,3),
         GeoFIPS=substr(series_id,6,10)) %>%
  filter(var=="03") %>%
  select(starts_with("year")| starts_with("value") | starts_with("GeoFIPS")) %>%
  rename(unempl_rate=value)

BEA<-merge(BEA, county_unempl, by.x=c("GeoFIPS","year"), by.y=c("GeoFIPS","year") )
rm(county_unempl)


#####ADD CONCENTRATION & ESTAB DATA TO BEA####

#IMPORT CBP employment data:
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/cbp_data")


cbp2002_complete<-read.delim("Cbp02co.txt", sep=",")
cbp2002_complete$year<-2002

cbp2003_complete<-read.delim("Cbp03co.txt", sep=",")
cbp2003_complete$year<-2003

cbp2004_complete<-read.delim("Cbp04co.txt", sep=",")
cbp2004_complete$year<-2004

cbp2005_complete<-read.delim("Cbp05co.txt", sep=",")
cbp2005_complete$year<-2005

cbp2006_complete<-read.delim("Cbp06co.txt", sep=",")
cbp2006_complete$year<-2006

cbp2007_complete<-read.delim("Cbp07co.txt", sep=",")
cbp2007_complete$year<-2007

cbp2008_complete<-read.delim("Cbp08co.txt", sep=",")
cbp2008_complete$year<-2008

cbp2009_complete<-read.delim("Cbp09co.txt", sep=",")
cbp2009_complete$year<-2009

cbp2010_complete<-read.delim("Cbp10co.txt", sep=",")
cbp2010_complete$year<-2010

cbp2011_complete<-read.delim("Cbp11co.txt", sep=",")
cbp2011_complete$year<-2011

cbp2012_complete<-read.delim("cbp12co.txt", sep=",")
cbp2012_complete$year<-2012

cbp2013_complete<-read.delim("Cbp13co.txt", sep=",")
cbp2013_complete$year<-2013

cbp2015_complete<-read.delim("Cbp15co.txt", sep=",")
cbp2015_complete$year<-2015
colnames(cbp2015_complete)<-tolower(colnames(cbp2015_complete))

cbp2016_complete<-read.delim("Cbp16co.txt", sep=",")
cbp2016_complete$year<-2016

cbp2017_complete<-read.delim("cbp17co.txt", sep=",")
cbp2017_complete$year<-2017
cbp2017_complete <- cbp2017_complete %>%
  rename(n1_4=n.5) %>%
  mutate_at(vars(n1_4:n1000_4), as.numeric) %>% 
  mutate_at(vars(n1_4:n1000_4),~replace(., is.na(.), 0))

cbp2018_complete<-read.delim("cbp18co.txt", sep=",")
cbp2018_complete$year<-2018
cbp2018_complete <- cbp2018_complete %>%
  rename(n1_4=n.5) %>%
  mutate_at(vars(n1_4:n1000_4), as.numeric) %>% 
  mutate_at(vars(n1_4:n1000_4),~replace(., is.na(.), 0))

cbp2019_complete<-read.delim("cbp19co.txt", sep=",")
cbp2019_complete$year<-2019
cbp2019_complete <- cbp2019_complete %>%
  rename(n1_4=n.5) %>%
  mutate_at(vars(n1_4:n1000_4), as.numeric) %>% 
  mutate_at(vars(n1_4:n1000_4),~replace(., is.na(.), 0))


CBP<-list(cbp2002_complete, cbp2003_complete, cbp2004_complete, cbp2005_complete, cbp2006_complete,
          cbp2007_complete, cbp2008_complete, cbp2009_complete, cbp2010_complete, cbp2011_complete,
          cbp2012_complete, cbp2013_complete, cbp2015_complete, cbp2016_complete, cbp2017_complete,
          cbp2018_complete, cbp2019_complete) %>%
  lapply(., function(x){
    x<-x %>%
      mutate(naics=as.numeric(gsub('-','',gsub('/','',naics))),
             GeoFIPS=paste(sprintf("%02d", fipstate), sprintf("%03d", fipscty), sep="" )) %>%
      select(starts_with("Geo") | starts_with("naics") | ends_with("emp") | ends_with("year") | starts_with("est") | starts_with("n"))
  }) %>%
  do.call(rbind, .)
rm(cbp2002_complete, cbp2003_complete, cbp2004_complete, cbp2005_complete, cbp2006_complete,
   cbp2007_complete, cbp2008_complete, cbp2009_complete, cbp2010_complete, cbp2011_complete,
   cbp2012_complete, cbp2013_complete, cbp2015_complete, cbp2016_complete, cbp2017_complete,
   cbp2018_complete, cbp2019_complete)

#Save employment vector for import exposure data (using 4-digit naics):
empl_vector<-CBP %>% filter(naics>999 & naics<10000) %>% select(GeoFIPS, naics, emp, year)

#Filter only 3-digit naics (highest granularity without omission bias)
CBP<-CBP %>%
  filter(naics>99 & naics<1000)

#code all cells with values <3 as zero (2017 does this, need to make this true for 2002-2012)
CBP<- CBP %>%
  mutate(emp=as.double(emp),
         est=as.double(est)) %>%
  mutate_at(vars(est:n1000_4),funs(case_when(.<3 ~ 0,.>=3~ .)) ) %>%
  mutate(est=rowSums(across(n1_4:n1000_4), na.rm = TRUE))


#FORMAT TO 2-digit NAICS:
CBP$naics<-substr(CBP$naics, 1,2)
CBP$naics[CBP$naics %in% c("31","32", "33")]<-"31-33"
CBP$naics[CBP$naics %in% c("44","45")]<-"44-45"
CBP$naics[CBP$naics %in% c("48","49")]<-"48-49"


CBP<-CBP %>% group_by(naics, GeoFIPS, year) %>%
  summarize_at(c("est", "emp", "n1_4", "n5_9", "n10_19", "n20_49", "n50_99", "n100_249", "n250_499", "n500_999","n1000_1", "n1000_2","n1000_3","n1000_4" ), sum, na.rm=TRUE) %>%
  ungroup()


#MERGE WITH BEA:
BEA<-merge(BEA, CBP, by.x=c("GeoFIPS", "year", "IndustryClassification"), by.y=c("GeoFIPS", "year", "naics"), all.x=TRUE)
rm(IND_CONC, industry_factors)

 

#####ADD IMPORT EXPOSURE DATA####

setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/import_data/BLS")


#Import value added data
va = list.files(pattern="NOMINAL_USE")
va = lapply(va, read.csv)
va = lapply(va, function(x){
  x<-x %>% filter(SECTORNUMBER==206) %>%
    t(.) %>% #transpose to get rows of industry value added
    data.frame(SECTORNUMBER = row.names(.), .) %>%
    filter(SECTORNUMBER!="SECTORNUMBER")
})

#add year data
for(i in 1:length(va)){va[[i]]$year<-i+2001}

#bind data:
va_df<-va %>% do.call(rbind,.) %>% rename(value_added=".")
va_df$SECTORNUMBER<-as.numeric(sub("X","",va_df$SECTORNUMBER))
rm(va)


#Import import data
imports = list.files(pattern="NOMINAL_FDAGG")
imports = lapply(imports, read.csv)
imports = lapply(imports, function(x){
  x<-x %>% select(SECTORNUMBER, X8)
})

#add year data
for(i in 1:length(imports)){imports[[i]]$year<-i+2001}

#bind data:
import_df<-imports %>% do.call(rbind,.) %>% rename(import=X8)
rm(imports)

#create import exposure df:
import_exposure<-merge(va_df, import_df, by=c("SECTORNUMBER", "year"))
rm(va_df, import_df)

#attach crosswalk:
crosswalk <- read_excel("SectorPlan312.xlsx")
crosswalk <- crosswalk %>% 
  rename(SECTORNUMBER=`Sector Number`,
         NAICS=`2017 North American Industry Classification System (NAICS)` )
import_exposure<-merge(import_exposure, crosswalk, by=c("SECTORNUMBER"))
rm(crosswalk)

#compute import exposure:
import_exposure$import_exposure<-(abs(import_exposure$import))/import_exposure$value_added

#merge with employment vector:
import_exposure<-merge(import_exposure %>% select(year, NAICS, import_exposure), empl_vector, by.x=c("NAICS", "year"), by.y=c("naics", "year"))

#FORMAT TO 2-digit NAICS:
import_exposure$naics_twoD<-substr(import_exposure$NAICS, 1,2)
import_exposure$naics_twoD[import_exposure$naics_twoD %in% c("31","32", "33")]<-"31-33"
import_exposure$naics_twoD[import_exposure$naics_twoD %in% c("44","45")]<-"44-45"
import_exposure$naics_twoD[import_exposure$naics_twoD %in% c("48","49")]<-"48-49"

import_exposure <- import_exposure %>% group_by(naics_twoD, year, GeoFIPS) %>%
  mutate(lambda=emp/(sum(emp, na.rm = TRUE)),
         weighted_import_exposure=lambda*import_exposure) %>%
  summarize(import_exposure=sum(weighted_import_exposure, na.rm = TRUE)) %>%
  ungroup()


BEA<-merge(BEA, import_exposure, by.x=c("GeoFIPS", "year", "IndustryClassification"), by.y=c("GeoFIPS", "year", "naics_twoD"), all.x=TRUE)



#####ADD CAPITAL COST DATA ####
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/capital_cost_data")

#import historical and current net stock data:
historical_cost <- read_csv("historical_cost.csv",skip = 7)
current_cost <- read_csv("current_cost.csv", skip = 7)

#wrangle
historical_cost <- historical_cost %>%
  rename(naics=X3, description=X2) %>%
  mutate(naics=as.numeric(substr(naics, 4,6))) %>%
  filter(!is.na(naics)) %>%
  select(!starts_with("19") & !starts_with("Line")) %>%
  select(starts_with("naics") | (!ends_with("00") & !starts_with("naics") ))

current_cost <- current_cost %>%
  rename(naics=X3, description=X2) %>%
  mutate(naics=as.numeric(substr(naics, 4,6))) %>%
  filter(!is.na(naics)) %>%
  select(!starts_with("19") & !starts_with("Line")) %>%
  select(starts_with("naics") | (!ends_with("00") & !starts_with("naics") ))

#merge:
price_indexes<-merge(historical_cost, current_cost, by="naics", suffix=c("_hist", "_current"))
rm(historical_cost, current_cost)

#compute price indexes:
price_indexes <- price_indexes %>%
  mutate(`2001`=`2001_current`/`2001_hist`,
         `2002`=`2002_current`/`2002_hist`,
         `2003`=`2003_current`/`2003_hist`,
         `2004`=`2004_current`/`2004_hist`,
         `2005`=`2005_current`/`2005_hist`,
         `2006`=`2006_current`/`2006_hist`,
         `2007`=`2007_current`/`2007_hist`,
         `2008`=`2008_current`/`2008_hist`,
         `2009`=`2009_current`/`2009_hist`,
         `2010`=`2010_current`/`2010_hist`,
         `2011`=`2011_current`/`2011_hist`,
         `2012`=`2012_current`/`2012_hist`,
         `2013`=`2013_current`/`2013_hist`,
         `2014`=`2014_current`/`2014_hist`,
         `2015`=`2015_current`/`2015_hist`,
         `2016`=`2016_current`/`2016_hist`,
         `2017`=`2017_current`/`2017_hist`,
         `2018`=`2018_current`/`2018_hist`,
         `2019`=`2019_current`/`2019_hist`
         ) %>%
  select(!contains("_"))

#to long:
price_indexes<-gather(price_indexes,year, capital_cost_index, `2001`:`2019`)

#FORMAT TO 2-digit NAICS:
price_indexes$naics<-substr(price_indexes$naics, 1,2)
price_indexes$naics[price_indexes$naics %in% c("31","32", "33")]<-"31-33"
price_indexes$naics[price_indexes$naics %in% c("44","45")]<-"44-45"
price_indexes$naics[price_indexes$naics %in% c("48","49")]<-"48-49"
price_indexes<-price_indexes %>%
  group_by(naics, year) %>%
  summarize_all(., mean, na.rm=TRUE)

#MERGE WITH BEA:
BEA<-merge(BEA, price_indexes,by.x=c("year", "IndustryClassification"), by.y=c("year", "naics"), all.x=TRUE )
rm(CBP, price_indexes)



####Transform from BEA -> CZ_panel####

#https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/documentation/
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/commuting_data")
CZ_crosswalk <- read_excel("cz00_eqv_v1.xls")

#trim:
CZ_crosswalk <- CZ_crosswalk %>%
  select((ends_with("2000") & contains("Commuting")) | starts_with("FIPS")) %>%
  rename(CZ_pop=`Commuting Zone Population 2000`,
         CZ_ID=`Commuting Zone ID, 2000`)

#merge with BEA data:
BEA<-merge(BEA, CZ_crosswalk, by.x=c("GeoFIPS"), by.y=c("FIPS"))

#aggregate by industry:
CZ_panel<-BEA %>%
  group_by(year, CZ_ID, IndustryClassification) %>%
  summarize(
    COMP=sum(as.numeric(COMP), na.rm = TRUE),
    PERSONAL_INC=sum(as.numeric(PERSONAL_INC), na.rm = TRUE),
    GDP=sum(as.numeric(GDP), na.rm = TRUE),
    RGDP=sum(as.numeric(RGDP), na.rm = TRUE),
    EMPL=sum(as.numeric(EMPL), na.rm = TRUE),
    emp=sum(as.numeric(emp), na.rm = TRUE),
    est=sum(est, na.rm = TRUE),
    union_memb=mean(union_memb, na.rm = TRUE),
    import_exposure=mean(import_exposure, na.rm = TRUE),
    unempl_rate=mean(unempl_rate, na.rm = TRUE),
    n1_4=sum(n1_4, na.rm = TRUE),
    n5_9=sum(n5_9, na.rm = TRUE),
    n10_19=sum(n10_19, na.rm = TRUE),
    n20_49=sum(n20_49, na.rm = TRUE),
    n50_99=sum(n50_99, na.rm = TRUE),
    n100_249=sum(n100_249, na.rm = TRUE),
    n250_499=sum(n250_499, na.rm = TRUE),
    n500_999=sum(n500_999, na.rm = TRUE),
    n1000_1=sum(n1000_1, na.rm = TRUE),
    n1000_2=sum(n1000_2, na.rm = TRUE),
    n1000_3=sum(n1000_3, na.rm = TRUE),
    n1000_4=sum(n1000_4, na.rm = TRUE),
    capital_cost_index=mean(capital_cost_index, na.rm = TRUE))


#create aggregate sectors:
CZ_panel$IndustryClassification[CZ_panel$IndustryClassification %in% c("42", "44-45")]<-"42-45"
CZ_panel$IndustryClassification[CZ_panel$IndustryClassification %in% c("54", "55", "56")]<-"54-56"
CZ_panel$IndustryClassification[CZ_panel$IndustryClassification %in% c("61", "62")]<-"61-62"
CZ_panel$IndustryClassification[CZ_panel$IndustryClassification %in% c("71", "72", "81")]<-"71-81"

CZ_panel<-CZ_panel %>%
  group_by(year, CZ_ID, IndustryClassification) %>%
  summarize(
    COMP=sum(as.numeric(COMP), na.rm = TRUE),
    PERSONAL_INC=sum(as.numeric(PERSONAL_INC), na.rm = TRUE),
    GDP=sum(as.numeric(GDP), na.rm = TRUE),
    RGDP=sum(as.numeric(RGDP), na.rm = TRUE),
    EMPL=sum(as.numeric(EMPL), na.rm = TRUE),
    emp=sum(as.numeric(emp), na.rm = TRUE),
    est=sum(est, na.rm = TRUE),
    union_memb=mean(union_memb, na.rm = TRUE),
    unempl_rate=mean(unempl_rate, na.rm = TRUE),
    import_exposure=mean(import_exposure, na.rm = TRUE),
    n1_4=sum(n1_4, na.rm = TRUE),
    n5_9=sum(n5_9, na.rm = TRUE),
    n10_19=sum(n10_19, na.rm = TRUE),
    n20_49=sum(n20_49, na.rm = TRUE),
    n50_99=sum(n50_99, na.rm = TRUE),
    n100_249=sum(n100_249, na.rm = TRUE),
    n250_499=sum(n250_499, na.rm = TRUE),
    n500_999=sum(n500_999, na.rm = TRUE),
    n1000_1=sum(n1000_1, na.rm = TRUE),
    n1000_2=sum(n1000_2, na.rm = TRUE),
    n1000_3=sum(n1000_3, na.rm = TRUE),
    n1000_4=sum(n1000_4, na.rm = TRUE),
  #  W_4_largest=mean(W_4_largest, na.rm = TRUE),
   # W_8_largest=mean(W_8_largest, na.rm = TRUE),
   # W_20_largest=mean(W_20_largest, na.rm = TRUE),
  #  W_50_largest=mean(W_50_largest, na.rm = TRUE),
    capital_cost_index=mean(capital_cost_index, na.rm = TRUE)
  )




#create additional variables
CZ_panel<-CZ_panel %>%
  ungroup() %>%
  group_by(CZ_ID, year) %>%
  mutate(EMPL_SHARE=EMPL/sum(EMPL, na.rm=TRUE),
         VA_SHARE=GDP/sum(GDP, na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(PAYROLL_SHARE=COMP/(GDP-(PERSONAL_INC-COMP)),
         LABOR_SHARE=COMP/GDP,
         est_per_worker=est/EMPL,
         HHI_labor=(n1_4*(4/EMPL)^2+n5_9*(9/EMPL)^2+n10_19*(19/EMPL)^2+n20_49*(49/EMPL)^2+n50_99*(99/EMPL)^2+n100_249*(249/EMPL)^2+n250_499*(499/EMPL)^2+n500_999*(999/EMPL)^2+n1000_1*(1499/EMPL)^2+n1000_2*(2499/EMPL)^2+n1000_3*(4999/EMPL)^2+n1000_4*(5000/EMPL)^2)
  )


#######################################FIGURES#################################################
####STYLIZED FACTS AND FIGURES ####

#TABLE 2 (summary_stats):
CZ_panel %>% group_by(sector,year) %>% mutate(x=(COMP)/(GDP)) %>% select(x) %>% summarize(median_psi=median(x, na.rm = TRUE))
CZ_panel_split %>% group_by(year) %>% mutate(x=(COMP_INDR)/(GDP_INDR)) %>% select(x) %>% summarize(median_psi=median(x, na.rm = TRUE))


#FIG: delta_psi_diff.png
CZ_panel_split %>%
  mutate(PSI_prog=COMP_prog/GDP_prog, delta=EMPL_prog/EMPL_stag) %>%
  filter(year==2002 | year==2017) %>% 
  group_by(CZ_ID) %>%
  arrange(., year, .group_by=TRUE ) %>%
  mutate(Delta_delta=delta-dplyr::lag(delta), 
         Delta_psi=PSI_prog-dplyr::lag(PSI_prog)) %>%
  filter(year==2017) %>%
  ggplot(aes(x=Delta_delta, y=Delta_psi))+
  geom_point()+
  xlim(-0.4,0.18)+
  ylim(-0.25,0.2)+
  geom_smooth(method = "lm", se = TRUE)+
  #stat_ellipse(level = 0.95)+
  theme_bw()+
  xlab(expression(paste(Delta,delta)))+
  ylab(expression(paste(Delta, psi[m])))




####MAPS####

#Note: process 'build_mappable_data' requires dataframes 'BEA' (which already has CZ-crosswalk built in)  and 'CZ_panel_split' (longitudinal CZ data)
#also, make sure plyr is NOT installed
build_mappable_data<-function(start_year, end_year, var, ind){
  var <- enquo(var) 
  #create county map:
  county_map <- map("county", fill = T, plot = FALSE)
  
  #obtain common fips values for match table
  county_map_match <- data.frame(name = county_map$names) 
  county_map_match<-left_join(county_map_match, county.fips, by=c("name"="polyname"))
  
  #merge CZ-crosswalk (from BEA) and CZ_panel data:
  mappable_CZ_panel<-merge(BEA %>% filter(IndustryClassification==ind)  %>% select(starts_with("year")  | starts_with("CZ_ID")| starts_with("Geo")), CZ_panel %>% filter(IndustryClassification==ind) , by = c("CZ_ID", "year"))
  
  #create mappable subset of county data:
  mappable_CZ_panel<-mappable_CZ_panel %>%
    filter(year==as.character(2002) | year==as.character(2017)) %>%
    mutate(fips=as.numeric(GeoFIPS)) %>%
    group_by(fips) %>%
    dplyr::arrange(., year, by_group=TRUE) %>%
    mutate(delta_var=(!!var-lag(!!var))) %>%
    filter(year==as.character(2017))
  
  
  #create mapping table between county fips & CZs:
  county_map_match <- county_map_match %>%
    left_join(mappable_CZ_panel, by = c("fips" = "fips"))
  #county_map_match<-distinct(county_map_match, county_map_match$fips)
  rownames(county_map_match) <- county_map_match$name
  
  # convert map to SpatialPolygon, then join with mapping table for SpatialPolygonDataFrame
  county_map <- map2SpatialPolygons(county_map, IDs = county_map$names)
  county_map <- SpatialPolygonsDataFrame(county_map, county_map_match)
  
  # remove invalidities in the county map
  gIsValid(county_map) #returns FALSE: there are invalid self-intersecting geometries in the polygons, which will cause problems
  county_map <- gBuffer(county_map, byid = TRUE, width = 0)
  gIsValid(county_map) #returns TRUE
  
  
  # dissolve county map by rating area & fortify to data frame
  cz_map <- unionSpatialPolygons(county_map, IDs = county_map$CZ_ID)
  cz_map <- fortify(cz_map)
  cz_map$group <- gsub(".1", "", x= cz_map$group, fixed = T)
  
  #merge data:
  cz_map<-merge(cz_map, mappable_CZ_panel, by.x=c("id"), by.y="CZ_ID")
  
  rm(mappable_CZ_panel, county_map, county_map_match)
  return(cz_map)
}

#FIG: empl_map.png:
BEA<-BEA %>% ungroup() %>% group_by(GeoFIPS, year) %>% mutate(empl_share=as.numeric(EMPL)/sum(as.numeric(EMPL), na.rm = TRUE))
CZ_panel<-CZ_panel %>% ungroup() %>% group_by(CZ_ID, year) %>% mutate(empl_share=EMPL/sum(EMPL, na.rm = TRUE))
temp<-build_mappable_data(2002, 2019, HHI_labor, "31-33") 
temp %>%
  ggplot()+
  geom_polygon(aes(x=long, y=lat, group = group, fill = delta_var), size = 0.01, colour = "black")+
  scale_fill_gradient2(low = "red", 
                       mid = "white",
                       midpoint = 0,
                       high = "blue",
                       limits=c(-0.001,0.005),
                       na.value = "white",
                       name = expression(paste("%",Delta,"L")))+
  theme(legend.position = c(0.9,0.2),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())




####POLITICAL MAPS & FIGS####

#import county election data; transform to CZ
countypres<- read_csv("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/election_data/countypres_2000_2020.csv")
countypres<-merge(countypres, CZ_crosswalk %>% mutate(county_fips=as.numeric(FIPS)) %>% select(!CZ_pop), by=c("county_fips"))
CZ_pres<-countypres %>%
  filter(party=="REPUBLICAN") %>%
  group_by(year, CZ_ID) %>%
  summarize_at(c("candidatevotes", "totalvotes"), sum, na.rm=TRUE)  
rm(countypres)

#fudge election years:
CZ_pres$year[CZ_pres$year==2012]<-2002
CZ_pres$year[CZ_pres$year==2016]<-2017
CZ_pres$year<-as.character(CZ_pres$year)
CZ_panel<-merge(CZ_panel, CZ_pres, by=c("CZ_ID", "year"), all.x=TRUE)
CZ_panel$repub_share<-CZ_panel$candidatevotes/CZ_panel$totalvotes

#FIG: repub_est.png
CZ_panel %>% 
  ungroup() %>%
  filter(year==2002 | year == 2017) %>%
  mutate(PSI=COMP/GDP) %>%
  group_by(CZ_ID, year) %>%
  mutate(EMPL_SHARE=EMPL/sum(EMPL, na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(CZ_ID, IndustryClassification) %>%
  mutate(D_PSI=(PSI)-(lag(PSI)), 
         D_EMPL=(EMPL-lag(EMPL))/lag(EMPL),
         D_EMPL_SHARE=(EMPL_SHARE-lag(EMPL_SHARE)),
         D_HHI_LABOR=log(HHI_labor)-log(lag(HHI_labor)),
         D_W_50_largest=log(W_50_largest)-log(lag(W_50_largest)),
         D_W_20_largest=log(W_20_largest)-log(lag(W_20_largest)),
         D_est=(est-lag(est))/lag(est),
         D_annual_state_minimum=log(annual_state_minimum)-log(lag(annual_state_minimum)),
         D_unempl_rate=unempl_rate-log(unempl_rate),
         D_capital_cost_index=capital_cost_index-lag(capital_cost_index),
         D_union_memb=union_memb-lag(union_memb),
         D_repub_share=(repub_share-lag(repub_share))
  ) %>%
  filter(IndustryClassification %in% c("31-33")) %>%
  filter(year!=2002) %>%
  select(starts_with("D"), year) %>%
  filter_all(all_vars(!is.infinite(.)))%>%
  ungroup() %>% 
  ggplot(aes(y=D_repub_share, x=D_est))+geom_point()+geom_smooth(method="lm")+
  #xlim(-1,1)+
  #ylim(-0.2,0.15)+
  ylab(expression(paste(Delta," Republican vote share, 2012-2016 ")))+
  xlab(expression(paste("%",Delta," Establishments, 2002-2017")))+
  theme_bw()




#####FIGURES####

#est_empl.png/est_empl_share.png
CZ_panel %>% 
  ungroup() %>%
  filter(year==2002 | year == 2017) %>%
  mutate(PSI=COMP/GDP) %>%
  group_by(CZ_ID, year) %>%
  mutate(EMPL_SHARE=EMPL/sum(EMPL, na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(CZ_ID, IndustryClassification) %>%
  mutate(D_PSI=log(PSI)-log(lag(PSI)), 
         D_EMPL=(EMPL-lag(EMPL))/lag(EMPL),
         D_EMPL_SHARE=(EMPL_SHARE-lag(EMPL_SHARE))/lag(EMPL_SHARE),
         D_HHI_LABOR=log(HHI_labor)-log(lag(HHI_labor)),
         D_W_50_largest=log(W_50_largest)-log(lag(W_50_largest)),
         D_W_20_largest=log(W_20_largest)-log(lag(W_20_largest)),
         D_est=(est-lag(est))/lag(est),
         D_annual_state_minimum=log(annual_state_minimum)-log(lag(annual_state_minimum)),
         D_unempl_rate=unempl_rate-log(unempl_rate),
         D_capital_cost_index=capital_cost_index-lag(capital_cost_index),
         D_union_memb=union_memb-lag(union_memb)
  ) %>%
  filter(IndustryClassification %in% c("31-33")) %>%
  filter(year!=2002) %>%
  select(starts_with("D"), year) %>%
  filter_all(all_vars(!is.infinite(.)))%>%
  ungroup() %>% 
  ggplot(aes(x=100*D_EMPL_SHARE, y=100*D_est))+geom_point()+geom_smooth(method="lm")+
  xlim(-75,100)+
  ylim(-100,200)+
  xlab(expression(paste("%",Delta," Employment share")))+
  ylab(expression(paste("%",Delta," Establishments")))+
  theme_bw()


#fig: empl_sahre_hhi.png
CZ_panel %>%
  group_by(IndustryClassification, year) %>%
  summarize(LABOR_SHARE=median(LABOR_SHARE, na.rm = TRUE), HHI_labor=median(HHI_labor, na.rm = TRUE), EMPL_SHARE=median(EMPL_SHARE, na.rm=TRUE)) %>%
  filter(IndustryClassification=="31-33" & !year %in% c(2018,2019,2001,2014, 2017)) %>%
  ggplot(aes(x=EMPL_SHARE, y=HHI_labor))+
  geom_point()+
 # xlim(0.16,0.32)+
  geom_text(aes(label=year), hjust=-0.5, vjust=-0.5)+
  geom_smooth(method = "lm")+
  theme_bw()+
  ylab("Labor market HHI")+
  xlab("Employment share")

#fig: man_labor_share_ts
CZ_panel %>%
  group_by(IndustryClassification, year) %>%
  summarize(LABOR_SHARE=median(LABOR_SHARE, na.rm = TRUE), HHI_labor=median(HHI_labor, na.rm = TRUE), EMPL_SHARE=median(EMPL_SHARE, na.rm=TRUE)) %>%
  filter(IndustryClassification=="31-33" & !year %in% c(2018,2019,2001,2014)) %>%
  ggplot(aes(x=as.numeric(year), y=LABOR_SHARE))+
  geom_line()+
  theme_bw()+
  ylab("Labor share")+
  xlab("")



########################################ESTIMATION##############################################
####STATA ESTIMATION####

#create first difference dataframe
first_diff_data<-CZ_panel %>% 
  ungroup() %>%
  #filter(year==2002 | year == 2017) %>%
  mutate(PSI=COMP/GDP) %>%
  group_by(CZ_ID, year) %>%
  mutate(EMPL_SHARE=EMPL/sum(EMPL, na.rm=TRUE)) %>%
  ungroup() %>% 
  group_by(CZ_ID, IndustryClassification) %>%
  mutate(D_PSI=(PSI)-(lag(PSI)), 
         D_EMPL=(EMPL)-lag((EMPL)),
         D_EMPL_SHARE=(EMPL_SHARE)-(lag(EMPL_SHARE)),
         D_HHI_LABOR=(HHI_labor)-(lag(HHI_labor)),
         D_W_50_largest=(W_50_largest)-(lag(W_50_largest)),
         D_W_20_largest=log(W_20_largest)-log(lag(W_20_largest)),
         D_est=(est/EMPL)-(lag(est/EMPL)),
         #  D_annual_state_minimum=(annual_state_minimum)-(lag(annual_state_minimum)),
         # D_unempl_rate=unempl_rate-lag(unempl_rate),
         D_capital_cost_index=log(capital_cost_index)-log(lag(capital_cost_index)),
         D_union_memb=union_memb-lag(union_memb)
  ) %>%
  #  filter(IndustryClassification=="31-33") %>%
  filter(year!=2002) %>%
  select(starts_with("D"), year) %>%
  filter_all(all_vars(!is.infinite(.)))%>%
  ungroup()

#create numeric time index
first_diff_data$time<-1
first_diff_data$time[first_diff_data$year==2007]<-2
first_diff_data$time[first_diff_data$year==2012]<-3
first_diff_data$time[first_diff_data$year==2017]<-4

#write:   
setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/estimation")
write.csv(first_diff_data %>% filter_all(all_vars(!is.na(.))), "panel.csv")


#create numeric time index
CZ_panel$time<-1
CZ_panel$time[CZ_panel$year==2007]<-2
CZ_panel$time[CZ_panel$year==2012]<-3
CZ_panel$time[CZ_panel$year==2017]<-4

setwd("C:/Users/Ansel/Box Sync/Documents/Structural Change/Job market paper/Preliminary analysis/estimation")
write.csv(CZ_panel %>% rename(firm_count=est) %>% group_by(CZ_ID, year) %>% mutate(firm_count_per_worker=firm_count/EMPL) %>% filter(!is.infinite(firm_count_per_worker)) %>% filter_all(all_vars(!is.na(.))), "diff_in_diff_panel.csv")


####CLEAN ESTIMATION####

#runs first difference estimations IND, outputs coeff. and robust SE
build_industry_coeff<-function(IND){
  if(substr(IND,1,1)=="2" | IND=="53" | IND=="55"){
    return(data.frame(term=character(),estimate=numeric(), std.error=numeric(), statistic=numeric(), p.value=numeric(), Industry=character() ))
  }
  else{
    sub_tab<-CZ_panel %>%
      filter(IndustryClassification==IND & EMPL>0 & est>0 & LABOR_SHARE>0 &  PAYROLL_SHARE>0 & HHI_labor>0 & year !="2001" & IndustryClassification=="31-33") %>%
      group_by(CZ_ID, IndustryClassification) %>%
      mutate(
        D_LABOR_SHARE=(LABOR_SHARE)-(lag(LABOR_SHARE)), 
        D_PAYROLL_SHARE=PAYROLL_SHARE-lag(PAYROLL_SHARE),
       # D_W_50_largest=(W_50_largest)-(lag(W_50_largest)),
        #D_W_20_largest=(W_20_largest)-(lag(W_20_largest)),
        D_est=log(est)-log(lag(est)),
        D_est_per_worker=est_per_worker-lag(est_per_worker),
        #D_est_per_mile=est_per_mile-lag(est_per_mile),
        D_HHI_labor=(HHI_labor)-(lag(HHI_labor)),
        D_union_memb=log(union_memb)-log(lag(union_memb)),
        #D_annual_state_minimum=log((annual_state_minimum))-log((lag(annual_state_minimum))),
        D_unempl_rate=log(unempl_rate)-log(lag(unempl_rate)),    
        D_capital_cost_index=(capital_cost_index)-(lag(capital_cost_index))
      ) %>%
      ungroup() %>%
      lm(D_LABOR_SHARE ~ D_HHI_labor+D_union_memb+D_capital_cost_index+D_unempl_rate, data = .) %>%
      coeftest(., vcov = vcovHC(., "HC1")) %>%   # robust; HC1 (Stata default)
      tidy() %>%
      mutate(Industry=IND,
             p.value=round(p.value,4))
    return(sub_tab)
  }
}

#run estimation for all industries 
TABLE<-bind_rows(lapply(unique(CZ_panel$IndustryClassification), build_industry_coeff))













#FIRST DIFFERENCE MODEL
FD_model<-CZ_panel %>% 
  ungroup() %>%
  group_by(CZ_ID, year) %>%
  mutate(EMPL_SHARE=EMPL/sum(EMPL, na.rm=TRUE),
         POP=sum(EMPL,na.rm = TRUE)) %>%
  ungroup() %>% 
  #filter(year<2017) %>%
  mutate(PSI=COMP/(GDP-PERSONAL_INC+COMP),
         wage=COMP/EMPL,
         prod=GDP/EMPL) %>%
  filter(IndustryClassification=="44-45" & EMPL>0 & est>0 & PSI>0 & HHI_labor>0) %>%
  group_by(CZ_ID) %>%
  mutate(
    D_EMPL_SHARE=EMPL_SHARE-lag(EMPL_SHARE),
    D_PSI=log(PSI)-log(lag(PSI)), 
    #D_W_50_largest=(W_50_largest)-(lag(W_50_largest)),
    #D_W_20_largest=(W_20_largest)-(lag(W_20_largest)),
    D_est=(est)-(lag(est)),
    D_est_per_worker=(est/EMPL)-(lag(est/EMPL)),
    D_est_per_capita=(est/POP)-(lag(est/POP)),
    D_est_per_mile=(est/LAND)-(lag(est/LAND)),
    D_HHI_labor=(HHI_labor)-(lag(HHI_labor)),
    #D_annual_state_minimum=(annual_state_minimum)-(lag(annual_state_minimum)),
    #D_unempl_rate=unempl_rate-lag(unempl_rate),
    #D_capital_cost_index=(capital_cost_index)-(lag(capital_cost_index)),
    #D_union_memb=union_memb-lag(union_memb)
  ) %>%
  ungroup() %>%
  lm(D_PSI~D_HHI_labor,.)
summary(FD_model)


#reproduce the Stata default
coeftest(FD_model, vcov = vcovHC(FD_model, "HC1"))    # robust; HC1 (Stata default)

# Adjust standard errors
cov1         <- vcovHC(FD_model, type = "HC1")
robust_se    <- sqrt(diag(cov1))

# Stargazer output (with and without RSE)
stargazer(FD_model, type = "text", 
          #covariate.labels=c("Establishments", "Product market concentration", "Unemployment rate", "Union membership", "Capital cost"),
          se = list(robust_se))



#FIXED EFFECTS MODEL
FE_model<-CZ_panel %>% 
  ungroup() %>%
  group_by(CZ_ID,year) %>%
  mutate(POP=sum(EMPL, na.rm = TRUE)) %>%
  ungroup() %>%
  # filter(year==2002 | year==2017) %>%
  mutate(PSI=COMP/(GDP-PERSONAL_INC+COMP),
         est_per_mile=est/LAND,
         est_per_capita=est/POP) %>%
  filter(IndustryClassification=="31-33" & est>0) %>%
  plm(PSI~est_per_capita,., index=c("CZ_ID", "year"), effect="twoways", model="within")
summary(FE_model)

# Adjust standard errors
cov1         <- vcovHC(FE_model, type = "HC1")
robust_se    <- sqrt(diag(cov1))

# Stargazer output (with and without RSE)
stargazer(FE_model, type = "text", 
          #covariate.labels=c("Establishments", "Product market concentration", "Unemployment rate", "Union membership", "Capital cost"),
          se = list(robust_se))









####2SLS ESTIMATION####


data<-CZ_panel %>%
  filter(EMPL>0 & est>0 & LABOR_SHARE>0 &  PAYROLL_SHARE>0 & HHI_labor>0 & year !="2001" & IndustryClassification=="31-33") %>%
  group_by(CZ_ID, IndustryClassification) %>%
  mutate(
    D_LABOR_SHARE=(LABOR_SHARE)-(lag(LABOR_SHARE)), 
    D_PAYROLL_SHARE=PAYROLL_SHARE-lag(PAYROLL_SHARE),
    D_EMPL_SHARE=(EMPL_SHARE)-(lag(EMPL_SHARE)),
    D_est=log(est)-log(lag(est)),
    D_HHI_labor=log(HHI_labor)-log(lag(HHI_labor)),
    D_union_memb=(union_memb)-(lag(union_memb)),
    #D_annual_state_minimum=log((annual_state_minimum))-log((lag(annual_state_minimum))),
    D_unempl_rate=log(unempl_rate)-log(lag(unempl_rate)),    
    D_capital_cost_index=(capital_cost_index)-(lag(capital_cost_index))
  ) %>% pdata.frame(., index=c("CZ_ID", "year"))

SEM_model<-ivreg(D_LABOR_SHARE ~ +lag(LABOR_SHARE) +lag(D_HHI_labor) + lag(D_union_memb) + lag(D_capital_cost_index) + lag(D_unempl_rate) | 
                   . - lag(D_HHI_labor) + lag(D_EMPL_SHARE), data=data )

summary(SEM_model, diagnostics=TRUE)

#test for rank condition (employment share is a good instrument for HHI)
reduced_form <- lm(D_LABOR_SHARE ~ + D_W_20_largest + D_EMPL_SHARE, data=data )
summary(reduced_form)


#####GMM ESTIMATION####

#Good explanation of Arellano-Bond approach:
#https://www.youtube.com/watch?v=OAqk-VAAEWA&ab_channel=MikkoR%C3%B6nkk%C3%B6

library(dplyr)

data <- CZ_panel %>% filter(IndustryClassification=="31-33" & year != "2001" & year !="2014")

detach("package:dplyr", unload=TRUE) #This is changing the results... w/o detaching, the results are positive....

gmm_model<-pgmm((LABOR_SHARE) ~ lag(LABOR_SHARE, 1:1)+lag(HHI_labor,1:1)+lag(union_memb, 1)+lag(capital_cost_index, 1)+lag(unempl_rate, 1)
                |lag(LABOR_SHARE, 3:20), #GMM instrument
                effect = "individual", #null=no FE, individual = First diff, twoways= FD and time dummies
                model="twosteps",
                collapse = TRUE,
                transformation="ld", #prefered system GMM because of unbalanced panel
                data= data, 
                robust=TRUE,
                time.dummies=TRUE) 

summary(gmm_model)




ols_model<- plm((LABOR_SHARE) ~ (lag(LABOR_SHARE, 1:1))+
                 (lag(HHI_labor,1:1))+
                 (lag(union_memb, 1:1))+
                 (lag(capital_cost_index, 1:1))+
                 (lag(unempl_rate, 0:0)),
               data=data,
               effect="individual", 
               model="fd")
summary(ols_model)









#####SINGLE EQUATION ESTIMATION####
#Declare panel:

library(dplyr)
ManPanel<-CZ_panel %>%
  filter(IndustryClassification=="31-33" & year !="2001" & year !="2014" & HHI_labor>0 & LABOR_SHARE<1) %>% 
  select(year, CZ_ID,LABOR_SHARE,unempl_rate,HHI_labor,capital_cost_index, union_memb, EMPL_SHARE )
ManPanel<-pdata.frame(ManPanel, index=c("CZ_ID", "year"))
#ManPanel<-ManPanel[complete.cases(ManPanel),]
detach("package:dplyr", unload=TRUE) #This is changing the results... w/o detaching, the results are positive....
 
#test for serial correlation:
pdwtest(LABOR_SHARE ~ lag(HHI_labor,1)+lag(union_memb, 0)+lag(capital_cost_index, 0)+lag(unempl_rate, 0), data=ManPanel, model="random")
#...serial correlation detected!

####STATIC MODELS

#FE model:
fe_model_static<- plm((LABOR_SHARE) ~
                # lag(LABOR_SHARE, 1:1)+
                 lag(HHI_labor, 0:0)+
                 lag(union_memb, 0:0)+
                 lag(capital_cost_index, 0:0)+
                 lag(unempl_rate, 0:0),
               #lag(import_exposure,0:0),
               data=ManPanel,
               model="within", #note: within estimation removes individual FE. Within demeans the data within each observation
               effects="twoways", index=c("CZ_ID", "year"))
summary(fe_model_static)

#FD model:
fd_model_static<- plm((LABOR_SHARE) ~
                 lag(HHI_labor, 0:0)+
                 lag(union_memb, 0:0)+
                 lag(capital_cost_index, 0:0)+
                 lag(unempl_rate, 0:0),
               #lag(import_exposure,0:0),
               data=ManPanel,
               model="fd", #note: within estimation removes individual FE. Within demeans the data within each observation
               effects="twoways", index=c("CZ_ID", "year"))
summary(fd_model_static)


####DYNAMIC MODELS

#FE model:
fe_model_dynamic<- plm((LABOR_SHARE) ~
                     lag(LABOR_SHARE, 1:1)+
                     lag(HHI_labor, 0:0)+
                     lag(union_memb, 0:0)+
                     lag(capital_cost_index, 0:0)+
                     lag(unempl_rate, 0:0),
                   data=ManPanel,
                   model="within", #note: within estimation removes individual FE. Within demeans the data within each observation
                   effects="twoways", index=c("CZ_ID", "year"))
summary(fe_model_dynamic)


#GMM with Arellano-Bond approach (system GMM) ar(1):
#See: https://www.youtube.com/watch?v=4D3MKnYQ44E&ab_channel=CrunchEconometrix

ab_model<-pgmm((LABOR_SHARE) ~
                  lag(LABOR_SHARE, 1:1)+
                  lag(HHI_labor, 0:0)+
                  lag(union_memb, 0:0)+
                  lag(unempl_rate, 0:0) +
                  lag(capital_cost_index, 0:0)
                |lag(LABOR_SHARE, 2:3), #GMM instrument
                effect ="individual", #null=no FE, individual = First diff, twoways= FD and time dummies
                model="onestep",
                collapse = TRUE,
                transformation="d", #prefered system GMM because of unbalanced panel
                data= ManPanel, 
                robust=TRUE,
                time.dummies=FALSE)  


summary(ab_model)
 
#NOTE: Want failure to reject null for Sargan test for validity and failure to reject autocorrelation (2) (Arellano-Bond test)

#thoughts on GMM estimation: inclusion of lagged dependent variable reduces the significance for most coefficients...
#does this seem right? Surely these factors do influence the labor share...
#But maybe exclusion of lagged is justified from a theoretical standpoint: after all, one of Kaldor's facts was its stability...


#OUTPUT TABLE
stargazer(fe_model_static_unbalanced, fd_model_static_unbalanced,fe_model_static, fd_model_static, fe_model_dynamic, ab_model,
          column.labels  = c("Unbalanced", "Static", "Dynamic"),
          column.separate = c(2,2,2),
          dep.var.labels.include=FALSE,
          model.names = TRUE)





#####MULTIPLE EQUATION ESTIMATION####
#See: https://www.sfu.ca/sasdoc/sashtml/ets/chap19/sect32.htm


library(dplyr)
ManPanel<-CZ_panel %>%
  filter(IndustryClassification=="31-33" & year !="2001" ) %>% 
  select(year, CZ_ID,LABOR_SHARE,unempl_rate,HHI_labor,capital_cost_index, union_memb, EMPL_SHARE, import_exposure )
ManPanel<-pdata.frame(ManPanel, index=c("CZ_ID", "year"))
#ManPanel<-ManPanel[complete.cases(ManPanel),]
detach("package:dplyr", unload=TRUE) #This is changing the results... w/o detaching, the results are positive....


#2sls FIXED EFFECTS
FE_2sls1<-plm(LABOR_SHARE ~ 
                      #lag(LABOR_SHARE, 1:1)+
                      lag(HHI_labor, 0:0) +
                      lag(union_memb, 0:0) +
                      lag(capital_cost_index, 0:0) +
                      lag(unempl_rate, 0:0) | . -lag(HHI_labor, 0:0)+ lag((EMPL_SHARE), 0:0)+lag((import_exposure), 0:0), 
                    data=ManPanel, effect="twoways", model="within")
summary(FE_2sls1)

FE_2sls2<-plm(HHI_labor~EMPL_SHARE+import_exposure, data=ManPanel, effects="twoways", model="within")
summary(FE_2sls2)

#2sls FIRST DIFFERENCE
FD_2sls1<-plm(LABOR_SHARE ~ 
                #lag(LABOR_SHARE, 1:1)+
                lag(HHI_labor, 0:0) +
                lag(union_memb, 0:0) +
                lag(capital_cost_index, 0:0) +
                lag(unempl_rate, 0:0) | . -lag(HHI_labor, 0:0)+ lag((EMPL_SHARE), 0:0)+lag((import_exposure), 0:0), 
              data=ManPanel, effect=NULL, model="fd")
summary(FE_2sls1)

FD_2sls2<-plm(HHI_labor~EMPL_SHARE+import_exposure, data=ManPanel, effects=NULL, model="fd")
summary(FE_2sls2)


stargazer(FE_2sls1, FE_2sls2, FD_2sls1, FD_2sls2, 
          column.labels  = c("FE", "FD"),
          column.separate = c(2,2),
          dep.var.labels.include=TRUE,
          model.names = TRUE)








#GMM system of equations:
gmm_system1<-pgmm((LABOR_SHARE) ~
                 lag(LABOR_SHARE, 1:1)+
                 lag(HHI_labor,0:0)+
                 lag(union_memb, 0:0)+
                 lag(unempl_rate, 0:0) +
                 lag(capital_cost_index, 0:0)
               |lag(LABOR_SHARE,2:3) #GMM instrument
               | . - lag(HHI_labor, 0:0)+lag((import_exposure), 0:0)+lag(EMPL_SHARE, 0:0),
               effect = "individual", #null=no FE, individual = First diff, twoways= FD and time dummies
               model="twostep",
               collapse = FALSE,
               transformation="d", #prefered system GMM because of unbalanced panel
               data= ManPanel, 
               robust=TRUE,
               time.dummies=TRUE)  

summary(gmm_system1)

gmm_system2<-pgmm(HHI_labor ~
                    lag(HHI_labor, 1:1)+
                    lag((import_exposure), 0:0)+
                    lag(EMPL_SHARE, 0:0) |
                    lag(HHI_labor, 3:4),
                  effect="individual",
                  model="twostep",
                  collapse=TRUE,
                  transformation="d",
                  data=ManPanel,
                  robust=TRUE)
summary(gmm_system2)


stargazer(twosls_system1, twosls_system2,gmm_system1, gmm_system2 )




#WHAT IS THE PERSISTANCE OF HHI_labor? Does it have a unit root?
gmm_hhi_persistence<-pgmm(HHI_labor ~lag(HHI_labor, 1:1) | lag(HHI_labor, 2:11),
                          effect="twoways",
                          model="onestep",
                          collapse=FALSE,
                          transformation="ld",
                          data=ManPanel,
                          robust=TRUE)
summary(gmm_hhi_persistence)

#unit root test:
library(dplyr)
X<- ManPanel %>% select(CZ_ID, year, HHI_labor) %>% filter(!is.na(HHI_labor) & !is.infinite(HHI_labor))
purtest(X, pmax=4,exo= "intercept", test="madwu")
