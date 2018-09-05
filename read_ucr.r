### LEMAS 2013, 36164-0001-Data
### ... 2007, 31161
### CSLLEA 2008, 27681
### UCR offenses http://doi.org/10.3886/E100707V6


#### read in ucr files

library(tidyverse)

# #### lemas
# lemas13<-read.delim("./data/ucr/36164-0001-Data.tsv")
# lemas07<-read.delim("./data/ucr/31161-0001-Data.tsv")
### ucr, csllea, govid crosswalk
xwalk<-read.delim("./data/35158-0001-Data.tsv", 
                  encoding="UTF-8", stringsAsFactors=FALSE)
xwalk<-xwalk%>%
  select(ORI9, GOVID, CSLLEA08_ID)
### csllea
csllea<-read.delim("./data/ucr/27681-0001-Data.tsv", 
                   encoding="UTF-8", stringsAsFactors=FALSE)
### join with xwalk to get ori, govid


### ucr offenses known, clearances
ucr<-read.csv("./data/ucr/ucr_offenses_known_yearly_1960_2016.csv",
              encoding="UTF-8", stringsAsFactors=FALSE)

index<-max(grep("act_", names(ucr)))
ucr<-ucr[, 1:index]

ucr<-ucr%>%
  mutate(murder = act_murder,
         violent = act_murder + act_manslaughter + 
           act_rape_total + act_robbery_total + 
           act_assault_total,
         property = act_burglary_total + act_theft_total +
           act_mtr_vhc_theft_total)%>%
  select(ori9, year, murder, violent, property)

ucr<-ucr%>%
  filter(year>2000)

### ucr xwalk join
ucr<-ucr%>%
  rename(ORI9=ori9)%>%
  left_join(xwalk)

### join with csllea
crime_dat<-csllea%>%
  select(-STATENAME)%>%
  left_join(ucr)%>%
  select(ORI9, GOVID, FIPS, AGCYNAME, STATE, year, violent, property, murder, FTSWORN)%>%
  mutate(violent = ifelse(violent<0, NA, violent),
         property = ifelse(property<0, NA, property),
         murder = ifelse(murder<0, NA, murder))

#### DO I JUST WANT TO USE FIPS PLACE?? OR GOVID?? WHICH IS THE MASTERKEY??
