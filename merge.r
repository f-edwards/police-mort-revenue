rm(list=ls())

library(tidyverse)
library(tm)
library(fuzzyjoin)
library(lubridate)

######## read in police mort data

# ... attach and configure mortality file
# ... join on imputed race data to grab fips codes (obtained via geocoding)
fe <- read_csv("./data/fe_data_6_8_2018.csv")
# .... make names more user-friendly
names(fe)<-c("id", "name", "age", "gender", "race", "URL", "death_date", 
                 "loc_address", "loc_city", "loc_state", "loc_zip", "loc_county", 
                 "loc_full_address", "Latitude", "Longitude", "agency", 
                 "cause_of_death","cause_description", "official_disposition", 
                 "news_url", "mental_illness", "video", "null1", "dateanddesc", 
                 "null2", "id2", "year", "null3")
# year variables don't work based on GDocs sheet

# ### filter ruled suicides
# fe<-fe%>%
#   mutate(official_disposition=tolower(official_disposition))
# 
# fe<-fe[-grep("suicide", fe$official_disposition),]

# .... re-label race variable
fe <- fe %>%
  select(id, name, age, gender, race, death_date, 
         loc_state, loc_county, loc_zip, loc_city, agency,
         agency, cause_of_death, official_disposition) %>%
  mutate(race = ifelse(race == "African-American/Black", "black", race),
         race = ifelse(race == "Asian/Pacific Islander", "asian", race),
         race = ifelse(race == "European-American/White", "white", race),
         race = ifelse(race == "Hispanic/Latino", "latino", race),
         race = ifelse(race == "Middle Eastern", "other", race),
         race = ifelse(race == "Native American/Alaskan", "other", race),
         race = ifelse(race == "Race unspecified", NA, race))
#### make date from date of death
fe$death_date<-mdy(fe$death_date)
fe$year<-year(fe$death_date)
fe<-fe%>%
  select(-death_date)
### 

### UCR data for crosswalk
source("read_ucr.r")

xwalk<-crime_dat%>%
  mutate(agency = toupper(AGCYNAME))%>%
  select(-AGCYNAME)%>%
  rename(loc_state=STATE)

###################################
######## merge and setup
###################################

fe<-fe%>%
  mutate(agency = toupper(agency))%>%
  mutate(agency = gsub("\\,.*","",agency))%>%
  filter(!(is.na(agency)))

### THIS GETS MOST OF THEM!!!! THAT'S AWESOME

### DEALING WITH MISMATCHES
### DEPT/DEPARTMENT
xwalk$agency<-trimws(xwalk$agency)
xwalk$agency<-gsub("’", "'", xwalk$agency)
xwalk$agency<-gsub("DEPARTMENT", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPARTMEN", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPARTME", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPARTM", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPART", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPAR", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPT.", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPTF", "DEPT", xwalk$agency)
xwalk$agency<-gsub("DEPTOF", "DEPT OF", xwalk$agency)
xwalk$agency<-gsub(" PD", " POLICE DEPT", xwalk$agency)
xwalk$agency<-gsub("SHERIFF'S DEPT", "SHERIFF'S OFFICE", xwalk$agency)
xwalk$agency<-gsub("DEPT", "", xwalk$agency)
xwalk$agency<-gsub("OFFICE", "", xwalk$agency)
xwalk$agency<-gsub("SHERIFF'S", "SHERIFF", xwalk$agency)

xwalk$agency<-trimws(xwalk$agency)

fe$agency<-trimws(fe$agency)
fe$agency<-gsub("’", "'", fe$agency)
fe$agency<-gsub("DEPARTMENT", "DEPT", fe$agency)
fe$agency<-gsub("DEPARTMEN", "DEPT", fe$agency)
fe$agency<-gsub("DEPARTME", "DEPT", fe$agency)
fe$agency<-gsub("DEPARTM", "DEPT", fe$agency)
fe$agency<-gsub("DEPART", "DEPT", fe$agency)
fe$agency<-gsub("DEPAR", "DEPT", fe$agency)
fe$agency<-gsub("DEPT.", "DEPT", fe$agency)
fe$agency<-gsub("DEPTF", "DEPT", fe$agency)
fe$agency<-gsub(" PD", " POLICE DEPT", fe$agency)
fe$agency<-gsub("DEPTOF", "DEPT OF", fe$agency)
fe$agency<-gsub("SHERIFF'S DEPT", "SHERIFF'S OFFICE", fe$agency)
fe$agency<-gsub("DEPT", "", fe$agency)
fe$agency<-gsub("OFFICE", "", fe$agency)
fe$agency<-gsub("SHERIFF'S", "SHERIFF", fe$agency)
fe$agency<-trimws(fe$agency)

############# JOIN FE TO UCR XWALK TO ATTACH GOVID, FIPS
### loop over states, fuzzy match. want exact match on states, fuzzy on agency
### distance 1 seems to perform well, 2 gets lots of false matches
### assume no duplicate agency names within states

states<-unique(xwalk$loc_state)

dat_out<-list()
for(i in 1:length(states)){
  fe_st<-fe%>%
    filter(loc_state==states[i])%>%
    select(agency, id)
  
  xwalk_st<-xwalk%>%
    filter(loc_state==states[i])%>%
    select(agency, GOVID, ORI9)%>%
    mutate(GOVID = as.character(GOVID))%>%
    distinct()
  ### exact match, then rbind fuzzymatches on non-exacts
  ### fuzzy is mismatching some that have exacts
  exacts<-left_join(fe_st, xwalk_st)%>%
    filter(!(is.na(GOVID)))
  
  fuzzies<-stringdist_left_join(
    fe_st%>%filter(!id%in%exacts$id), 
    xwalk_st, distance_col="dist",
    max_dist=1)%>%
    select(agency.x, id, GOVID, ORI9)%>%
    rename(agency = agency.x)
  
  ### deal with federal agencies
  fuzzies<-fuzzies%>%
    mutate(
      GOVID = case_when(
        grepl("^U.S.", agency) ~ "FEDERAL",
        agency == "IMMIGRATION AND NATURALIZATION SERVICE" ~ "FEDERAL",
        agency == "TRANSPORTATION SECURITY ADMINISTRATION" ~ "FEDERAL",
        TRUE ~ as.character(GOVID))
    )
  
  dat_out[[i]]<-bind_rows(exacts, fuzzies)%>%
    mutate(loc_state = states[i])
}

#### this shows the matches from by state fuzzy match
dat_out<-bind_rows(dat_out)
### remove federal agencies?
#### join fe, xwalk, miss_out, overwrite GOVID on IDs in missing
#### write out csv for manual recoding, use fbi xwalk and cog govids to recode missings
#### govid is more important here
missings<-dat_out%>%filter(is.na(GOVID))
# 
#write_csv(missings, "./data/missings_govid_manual_2.csv")
### previous coding work, don't re-invent the wheel...
# missings2<-read_csv("./data/manual_ucr_fe_coding.csv")%>%
#   select(-agency, -loc_state)
# ### join new and old, isolate still missings
# merge_miss<-missings%>%
#   left_join(missings2)%>%
#   select(-GOVID, -ORI9)
# 
# write_csv(merge_miss, "./data/new_missings_manual_coding.csv")

missings<-read.csv("./data/manual_recodes_ucr_govid.csv",
                   stringsAsFactors = FALSE)%>%
  rename(GOVID=man_GOVID, ORI9=man_ORI9)

not_missings<-dat_out%>%
  filter(!(id%in%missings$id))

#### bind all together for an id, agency, ori, govid, state xwalk for FE

fe_index<-bind_rows(missings, not_missings)%>%
  select(-agency)%>%
  arrange(id)

############################################################################################################
################################ Finance Data
### only include 2012 CoG data
### drop state (0), special district (4), and school districts (5)
### amounts are in $1,000s. Need to be inflation adjusted


gov_id<-read_fwf("./data/Fin_GID_2012.txt", 
                 fwf_widths(
                   c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2),
                   c("gov_id", "gov_name", "cnty_name",
                     "fips_st", "fips_cnty", "fips_place", 
                     "pop", "pop_yr", "enroll", "enroll_yr", "funct_code",
                     "school_code", "fiscal_year", "survey_year")))%>%
  mutate(type_code = substr(gov_id, 3, 3))%>%
  mutate(GOVID = as.numeric(substr(gov_id, 1, 9)))

#### read finance data
#### get codes from https://www2.census.gov/govs/local/ summary tabulation methodology
#### defined codes in cog_codes.R
fin_dat<-read_fwf("./data/2012FinEstDAT_08172017modp_pu.txt",
                  fwf_widths(c(14, 3, 12, 4, 1),
                             c("gov_id", "item_code", "amount", "year", "imp_flag")
                  ))

source("cog_codes.R")

### govt ids are 14 digit here, 9 digit in xwalk
### gov_id digits 10-14 are zero, substr(gov_id$gov_id, 10, 14)) == 00000
### strip trailing zeroes, convert to numeric
#### this matches UCR xwalk file very well

fin_wide<-make_wide_cog(fin_dat)%>%
  ungroup()%>%
  mutate(gov_id = substr(gov_id, 1, 9))%>%
  mutate(GOVID=as.numeric(gov_id))%>%
  select(-rev_sales_tax, -rev_int_gov_police, -gov_id)

fin_dat_2007<-read_csv("./data/unit_file_2007.csv")%>%
  select(-rev_salse_tax)

### merge two files

fin_merge<-fin_wide%>%
  bind_rows(fin_dat_2007)
### join gov_id geo data

fin_merge<-fin_merge%>%
  left_join(gov_id%>%
              select(GOVID, gov_name, fips_st, fips_cnty, fips_place, type_code))

fin_local<-fin_merge%>%
  filter(type_code %in% c(1, 2, 3))%>%
  filter(exp_police>0)

#### check on matching in FE, UCR merge and CoG, some valid cases aren't matchin...

# table(dat_merge$GOVID%in%fin_wide$GOVID)
# z<-which(!(dat_merge$GOVID%in%fin_wide$GOVID))
# checks<-dat_merge[z,"ORI9"]%>%filter(!(is.na(ORI9)))
# 
# z1<-which(xwalk$ORI9%in%checks$ORI9)
# 
# xwalk[z1,]%>%select(ORI9, GOVID, COMMENT)%>%filter(!(is.na(COMMENT)))

### 623 missing of 24000
### Mismatches are: Federal, Tribal, and 999999 GOVID in UCR. Can try to match those better with xwalk
### xwalk notes has GOVID for multi-jurisdiction units. just going to assign on the first one
### GOVID: 999999991 is multi-unit, pull IDs from COMMENT
### > xwalk[z1,]%>%select(ORI9, GOVID, COMMENT)%>%filter(!(is.na(COMMENT)))
### manually update GOVID for link with ORI9

fe_index<-fe_index%>%
  mutate(GOVID = ifelse(ORI9=="GA0250300", "112025003", GOVID),
         GOVID = ifelse(ORI9=="ID0420300", "132042006", GOVID),
         GOVID = ifelse(ORI9=="MN0270700", "242027009", GOVID),
         GOVID = ifelse(ORI9=="NC0600100", "342060001", GOVID),
         GOVID = ifelse(ORI9=="PA0021B00", "393002030", GOVID),
         GOVID = ifelse(ORI9=="PA0064100", "392006024", GOVID),
         GOVID = ifelse(ORI9=="PA0152700", "393015056", GOVID),
         GOVID = ifelse(ORI9=="PA0210500", "392021003", GOVID),
         GOVID = ifelse(ORI9=="PA0260400", "392026001", GOVID),
         GOVID = ifelse(ORI9=="PA0450800", "392045004", GOVID),
         GOVID = ifelse(ORI9=="PA0520400", "392052001", GOVID),
         GOVID = ifelse(ORI9=="PA0672500", "393067011", GOVID),
         GOVID = ifelse(ORI9=="PA0671600", "392067019", GOVID),
         GOVID = ifelse(ORI9=="TX1012400", "442101003", GOVID),
         GOVID = ifelse(ORI9=="VA0980000", "471100100", GOVID))

write_csv(fe_index, "./data/fe_ucr_cog_xwalk.csv")

#### MERGE ALL THE DATAS, START WITH VALID CoG LOCAL GOVS
### time series by govid
### create index of all govids with police, merge, count

# filter irrelevant causes of death, dropping abt 2.5% of cases
fe<-fe%>%
  filter(cause_of_death %in%
           c('Asphyxiated/Restrained','Beaten/Bludgeoned with instrument',
             'Chemical agent/Pepper spray', 'Medical emergency', 'Tasered',
             'Gunshot', 'Vehicle'))

local_govs<-fin_local%>%
  select(GOVID, fips_st, fips_cnty, fips_place)%>%
  distinct()

fe_govid_ts<-local_govs%>%
  mutate(GOVID = as.character(GOVID))%>%
  left_join(fe%>%
  left_join(fe_index)%>%
  group_by(year, GOVID)%>%
  summarise(deaths = n()))%>%
  ungroup()%>%
  complete(year, nesting(GOVID, fips_st, fips_cnty, fips_place), 
           fill = list(deaths = 0))%>%
  arrange(GOVID)

fe_cnty<-fe_govid_ts%>%
  ungroup()%>%
  group_by(year, fips_st, fips_cnty)%>%
  summarise(deaths = sum(deaths))

#### make 5-yr averages for 2005-9, 2010-14
fe_govid_ts<-fe_govid_ts%>%
  mutate(
    year_range = case_when(
      year>=2007 & year<2011 ~ "2007-2011",
      year>=2012 & year<2016 ~ "2012-2016"
    )
  )%>%
  filter(!(is.na(year_range)))%>%
  group_by(year_range, GOVID, fips_st, fips_cnty, fips_place)%>%
  summarise(deaths = sum(deaths))

fe_cnty_ts<-fe_cnty%>%
  mutate(
    year_range = case_when(
      year>=2007 & year<2011 ~ "2007-2011",
      year>=2012 & year<2016 ~ "2012-2016"
    )
  )%>%
  filter(!(is.na(year_range)))%>%
  group_by(year_range, fips_st, fips_cnty)%>%
  summarise(deaths = sum(deaths))

### can preserve as counts for now, multiply offset by 5 to get it back to 
### appropriate rate. think abt alt to poisson/negbin for data with zeroes

##################### UCR TO MERGE
# make 5-yr moving avg, denom is yrs reported
# unit is parent government, not police agency
# depts with zero reported crimes are non-reporting

crime_dat<-crime_dat%>%
  filter(!(violent + property + murder ==0))

crime_govid_ts<-crime_dat%>%
  mutate(
      year_range = case_when(
      year>=2007 & year<2011 ~ "2007-2011",
      year>=2012 & year<2016 ~ "2012-2016"
    ))%>%
  filter(!(is.na(year_range)))%>%
  group_by(GOVID, year_range)%>%
  summarise(violent.yr = sum(violent / n()), 
            property.yr = sum(property / n()),
            murder.yr = sum(murder / n()),
            ft_sworn = sum(FTSWORN/n()))

crime_cnty_ts<-crime_dat%>%
  mutate(
    year_range = case_when(
      year>=2007 & year<2011 ~ "2007-2011",
      year>=2012 & year<2016 ~ "2012-2016"
    ))%>%
  filter(!(is.na(year_range)))%>%
  group_by(FIPS, year_range)%>%
  summarise(violent.yr = sum(violent / n()), 
            property.yr = sum(property / n()),
            murder.yr = sum(murder / n()),
            ft_sworn = sum(FTSWORN/n()))


#### POP DATA
make_pop_nhgis07_11<-function(x){
  out<-x
  out<-out%>%
    mutate(pop_tot = MNTE001,
           pop_pct_men_15_34 = (MNIE006 + MNIE007 + MNIE008 + MNIE009 + 
                                  MNIE010 + MNIE011 + MNIE012) / MNIE001,
           pop_wht = MN2E003,
           pop_blk = MN2E004 + MN2E014,
           pop_ami = MN2E005 + MN2E015,
           pop_api = MN2E006 + MN2E016 + MN2E007 + MN2E017,
           pop_lat = MN2E012,
           pop_pct_pov = (MPVE002 + MPVE003)/MPVE001,
           pop_pct_deep_pov = MPVE002/MPVE001,
           pop_med_income = MP1E001,
           pop_pc_income = MRUE001
    )
  out<-out%>%
    select(STATEA, COUNTYA, PLACEA, 
           pop_tot, pop_pct_men_15_34,
           pop_wht, pop_blk, pop_ami, pop_api, pop_lat,
           pop_pct_pov, pop_pct_deep_pov,
           pop_med_income, pop_pc_income)%>%
    mutate(year_range = "2007-2011")
  return(out)
}

make_pop_nhgis12_16<-function(x){
  out<-x
  out<-out%>%
    mutate(pop_tot = AF2LE001,
           pop_pct_men_15_34 = (AF2AE006 + AF2AE007 + AF2AE008 + AF2AE009 + 
                                  AF2AE010 + AF2AE011 + AF2AE012) / AF2AE001,
           pop_wht = AF2UE003,
           pop_blk = AF2UE004 + AF2UE014,
           pop_ami = AF2UE005 + AF2UE015,
           pop_api = AF2UE006 + AF2UE016 + AF2UE007 + AF2UE017,
           pop_lat = AF2UE012,
           pop_pct_pov = (AF43E002 + AF43E003)/AF43E001,
           pop_pct_deep_pov = AF43E002/AF43E001,
           pop_med_income = AF49E001,
           pop_pc_income = AF6AE001
    )
  out<-out%>%
    select(STATEA, COUNTYA, PLACEA, 
           pop_tot, pop_pct_men_15_34,
           pop_wht, pop_blk, pop_ami, pop_api, pop_lat,
           pop_pct_pov, pop_pct_deep_pov,
           pop_med_income, pop_pc_income)%>%
    mutate(year_range = "2012-2016")
  return(out)
}

pop_place<-bind_rows(make_pop_nhgis07_11(
  read_csv("./data/nhgis/nhgis0031_ds184_20115_2011_place.csv")),
                     make_pop_nhgis12_16(
                       read_csv("./data/nhgis/nhgis0031_ds225_20165_2016_place.csv")))

pop_cnty<-bind_rows(make_pop_nhgis07_11(
  read_csv("./data/nhgis/nhgis0031_ds184_20115_2011_county.csv")),
  make_pop_nhgis12_16(
    read_csv("./data/nhgis/nhgis0031_ds225_20165_2016_county.csv")))

###### join order - fe->fin->pop->ucr
###### fe is complete relative to governments in data
fin_local<-fin_local%>%
  mutate(
    year_range = case_when(
      year>=2007 & year<2011 ~ "2007-2011",
      year>=2012 & year<2016 ~ "2012-2016"
    ))

#### county govs have a place code, but won't match the pop file  
#### need to make separate files for muni, county govs
### type_code 0 = state, 1 = county, 2 = muni, 3 = township
### 1 and 3 not matching at all to pop data
### can I recover a different fips_place code with the gov or ucr data?

#### making one table just for munis, counties and villages 
#### can get swept up in the aggregate table
#### filter out units with 0 reported revenue
#### filter out those not in UCR, only 13 cases with UCR reports and >0 deaths
#### maybe I should impute there... robustness check later
#### filtering to munis with at least 500, 2nd pctile of the data

dat_gov_muni<-fe_govid_ts%>%
  ungroup()%>%
  mutate(GOVID = as.numeric(GOVID))%>%
  left_join(fin_local%>%
              select(-year))%>%
  filter(type_code==2)%>%
  left_join(pop_place%>%
              rename(fips_st = STATEA,
                     fips_place = PLACEA)%>%
              select(-COUNTYA))%>%
  left_join(crime_govid_ts)%>%
  filter(rev_gen_ownsource>0)%>%
  filter(!(is.na(property.yr)))%>%
  filter(pop_tot>500)
  
### second table by county by gov type (muni, county, village)

dat_cnty<-fe_govid_ts%>%
  ungroup()%>%
  mutate(GOVID = as.numeric(GOVID))%>%
  left_join(fin_local%>%
              select(-year))%>%
  group_by(year_range, fips_st, fips_cnty, type_code)%>%
  select(-gov_name, -fips_place, -GOVID)%>%
  summarise_all(sum)%>%
  filter(!(is.na(type_code)))%>%
  left_join(pop_cnty%>%
              rename(fips_st = STATEA,
                     fips_cnty = COUNTYA)%>%
              select(-PLACEA))%>%
  mutate(FIPS = as.numeric(paste(fips_st, fips_cnty, sep="")))%>%
  left_join(crime_cnty_ts)%>%
  filter(!(is.na(property.yr)))%>%
  filter(rev_gen_ownsource>0)%>%
  filter(!(is.na(pop_tot)))%>%
  select(-FIPS)

dat_cnty_nogov<-fe_govid_ts%>%
  ungroup()%>%
  mutate(GOVID = as.numeric(GOVID))%>%
  left_join(fin_local%>%
              select(-year))%>%
  group_by(year_range, fips_st, fips_cnty)%>%
  select(-gov_name, -fips_place, -GOVID, -type_code)%>%
  summarise_all(sum)%>%
  left_join(pop_cnty%>%
              rename(fips_st = STATEA,
                     fips_cnty = COUNTYA)%>%
              select(-PLACEA))%>%
  mutate(FIPS = as.numeric(paste(fips_st, fips_cnty, sep="")))%>%
  left_join(crime_cnty_ts)%>%
  filter(!(is.na(property.yr)))%>%
  filter(rev_gen_ownsource>0)%>%
  filter(!(is.na(pop_tot)))%>%
  select(-FIPS)


# NOTE: CoG vars are in $1,000s, unadjusted
# CPI inflation: $1_2007 = $1.24_2018; $1_2012 = $1.11_2018
# set up budget/income vars

infl<-data.frame(year_range = c("2007-2011", "2012-2016"), 
                 infl = c(1.24, 1.11),
                 stringsAsFactors = FALSE)

cog_vars<-names(dat_gov_muni)[which(names(dat_gov_muni)=="exp_tot"):
                                which(names(dat_gov_muni)=="rev_tax")]

dat_gov_muni<-dat_gov_muni%>%
  left_join(infl)%>%
  mutate_at(vars(cog_vars), funs(. * infl * 1000))%>%
  mutate(pop_med_income = pop_med_income * infl,
         pop_pc_income = pop_pc_income * infl)%>%
  select(-infl)

dat_cnty<-dat_cnty%>%
  left_join(infl)%>%
  mutate_at(vars(cog_vars), funs(. * infl * 1000))%>%
  mutate(pop_med_income = pop_med_income * infl,
         pop_pc_income = pop_pc_income * infl)%>%
  select(-infl)

dat_cnty_nogov<-dat_cnty_nogov%>%
  left_join(infl)%>%
  mutate_at(vars(cog_vars), funs(. * infl * 1000))%>%
  mutate(pop_med_income = pop_med_income * infl,
         pop_pc_income = pop_pc_income * infl)%>%
  select(-infl)
  
#### END MERGE, PROCEED TO MODELS

write_csv(dat_gov_muni, "./data/merged_dat_gov_muni.csv")
write_csv(dat_cnty_nogov, "./data/merged_dat_cnty.csv")
write_csv(dat_cnty, "./data/merged_dat_cnty_by_gov.csv")