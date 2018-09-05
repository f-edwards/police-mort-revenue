################
## Descriptives for police-mort-revenue
##
## Exploratory data analysis and data vis for police-mort-revenue
## two predictors of interest: rev_fines / pop_tot; rev_fines / rev_gen_ownsource
## all money values are inflated to uniform 2018 dollars
## outcome of interest: deaths per capita
## may also want to consider regional spillover neighboring munis
## interactions between metro-level segregation, local racial composition

rm(list=ls())

library(MASS)
library(xtable)
library(tidyverse)
library(lme4)
library(brms)

## read tables created by merge.r, read_ucr.r
## tables include FE mortality, GoG finance, UCR crimes known, and ACS 5yr pop

### GOVID to name crosswalk

gov_id<-read_fwf("./data/Fin_GID_2012.txt", 
                 fwf_widths(
                   c(14, 64, 35, 2, 3, 5, 9, 2, 7, 2, 2, 2, 4, 2),
                   c("gov_id", "gov_name", "cnty_name",
                     "fips_st", "fips_cnty", "fips_place", 
                     "pop", "pop_yr", "enroll", "enroll_yr", "funct_code",
                     "school_code", "fiscal_year", "survey_year")))%>%
  mutate(type_code = substr(gov_id, 3, 3))%>%
  mutate(GOVID = as.numeric(substr(gov_id, 1, 9)))

### ferguson govid: 262095032

### get cdc ur codes
ur<- read_fwf(file = './data/NCHSURCodes2013.txt', 
               fwf_positions(c(1, 3, 7, 10, 47, 98, 107, 116, 118, 120), 
                             c(2, 5, 8, 45, 96, 105, 114, 116, 118, 120))) %>%
  select(X1, X2, X3, X4, X8) %>%
  rename(fips_st = X1, fips_cnty = X2, state = X3, county = X4, ur.code = X8) %>%
  mutate(county = tolower(county),
  ur.code = ifelse(ur.code == 1, '1: Large central metro',
                   ifelse(ur.code == 2, '2: Large fringe metro',
                          ifelse(ur.code == 3, '3: Medium metro',
                                 ifelse(ur.code == 4, '4: Small metro', 
                                        ifelse(ur.code == 5, '5: Micropolitan', 
                                               ifelse(ur.code == 6, '6: Noncore', NA)))))))%>%
  mutate(fips_cnty = ifelse(fips_cnty == "113" & fips_st=="46", "102", fips_cnty)) %>%
  mutate(fips_cnty = ifelse(fips_cnty == "270" & fips_st =="02", "158", fips_cnty))

muni<-read_csv("./data/merged_dat_gov_muni.csv")%>%
  left_join(ur)
cnty<-read_csv("./data/merged_dat_cnty.csv")
cnty_bygov<-read_csv("./data/merged_dat_cnty_by_gov.csv")

# construct outcome variables for muni and county tables
# cnty_bygov is long, by gov type (1 = county, 2 = muni, 3 = village/township)

make_predictors<-function(x){
  x<-x%>%
    mutate(rev_fines_pc = rev_fines / pop_tot,
           rev_fines_pct_rev = rev_fines / rev_gen_ownsource,
           death_pc = deaths / pop_tot * 1e5,
           violent_pc = violent.yr / pop_tot,
           rev_taxes_pc = rev_tax / pop_tot,
           rev_proptaxes_pc = rev_prop_tax / pop_tot,
           exp_police_pc = exp_police / pop_tot,
           pop_pct_blk = pop_blk / pop_tot,
           pop_pct_ami = pop_ami / pop_tot,
           pop_pct_lat = pop_lat/ pop_tot,
           pop_nonwht = 1 - pop_wht / pop_tot,
           officers_pc = ft_sworn / pop_tot)
  return(x)
}

muni<-make_predictors(muni)

cnty<-make_predictors(cnty)

cnty_bygov<-make_predictors(cnty_bygov)

##################################
## Municipal descriptives
## possibilities
## density of kilings by rev pctile or some other categorical cut?
## some visual on revenues by something, maybe crime? property tax? income per cap?


##### FINES AS PERCENT OF OWN-SOURCE REVENUES
##### DESCRIPTIVE STATISTICS AND HISTOGRAMS 
##### MUNI LEVEL

desc_tab<-muni%>%
  filter(year_range=="2012-2016")%>%
  group_by(ur.code)%>%
  summarise(mean = mean(rev_fines_pct_rev),
            median = median(rev_fines_pct_rev),
            sd = sd(rev_fines_pct_rev),
            pct_90 = quantile(rev_fines_pct_rev, 0.9),
            pct_95 = quantile(rev_fines_pct_rev, 0.95))

ggplot(muni%>%
         filter(year_range=="2012-2016",
                rev_fines_pct_rev<0.25),
       aes(x = rev_fines_pct_rev))+
  geom_histogram() + 
  facet_wrap(~ur.code, scales = "free_y") + 
  ggtitle("Fines as percent of municipal\nown-source revenue by county metro type") + 
  xlab("Fine revenue / General own-source revenue") + 
  ylab("") + 
  ggsave("./vis/fines_pct_rev_hist_ur.png")

ggplot(muni%>%
         filter(year_range=="2012-2016",
                rev_fines_pc<100),
       aes(x = rev_fines_pc))+
  geom_histogram()+ 
  facet_wrap(~ur.code) + 
  ggtitle("Fine revenue per capita by county metro type") + 
  ggsave("./vis/fines_pc_hist_ur.png")

### deaths visual
### lump for full period, break into 0, 1, then bins
### also do a per cap histogram with higher pop threshold
# 
# muni_cross_period<-muni%>%
#   group_by(GOVID)%>%
#   summarise(deaths = sum(deaths))%>%
#   left_join(muni%>%
#             filter(year_range == "2012-2016")%>%
#               select(GOVID, pop_tot))%>%
#   mutate(high_pop = ifelse(pop_tot>75000, "More than 75,000 people",
#                            "75,000 people or less"))%>%
#   mutate(deaths_pc = deaths / pop_tot * 1e5)%>%
#   filter(!(is.na(high_pop)))

ggplot(muni,
       aes(x = deaths)) + 
  geom_histogram() + 
  facet_wrap(~ur.code, scales = "free") + 
  xlab("People killed by police by municipality (2007 - 2016)") + 
  ggsave("./vis/deaths_hist.png")

ggplot(muni,
       aes(x = deaths_pc)) + 
  geom_histogram() + 
  facet_wrap(~ur.code, scales = "free") + 
  xlab("People killed by police per 100,000 persons by municipality (2007 - 2016") + 
  ggsave("./vis/deaths_pc_hist.png")

#### lots of zeroes on death rates
cnty_cross_period<-muni%>%
  group_by(fips_st, fips_cnty)%>%
  summarise(deaths = sum(deaths))%>%
  left_join(cnty%>%
              filter(year_range == "2012-2016")%>%
              select(fips_st, fips_cnty, pop_tot))%>%
  mutate(high_pop = ifelse(pop_tot>75000, "More than 75,000 people",
                           "75,000 people or less"))%>%
  mutate(deaths_pc = deaths / pop_tot * 1e5)%>%
  filter(!(is.na(high_pop)))

#############################################
### TABLES FOR FINES PC, FINES PCT BY IN MSA

### deaths visual
### lump for full period, break into 0, 1, then bins
### also do a per cap histogram with higher pop threshold

msa_cross_period<-muni%>%
  group_by(cbsa)%>%
  summarise(deaths = sum(deaths))%>%
  left_join(muni%>%
              filter(year_range == "2012-2016")%>%
              select(cbsa, pop_tot)%>%
              group_by(cbsa)%>%
              summarise(pop_tot = sum(pop_tot)))%>%
  mutate(deaths_pc = deaths / pop_tot * 1e5)

ggplot(msa_cross_period,
       aes(x = deaths)) +
  geom_histogram() +
  xlab("People killed by police by MSA (2007 - 2016)") +
  ggsave("./vis/deaths_hist.png",
         width = 6,
         height = 5)

ggplot(msa_cross_period,
       aes(x = deaths_pc)) +
  geom_histogram() +
  xlab("People killed by police per 100,000 persons by MSA (2007 - 2016)") +
  ggsave("./vis/deaths_pc_hist.png",
         width = 6,
         height = 5)

### COUNTY MAPS?
### MAYBE WORK UP A CHOROPLETH WITH ALPHA = log(pop), color = death.rt

### how many fergusons?
### pct own source > 0.2, per cap > $100

fergusons<-muni%>%
  filter(rev_fines_pc>=100 | rev_fines_pct_rev>=0.2,
         year_range == "2012-2016")

write_csv(fergusons%>%
            select(gov_name, state, pop_tot, rev_fines, rev_prop_tax, rev_tot, rev_gen_ownsource),
          "./vis/fergusons_rev_coding.csv")

