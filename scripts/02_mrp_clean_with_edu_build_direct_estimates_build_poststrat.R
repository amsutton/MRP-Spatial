#### MRP + Spatial Priors using CTIS Data ####

#Script 2: Clean data, build survey direct estimates, and poststratification data 

#Build a series of models representing vaccination probability (min. 1 dose)
#For June 12, 2021 using the COVID-19 Trends and Impacts Survey (CTIS)

#Post-stratification of age/sex/education at ZCTA level
#potential secondary post-stratification using urban/rural classification later

pacman::p_load(tidyverse, tidycensus, haven, here, janitor, srvyr, sf, spdep)


options(tigris_use_cache = TRUE)

tidycensus::census_api_key(Sys.getenv("CENSUS_API_KEY"), overwrite = FALSE, install = FALSE)

here::i_am("scripts/02_mrp_clean_with_edu_build_direct_estimates_build_poststrat.R")


#### set geographic and model string specifications for intelligent labeling ####
state_choice <- "CA"
state <- "California"
whole_time_period <- FALSE


#### read/clean data: ctis 3, wave 11, june 2021 ####
ctis <- read.csv(file=here::here("data/ctis_data_do_not_share/ctis_w11_2021-06.csv"),
                 header = TRUE, colClasses=c("fips"="character", "A3"="character"))

ctis <- ctis %>%
  filter(!is.na(V1)) %>%
  mutate(
    xfips = fips,
    xzip = A3,
    sex = D1,
    age = D2,
    edu = D8,
    mask = C14a,
    vax = V1) %>%
  select(StartDatetime, EndDatetime, weight, xzip,xfips, sex, age, edu, vax) %>%
  na.omit() %>%
  # mutate(state=NA) %>%  # we don't have state, but we do have zip code and fips code which we use to get state
  separate(StartDatetime, c("start_date", "start_time"), sep = " ") %>%
  separate(EndDatetime, c("end_date", "end_time"), sep = " ")

# if(whole_time_period == FALSE){
#   #only May 22 to June 12 to match the CDC data
#   ctis <- ctis %>%
#     filter(start_date < as.Date("2021-06-13")
#   
# }

#make it binary for binomial models
#set 3 == i don't know to NA (nb:1=yes, 2=no)
ctis <- ctis %>%
  mutate(vax= case_when(vax==1 ~ 1,
                        vax==2 ~ 0,
                        vax==3 ~ NA_real_))


#identify state by county fips codes; 
#more information to join geographically on later if needed
data("fips_codes")

fips_codes <- fips_codes %>% 
  filter(state == state_choice) %>%
  mutate(xfips = paste0(state_code,county_code, sep=""))

#### match ctis data categories to ACS bins ####

#filter sex to ONLY male or female (male = 1, female = 2) 
#because eventually we'll use MRP w/ the ACS data as post-strat matrix
#and ACS only counts male and female, and they must match.
ctis <- ctis %>%
  filter(sex == 1 | sex == 2) %>%
  mutate(sex = ifelse(sex == 1, 1, 0))


#to match the ACS categories listed later in this script under object "acs"
# ctis <- ctis %>%
#   mutate(age = case_when(
#     age == 1 ~ 1, #18-24
#     age == 2 ~ 2, #25-34
#     age == 3 ~ 3, #35-44
#     age == 4 ~ 4, #45-54
#     age == 5 ~ 4, #55-64
#     age == 6 ~ 5, #65-74
#     age == 7 ~ 5  #75+
#   ))


#to match the ACS And the basic assumptions of the CDC baselines in file:
#01b_establish_baseline_proportions_cadoh_data.R

ctis <- ctis %>%
  mutate(age = case_when(
    age == 1 ~ 1, #18-24
    age == 2 ~ 2, #25-34
    age == 3 ~ 2, #35-44
    age == 4 ~ 2, #45-54
    age == 5 ~ 2, #55-64
    age == 6 ~ 2, #65-74
    age == 7 ~ 3  #75+
  ))


#to match acs education values
ctis <- ctis %>%
  mutate(edu = ifelse(edu==6 | edu == 7 | edu == 8, 6, edu))


#filter ctis such that only the fips codes present in fips_codes are left; 
#in this case, we are approximating California
dat <- ctis %>% filter(str_starts(xfips, "06")) #I checked with the tigris object filter and it's still 57 counties; missing one!

#sanity check: it is a reasonable number of FIPS codes for the context
length(unique(dat$xfips)) # == missing a county, 57 out of 58

#### get county geographies using tigris ####
#county are only available by state for 2000 and 2010
geo <- tigris::counties(cb=FALSE, year= 2010, state= state_choice) %>% clean_names()

dat <- dat %>%
  select(weight, xfips, vax, sex, age, edu) 

#quick check: looks good
dat %>% group_by(vax) %>% summarize(n())

#### Build Post-stratification Matrix using ACS ####

#sex by age estimates for state of California
acs <- get_acs(geography = "county",
               state = "CA",
               table = "B15001",
               survey = "acs5",
               geometry = FALSE,
               year = 2020)

vars <- load_variables(2020, "acs5", cache=TRUE)
vars = vars %>% 
  filter(str_detect(name,"B15001"),
         str_detect(label, "18 to 24 years:!!|25 to 34 years:!!|35 to 44 years:!!|45 to 64 years:!!|65 years and over:!!")) 


#education using original table
vars <- vars %>%
  mutate(edu = case_when(str_detect(label, "than 9th") ~ 1, #less than high school
                         str_detect(label, "no diploma") ~ 1, #less than high school
                         str_detect(label, "(includes equivalency)") ~ 2, #high school or equivalent
                         str_detect(label, "no degree") ~ 3, #some college
                         str_detect(label, "years:!!Associate's degree") ~ 4,
                         str_detect(label, "years:!!Bachelor's degree") ~ 5,
                         str_detect(label, "professional degree") ~ 6)) %>% #professional or graduate degree
  na.omit() #to drop the variables we don't want

acs <- left_join(vars, acs,  by=c("name"="variable"))

acs <- acs %>%
  mutate(sex = ifelse(str_detect(label, "Male"), 1, 0))  # 1= male, 0= female, like in ctis

acs <- acs %>%        
  mutate(age = case_when(str_detect(label, "18") ~ 1,
                         str_detect(label, "25|35|45") ~ 2,
                         str_detect(label, "65") ~ 3 # a concession; doesn't match previous groups that are 75+
  )) %>%
  filter(!str_detect(concept, "ALONE|HISPANIC|RACES"))

#this is the post-strat table; it has multiples in some categories 
#which we handle later when we build post and pred_strat
acs <- acs %>%
  clean_names() %>%
  rename(fips = geoid) %>%
  select(fips, sex, age, edu, estimate)

acs <- acs %>%
  group_by(fips, sex,age, edu) %>%
  summarize(estimate=sum(estimate)) %>%
  ungroup() %>%
  distinct()


#### Direct Estimates ####
#get direct estimates of masking in dat by zcta w/ CI

c1 <- dat 

c1 = c1 |>
  mutate(sex = ifelse(sex == 1, "Male", "Female"),
         vax = ifelse(vax == 1, "Yes", "No"))

direct  = c1 |>
  as_survey_design(1, weight = weight, variables = c(vax, age,sex,edu))

direct = direct |>
  group_by(vax,age,sex,edu) |>
  summarize(mean = survey_mean(,var = "ci", na.rm = TRUE, level = 0.95))

direct = na.omit(direct)

direct = direct %>%
  mutate(age = case_when(age == 1 ~ "18-24 years",
                         age == 2 ~ "25-64 years",
                         age == 3 ~ "65 years and over"),
         edu = case_when(edu == 1 ~ "less than high school",
                         edu == 2 ~ "high school or equivalent",
                         edu == 3 ~ "some college",
                         edu == 4 ~ "associate's degree",
                         edu == 5 ~ "bachelor's degree",
                         edu == 6 ~ "professional or graduate degree"),
         mean = round(mean, digits = 3),
         mean_low = round(mean_low, digits = 3),
         mean_upp = round(mean_upp, digits = 3),
         ci = paste(mean_low,mean_upp,sep=", "))


write.csv(direct, here("data/direct_estimates/ctis_vax_direct_estimates_not_boostrapped_with_edu.csv"))


#### Build Adjacency Matrix ####

#compute adjacency matrix and make sure both column and row names correspond 
#to the zcta names
# geo$zcta <- as.factor(geo$zcta)
geo$xfips = as.factor(geo$geoid)

#we must assign the zctas an id 1:nrow(geo) because the adjacency matrix only works
#with integers 1:n -- and zctas aren't like that! NB: the 1:n order is artificial
#and needn't relate to the order of the zctas; it's simply important to have them
#in this format.
geo$id <- 1:nrow(geo) 
geo$geoid10
#make an index of id-to-zcta so we can refer to and use it later
index <- geo %>% 
  # select(id, zcta) %>% 
  rename(fips = geoid10) %>%
  select(id, fips) %>%
  mutate(id = as.integer(factor(id)))



#### Build Data for Poststratification: No spatial id, no edu ####
# #make sure acs has id-to-zcta so can identify between later
# acs <- left_join(acs, index, by = "zcta")

#poststratification data: combine variables where because of recoding, 
#there are multiples (i.e. collapse strata that now are identical across id, 
#sex, age, edu due to recode)

post <- acs %>%
  group_by(fips, sex,age,edu) %>%
  summarize(estimate = sum(estimate)) %>%
  select(fips, sex, age, edu, estimate) %>%
  ungroup() #double-check no grouping


#Make sure nothing is a factor; you can assign in the formula later; tidy up data
dat <- dat %>%
  # select(weight, id, vax, sex, age, edu) %>% # don't need weights in full Bayesian!
  select(xfips, vax, sex, age,edu) %>%
  mutate(age = as.integer(age))

dat = dat %>% na.omit()

# #write graph, acs, dat and post object to rda
# save(geo, file=here("Aja/COVID_Behaviors/data/smrp/cleaned_data/ca_geo_sf_county.rda"))
save(dat, file = here("data/cleaned_input_data/clean_data_county_with_edu.rda"))
save(post, file = here("data/cleaned_input_data//clean_postrat_age_sex_county_with_edu.rda"))

