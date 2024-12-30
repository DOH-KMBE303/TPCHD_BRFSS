#BRFSS DATA IN R--------------------------------------------------

#This script uses Washington BRFSS data to calculate estimated counts and percents 
#of 5-year disease prevalence in two Tacoma subareas: the tideflats, and the South 
#Tacoma Groundwater Protection District. These metrics cover each of three 
# geographies: individual zip codes, aggregated subareas, and the county overall. 
# This project was undertaken in service of two Health Impact Assessments being 
# conducted in the two subareas mentioned above.

#NOTE 12/30/2024: Error still occuring when attempting to calculate zip-code-level
#metrics. Troubleshooting ongoing. Likely related to small sample size/lonely
#primary sampling units (PSUs)

#Contact Kendall.Billig@doh.wa.gov with questions about this script.

##INPUTS:------------------------------------------------
#1. WA_BRFSS_2011_2023_v2.csv, located in the Y drive for DOH staff (TCPHD staff 
# have their own internal access)

##LIBRARIES---------------------------------------------------------------------

library(tidyverse)
library(survey)

#READ IN DATA-------------------------------------------------------------------

brfss <- read.csv("C:/Users/kmbe303/OneDrive - Washington State Executive Branch Agencies/BES/EHAT/BE Data/WA_BRFSS_2011_2023_v2.csv")

#CLEAN UP VARS OF INTEREST------------------------------------------------------

brfss_recode <- brfss %>%
  #recoding asthma variable
  mutate(use_asth = case_when(X_casthm1 == 1 ~ 0, #no
                              X_casthm1 == 2 ~ 1, #yes
                              X_casthm1 == 9 ~ NA, #don't know
                            TRUE ~ NA), 
         #anxiety recoding
         use_anx = case_when(misnervs == 1 |
                               misnervs == 2 ~ 1, #all or most of the time = yes
                             misnervs == 7 |      #don't know or refused = NA
                               misnervs == 9 ~ NA,
                             is.na(misnervs) ~ NA, #if NA (years where question wasn't asked) then keep NA so it can be dropped in filter
                             TRUE ~ 0), #all other responses = no
         #depression recoding
         use_depr = case_when(misdeprd == 1 |
                                misdeprd == 2 ~ 1, #all or most of the time = yes
                              misdeprd == 7 |
                                misdeprd == 9 ~ NA, #don't know or refused = NA
                              is.na(misdeprd) ~ NA,
                              TRUE ~ 0),            #all other responses = no
         #diabetes recoding
         use_diab = case_when(diabete4 == 1 |
                                diabete4 == 2 ~ 1, #yes or gestational = yes
                              diabete4 == 3 |
                                diabete4 == 4 ~ 0, #no or prediabetes only = no
                              TRUE ~ NA),      #all others (DK/R) = NA
         #hypertension recoding - need to collapse 5 and 6 bc X is coded incorrectly
         use_hype = case_when(X_rfhype5 == 1 &
                                is.na(X_rfhype6) ~ 0, #calculated htn 2011-2019 = no
                              X_rfhype5 == 2 &
                                is.na(X_rfhype6) ~ 1, #calculated htn 2011-2019 = yes
                              X_rfhype6 == 1 &
                                is.na(X_rfhype5) ~ 0, #calculated htn 2021-2023 = no
                              X_rfhype6 == 2 &
                                is.na(X_rfhype5) ~ 1, #calculated htn 2021-2023 = no
                              TRUE ~ NA),        #all others = NA
         #heart attack recoding
         use_mi = case_when(cvdinfr4 == 1 ~ 1, #ever told mi = yes
                            cvdinfr4 == 2 ~ 0, #ever told mi = no
                            TRUE ~ NA),         #all others = NA
         #stroke recoding
         use_strk = case_when(cvdstrk3 == 1 ~ 1,
                              cvdstrk3 == 2 ~ 0,
                              TRUE ~ NA),
         #new weighting var as Cam defined it
         use_5yr = use_wt/5)

#FILTER TO TIME OF INTEREST----------------------------------------

#identify years
years <- c(2019, 2020, 2021, 2022, 2023)

##execute filtering--------------------

#filter years and cols first
brfss_filt <- brfss_recode %>%
  #filter to 2019-2023
  filter(year %in% years) %>%
#select cols of interest for readability - not necessary just preference
select(year,
       seqno,
       X_ststr,
       ctycode2,
       zipcode,
       X_casthm1,
       use_asth,
       misnervs,
       use_anx,
       misdeprd,
       use_depr,
       diabete4,
       use_diab,
       X_rfhype5,
       X_rfhype6,
       use_hype,
       cvdinfr4,
       use_mi,
       cvdstrk3,
       use_strk,
       use_wt,
       use_5yr)


#SURVEY PACKAGE IMPLEMENTATION--------------------------------------------------

#this needs to be executed when PSU = 1 which can happen with filtering
#different options for handling it, seems like this one is most common
options(survey.lonely.psu = "adjust")

##CUSTOM FUNCITON DEFINING-----------------------------------------------------

# Define the function
calculate_survey_metrics <- function(data, filter_column, filter_value, health_condition, aggregate = FALSE) {
  if (aggregate) {
    # Filter data for an aggregate collection of zip codes
    filtered_data <- data %>%
      filter(!!sym(filter_column) %in% filter_value)
  } else {
    # Filter data for an individual value
    filtered_data <- data %>%
      filter(!!sym(filter_column) == filter_value)
  }
  
  # psu_count_per_stratum <- table(filtered_data$X_ststr)
  # strata_to_include <- names(psu_count_per_stratum[psu_count_per_stratum > 1])
  # filtered_data <- filtered_data %>%
  #   filter(X_ststr %in% strata_to_include)1 
  
  # Create a survey design object
  design <- svydesign(
    ids = ~1,       # Replace with your Primary Sampling Unit column
    strata = ~X_ststr, # Replace with your strata column
    weights = ~use_5yr, # Replace with your weights column
    data = filtered_data,
    survey.lonely.psu = "adjust"
  )
  
  # Calculate total count and percentage for the health condition
  total_count <- svytotal(~get(health_condition),
                          design,
                          na.rm = TRUE)
  #can use mean for percentage because all our variables are binary (0 or)
  percent <- svymean(~get(health_condition), 
                     design,
                     na.rm = TRUE) * 100
  
  # Calculate standard errors and confidence intervals
  ci <- confint(percent) 
  ci_lower <- ci[1]
  ci_upper <- ci[2]
  
  # Return results as a data frame row
  data.frame(
    grouping_type = ifelse(aggregate, "zip_code_collection", filter_column),
    group_value = ifelse(aggregate, paste(filter_value, collapse = "_"), filter_value),
    health_condition = health_condition,
    total_count = as.numeric(total_count),
    percent = as.numeric(percent),
    ci_lower = as.numeric(ci_lower), # Lower Confidence Interval
    ci_upper = as.numeric(ci_upper)  # Upper Confidence Interval
  )
}

##DEFINE FILTERING AND LOOPING CRITERIA-----------------------------------------

# Define geographies and health conditions
pierce_cty <- 53 #pierce

#identify tideflats zips
zips_tideflats <- c(98402,
                    98403,
                    98404,
                    98405,
                    98418,
                    98421,
                    98422,
                    98424,
                    98443)

#identify stgpd zips
zips_stgpd <- c(98402,
                98405,
                98408,
                98409,
                98411,
                98413,
                98418,
                98444,
                98465,
                98466,
                98467,
                98499)

#list of all zips - this is for individual zip calculations
all_zips <- unique(c(zips_tideflats, 
                     zips_stgpd))

#names of health cols we want to calculate on
health_conditions <- c("use_asth", 
                       "use_anx", 
                       "use_depr", 
                       "use_hype", 
                       "use_mi",
                       "use_strk") 

##RUN FUNCTION------------------------------------------------------------------

# Initialize an empty data frame to store results
all_results <- data.frame()

# Run analysis for counties
for (county in pierce_cty) { #loop through county/counties
  for (cond in health_conditions) { #loop through health conditions
    result <- calculate_survey_metrics(brfss_filt, "ctycode2", county, cond)
    all_results <- rbind(all_results, result)
  }
}

# Run analysis for aggregate zip code collections
for (zip_collection in list(zips_tideflats, zips_stgpd)) { #loop through aggregated zips
  for (cond in health_conditions) { #loop through health conditions
    result <- calculate_survey_metrics(brfss_filt, "zipcode", zip_collection, cond, aggregate = TRUE)
    all_results <- rbind(all_results, result)
  }
}

###ERROR I'M STILL WORKING ON---------------------------------------------------

# Run analysis for individual zip codes - this is still throwing an error but I don't think we'll be using this data anyway?
#it pops out a couple zips before erroring out
for (zip in all_zips) { #loop through each zip
  for (cond in health_conditions) { #loop through health conditions
    result <- calculate_survey_metrics(brfss_filt, "zipcode", zip, cond)
    all_results <- rbind(all_results, result)
  }
}

#OPTIONAL WRITE TO FILE---------------------------------------------------------

# Optional: Save the results to a CSV file
write.csv(all_results, "C:/Users/kmbe303/OneDrive - Washington State Executive Branch Agencies/BES/EHAT/BE Data/all_geographies_results.csv", 
          row.names = FALSE)
