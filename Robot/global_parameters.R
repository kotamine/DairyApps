
# Loaded once. Shared across all sessions.
library(shiny)
library(shinyBS)
library(rmarkdown)
library(ggplot2)
suppressPackageStartupMessages(library(googleVis))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(xlsx))
suppressPackageStartupMessages(library(XLConnect))

base_profiles <- c("Robots","Retrofit","New")
base_profiles_se <- c(outer(base_profiles, paste0("_se",c(1:10)),FUN=paste,sep=""))

# combo_profiles <- c("RetrofitRobots","RetrofitNew")
refProfileName <-  function(x) {
  if (grepl("_se", x)) {  # TRUE/FALSE for sensitivity analysis
    x <- gsub("_se\\d+","",x)
  } 
  switch(x, 
         "Robots"="Robots",
         "Retrofit"="Retrofit Parlors",
         "New"="New Parlors")
}

ems <- function(txt) em(strong(txt)) 

common_variables_min_step <- read.xlsx("www/user_input_data_min_step.xlsx", 
                                       sheetIndex = 1, stringsAsFactors =FALSE) 
profile_specific_variables_min_step <- read.xlsx("www/user_input_data_min_step.xlsx", 
                                                 sheetIndex = 2, stringsAsFactors =FALSE)  



conv_factor <- 2.2046  # conversion factor from kg to pound 

list_inputs_shared <- c("herd_size","milk_cow_day","scc_average","hours_milking","hr_heat_detection",
                        "price_milk","scc_premium","cost_DM","cost_heifer","cull_price","labor_rate",
                        "labor_rate_rc_mgt","inflation_robot","inflation_margin","inflation_labor",
                        "hurdle_rate","tax_rate","dep_method")

list_inputs_feed <- c("milk_cow_coeff","milk_fat","milk_fat_coeff","adj_milk_cow_coeff",
                      "body_weight","body_weight_coeff1","body_weight_coeff2","lcatation_week",
                      "lactation_coeff1","lactation_coeff2")

list_inputs_profile <- c("herd_increase","additional_labor","additional_cost", "cost_robot","n_robot",
                         "cost_parlors","cost_housing_cow","repair","useful_years", "n_sets",
                         "salvage_milking1","insurance_rate","milk_change","pellets","cost_pellets",
                         "scc_change","software","change_turnover","hr_sv_milking",
                         "anticipated_hours_heat","increase_rc_mgt","decrease_lab_mgt",
                         "change_electricity","change_water","change_chemical","yr_system1",
                         "delay_housing1", "delay_milk_increase", "delay_labor_saving",
                         "down_housing","down_milking1","down_milking2",
                         "r_housing","r_milking1","n_yr_housing","n_yr_milking1","n_yr_milking2")

label_inputs_shared <- c("Current herd size (milking & dry animals)",
                         "Milk per cow per day, past year (lbs/cow/day)",
                         "Current annual bulk tank average SCC (SCC/ml)",
                         "Current hours of milking & chore labor (hours/day)",
                         "Current hours of heat detection (hours per day)",
                         "Mailbox milk price ($/cwt)",
                         "SCC premium per 1,000 SCC (SCC/ml)",
                         "Cost per lb of TMR dry matter ($ per lb DM)",
                         "Cost of replacement heifer ($)",
                         "Cull price per cow ($)",
                         "Labor rate for milking and heat detection ($/hour)",
                         "Labor rate for records and labor management ($/hour)",
                         "Robot/parlor & related-housing prices (%)",
                         "Margin milk over feed & operation per cow with robots (%)",
                         "Milking & chore labor rate per hour (%)",
                         "Hurdle rate for equity (%)",
                         "Marginal (federal + state) income tax rate (%)",
                         "Depreciation accounting method")

label_inputs_feed <-  c( "Milk per cow per day",
                         "Milk fat content (%)",
                         "Coefficient for Milk fat content",
                         "Coefficient for Milk/cow/day adjusted to 4% fat",
                         "Milking herd avg body weight (lb)",
                         "Coefficient 1 for Milking herd avg body weight",
                         "Coefficient 2 for Milking herd avg body weight",
                         "Lactation weeks",
                         "Coefficient 1 for Lactation weeks",
                         "Coefficient 2 for Lactation weeks")

label_inputs_profile <-  c("Anticipated increase in milking herd (cows)",
                           "Additional labor expense with herd expansion ($/cow)",
                           "Other expense with herd expansion ($/cow)",
                           "Unit cost for Robots ($)",
                           "Number of Robots (units)",
                           "Cost for Retrofit or New Parlors ($)",
                           "Housing changes per cow ($)",
                           "Estimated annual change in milking system repair ($)",
                           "Robots useful life (years)",
                           "Sets of Robots or Parlors in planing horizon",
                           "Salvage value of Robots in today's term ($)",
                           "Insurance rate per $1000 value (%)",
                           "Projected change in milk production (lbs/cow/day)",
                           "Pellets fed in robot booth (lb/cow/day)",
                           "Extra cost for pellets fed in robot booth ($/ton)",
                           "Estimated percent change in SCC (%)",
                           "Reproduction and herd health value of software ($/cow/year)",
                           "Anticipated change in annual turnover rate (%)",
                           "Anticipated savings in milking & chore labor (hours/day)",
                           "Anticipated hours of heat detection (hours/day)",
                           "Increased hours of records management (hours/day)",
                           "Reduced hours of labor management (hours/day)",
                           "Anticipated change in electricity cost ($/cow)",
                           "Anticipated change in water cost ($/cow)",
                           "Anticipated change in chemical cost ($/cow)",
                           "Year of investment for the first set of Robots/Parlors",
                           "Delayed amount of the housing investment till Robots/Parlors installment ($)",
                           "Portion of anticipated milk increase installed with the initial housing investment (%)",
                           "Portion of anticipated labor saving installed with the initial housing investment (%)",
                           "Down payment for housing ($)",
                           "Down payment for the first set of Robots/Parlors ($)",
                           "Down payment for the second set of Robots/Parlors ($)",
                           "Interest rate for housing loan (%)",
                           "Interest rate for robots/parlors loan (%)",
                           "Loan period for housing (year)",
                           "Loan period for the first set of robots/parlors (years)",
                           "Loan period for the second set of robots/parlors (years)")


myRead.xlsx <- function(file, sheetIndex, rownameIndex=1, stringsAsFactors=FALSE) {
  df <- read.xlsx(file, sheetIndex = sheetIndex, stringsAsFactors=stringsAsFactors) 
  rownames(df) <- df[,rownameIndex]
  df 
}

