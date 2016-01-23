
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

ldquo <- HTML("&ldquo;")
rdquo <- HTML("&rdquo;")
emquo <- function(txt) em(HTML(paste0("&ldquo;",txt,"&rdquo;"))) 


myRead.xlsx <- function(file, sheetIndex, rownameIndex=1, stringsAsFactors=FALSE) {
  df <- read.xlsx(file, sheetIndex = sheetIndex, stringsAsFactors=stringsAsFactors) 
  rownames(df) <- df[,rownameIndex]
  df 
}

conv_factor <- 2.2046  # conversion factor from kg to pound 


## Load the min/max/step setting values 
common_variables_min_step <- read.xlsx("www/user_input_data_min_step.xlsx", 
                                       sheetIndex = 1, stringsAsFactors =FALSE) 
profile_specific_variables_min_step <- read.xlsx("www/user_input_data_min_step.xlsx", 
                                                 sheetIndex = 2, stringsAsFactors =FALSE)  


loc_dep_method <- which(common_variables_min_step$variable=="dep_method") 
loc_n_sets <- which(profile_specific_variables_min_step$variable=="n_sets") 

numeric_vars_min <- c(common_variables_min_step$min[-c(loc_dep_method)], 
                      profile_specific_variables_min_step$min[-c(loc_n_sets)]) %>% as.numeric()

numeric_vars_max <- c(common_variables_min_step$max[-c(loc_dep_method)], 
                      profile_specific_variables_min_step$max[-c(loc_n_sets)]) %>% as.numeric()

## Load the default setting values 
default_common_case_1 <- myRead.xlsx("www/user_input_data_case_1.xlsx", 
                                     sheetIndex = 1, stringsAsFactors =FALSE) 
default_profile_specific_case_1 <- myRead.xlsx("www/user_input_data_case_1.xlsx", 
                                               sheetIndex = 2, stringsAsFactors =FALSE) 


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

label_inputs_shared <- default_common_case_1$label[1:length(list_inputs_shared)] 

label_inputs_feed <- default_common_case_1$label[(length(list_inputs_shared)+1):length(default_common_case_1$label)]

label_inputs_profile <-  default_profile_specific_case_1$label

numeric_vars_label <- c(c(label_inputs_shared, label_inputs_feed)[-c(loc_dep_method)],  
                        label_inputs_profile[-c(loc_n_sets)])


