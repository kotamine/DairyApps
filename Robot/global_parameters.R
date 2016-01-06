
# Loaded once. Shared across all sessions.

conv_factor <- 2.2046  # conversion factor from kg to pound 

list_inputs_shared <- c("herd_size","milk_cow_day","scc_average","hours_milking","hr_heat_detection",
                        "price_milk","scc_premium","cost_DM","cost_heifer","cull_price","labor_rate",
                        "labor_rate_rc_mgt","inflation_robot","inflation_margin","inflation_labor",
                        "hurdle_rate","tax_rate","dep_method")

list_inputs_feed <- c("milk_cow_coeff","milk_fat","milk_fat_coeff","adj_milk_cow_coeff",
                      "body_weight_coeff1","body_weight_coeff2","lcatation_week",
                      "lactation_coeff1","lactation_coeff2")
  
list_inputs_profile <- c("herd_increase","additional_labor","additional_cost", "cost_robot","n_robot",
                         "cost_parlors","cost_housing_cow","repair","useful_years", "n_sets",
                         "salvage_milking1","insurance_rate","milk_change","pellets","cost_pellets",
                         "scc_change","software","change_turnover","hr_sv_milking",
                         "anticipated_hours_heat","increase_rc_mgt","decrease_lab_mgt",
                         "change_electricity","change_water","change_chemical","yr_system1",
                         "delay_housing1","down_housing","down_milking1","down_milking2",
                         "r_housing","r_milking1","n_yr_housing","n_yr_milking1","n_yr_milking2")

myRead.xlsx <- function(file, sheetIndex, rownameIndex=1, stringsAsFactors=FALSE) {
  df <- read.xlsx(file, sheetIndex = sheetIndex, stringsAsFactors=stringsAsFactors) 
  rownames(df) <- df[,rownameIndex]
  df #<- df[,-rownameIndex]
}

