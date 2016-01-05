
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


# vars_selected_profile <- c("robot_parlor","profile_choice","herd_size","herd_increase",
#                            "additional_labor","additional_cost","n_robot","cost_robot",
#                            "cost_parlors", "cost_housing_cow","repair","robot_years",
#                            "n_robot_life","milking_years","salvage_milking1","insurance_rate",
#                            "hours_milking","hr_sv_milking",
#                            "hr_heat_detection", "anticipated_hours_heat", "labor_rate",
#                            "increase_rc_mgt","decrease_lab_mgt","labor_rate_rc_mgt",
#                            "price_milk","milk_cow_day","milk_change","scc_premium",
#                            "scc_average", "scc_change", "software","cost_DM",
#                            "pellets","cost_pellets","milk_cow_coeff","milk_fat",
#                            "milk_fat_coeff","adj_milk_cow_coeff","body_weight",
#                            "body_weight_coeff1", "body_weight_coeff2", "lcatation_week",
#                            "lactation_coeff1","lactation_coeff2", "culling_rate",
#                            "death_rate","cost_heifer","cull_price", "change_turnover",
#                            "change_electricity", "change_water","change_chemical",
#                            "inflation_robot","inflation_margin","inflation_labor",
#                            "interest", "hurdle_rate", "tax_rate", "dep_method",
#                            "down_housing","down_milking1","down_milking2",
#                            "r_housing","r_milking1","r_milking2","n_yr_housing",
#                            "n_yr_milking1","n_yr_milking2")


# vars_all_profiles <- c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#                        "anticipated_hours_heat","increase_rc_mgt",
#                        "decrease_lab_mgt", "milk_change","scc_change","software", 
#                        "pellets","cost_pellets","change_turnover","change_electricity",
#                        "change_water", "change_chemical",
#                        "down_housing", "down_milking1", "down_milking2",
#                        "n_yr_housing", "n_yr_milking1","n_yr_milking2",
#                        "salvage_housing", "salvage_milking1", 
#                        "milking_years",  "robot_years", "n_robot",
#                        "cost_housing_cow",  "cost_parlors", "cost_robot")
# 
# mins_vars_all_profiles <- c(rep(0,12), -10, rep(0,11), rep(1, 2), rep(0,4))
# 
# steps_vars_all_profiles <- c(10, 500, 0.1, 0.2, 
#                              0.05, 0.1,
#                              0.1, 2, 0.25, 5, 
#                              2, 2, 0.25, 0.25,
#                              0.25, 0.25,
#                              rep(20000,3), rep(1,3),rep(5000,2), rep(1,2), 
#                              rep(10000,4)) 



