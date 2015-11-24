
# Loaded once. Shared across all sessions.

conv_factor <- 2.2046  # conversion factor from kg to pound 

input_colnames <- c("input_id", "milk_cow_day","milk_change") # UNUSED 


c_colnames <- c("Variable", "% Change","Base Value","New Value",
                "Net Impact","diff: Net Impact", 
                "Milk minus Feed", "diff: Milk minus Feed",
                "Labor plus Repair", "diff: Labor plus Repair",
                "Capital Cost", "diff: Capital Cost",
                "Others", "diff: Others",
                "Inflation Adj.", "diff: Inflation Adj.",
                "IFOC.gain cow", "diff: IOFC.gain cow",
                "IFOC.gain cwt", "diff: IOFC.gain cwt",
                "Breakeven Wage","diff: Wage",
                "Break.Wage Inflation","diff: Break.Wage Inflation")


c_noncurrency <- c("Variable", "% Change","Base Value","New Value",
                   "Break.Wage Inflation","diff: Break.Wage Inflation")

c_varnames <- c("cost_robot","cost_housing_cow", "repair",
                "robot_years","salvage_milking1", "hr_sv_milking",
                "milk_change")

c_labels <- c("Estimated cost per robot", "Related housing changes needed per cow",
              "Estimated annual change in milking system repair",
              "Robots: years of useful life",
              "Value of robots after useful life",
              "Anticipated savings in milking & chore labor",
              "Projected change in milk production") 

c_varnames_parlor <- c("cost_parlors","cost_housing_cow", "repair",
                       "milking_years","salvage_milking1", "hr_sv_milking",
                       "milk_change")

c_labels_parlor <- c("Estimated cost of parlors", "Related housing changes needed per cow",
                     "Estimated annual change in milking system repair",
                     "Parlors: years of useful life",
                     "Salvage value of parlors after useful life",
                     "Anticipated savings in milking & chore labor",
                     "Projected change in milk production") 

s_varnames <- c("cost_robot","cost_housing_cow", "milk_change",
                "scc_change","pellets")

# s_labels <- c("Estimated cost per robot", "Related housing changes needed per cow",
#               "Projected change in milk production",
#               "Estimated percent change in SCC (%)", "Pellets fed in robot booth") 

s_labels <- c("Increased investment",
              "Use less pellets",
              "New barn ($120k/stall)")

s_colnames <- c("Scenario", 
                "%.change: robot", "%.change: housing", "%.change: milk",
                "%.change: scc", "%.change: pellets",
                "new.value: robot", "new.value: housing", "new.value: milk",
                "new.value: scc", "new.value: pellets",
                "Net Impact","diff: Net Impact", 
                "Milk minus Feed", "diff: Milk minus Feed",
                "Labor plus Repair", "diff: Labor plus Repair",
                "Capital Cost", "diff: Capital Cost",
                "Others", "diff: Others",
                "Inflation Adj.", "diff: Inflation Adj.",
                "IFOC.gain cow", "diff: IOFC.gain cow",
                "IFOC.gain cwt", "diff: IOFC.gain cwt",
                "Breakeven Wage","diff: Wage",
                "Break.Wage Inflation","diff: Break.Wage Inflation")

s_noncurrency <- c("Scenario", 
                   "%.change: robot", "%.change: housing", "%.change: milk",
                   "%.change: scc", "%.change: pellets",
                   "new.value: robot", "new.value: housing", "new.value: milk",
                   "new.value: scc", "new.value: pellets",
                   "Break.Wage Inflation","diff: Break.Wage Inflation")



p_colnames <- c("Barn Only","Retrofit Parlors","New Parlors","Robots")


vars_selected_profile <- c("robot_parlor","profile_choice","herd_size","herd_increase",
                           "additional_labor","additional_cost","n_robot","cost_robot",
                           "cost_parlors", "cost_housing_cow","repair","robot_years",
                           "n_robot_life","milking_years","salvage_milking1","insurance_rate",
                           "hours_milking","hr_sv_milking",
                           "hr_heat_detection", "anticipated_hours_heat", "labor_rate",
                           "increase_rc_mgt","decrease_lab_mgt","labor_rate_rc_mgt",
                           "price_milk","milk_cow_day","milk_change","scc_premium",
                           "scc_average", "scc_change", "software","cost_DM",
                           "pellets","cost_pellets","milk_cow_coeff","milk_fat",
                           "milk_fat_coeff","adj_milk_cow_coeff","body_weight",
                           "body_weight_coeff1", "body_weight_coeff2", "lcatation_week",
                           "lactation_coeff1","lactation_coeff2", "culling_rate",
                           "death_rate","cost_heifer","cull_price", "change_turnover",
                           "change_electricity", "change_water","change_chemical",
                           "inflation_robot","inflation_margin","inflation_labor",
                           "interest", "hurdle_rate", "tax_rate", "dep_method",
                           "down_housing","down_milking1","down_milking2",
                           "r_housing","r_milking1","r_milking2","n_yr_housing",
                           "n_yr_milking1","n_yr_milking2")


vars_all_profiles <- c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
                       "anticipated_hours_heat","increase_rc_mgt",
                       "decrease_lab_mgt", "milk_change","scc_change","software", 
                       "pellets","cost_pellets","change_turnover","change_electricity",
                       "change_water", "change_chemical",
                       "down_housing", "down_milking1", "down_milking2",
                       "n_yr_housing", "n_yr_milking1","n_yr_milking2",
                       "salvage_housing", "salvage_milking1", 
                       "milking_years",  "robot_years", "n_robot",
                       "cost_housing_cow",  "cost_parlors", "cost_robot")

mins_vars_all_profiles <- c(rep(0,12), -10, rep(0,11), rep(1, 2), rep(0,4))

steps_vars_all_profiles <- c(10, 500, 0.1, 0.2, 
                             0.05, 0.1,
                             0.1, 2, 0.25, 5, 
                             2, 2, 0.25, 0.25,
                             0.25, 0.25,
                             rep(20000,3), rep(1,3),rep(5000,2), rep(1,2), 
                             rep(10000,4)) 


