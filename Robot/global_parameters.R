
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


c_dummy <- matrix(rep(NA,length(c_colnames)),nrow=1)
colnames(c_dummy) <- c_colnames 
c_empty_table <- data.frame(Column1 = numeric(0)) 
c_empty_table <- rbind(c_empty_table, c_dummy)[NULL,] 

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


s_dummy <- matrix(rep(NA,length(s_colnames)),nrow=1)
colnames(s_dummy) <- s_colnames 
s_empty_table <- data.frame(Column1 = numeric(0)) 
s_empty_table <- rbind(s_empty_table, s_dummy)[NULL,] 


p_colnames <- c("Barn Only","Retrofit Parlors","New Parlors","Robots")
p_dummy <- matrix(rep(NA,length(p_colnames)),nrow=1)
colnames(p_dummy) <- p_colnames 
p_empty_table <- data.frame(Column1 = numeric(0)) 
p_empty_table <- rbind(p_empty_table, p_dummy)[NULL,] 

