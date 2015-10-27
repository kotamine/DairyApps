
# Loaded once. Shared across all sessions.

conv_factor <- 2.2046  # conversion factor from kg to pound 

input_colnames <- c("input_id", "milk_cow_day","milk_change")

robustness_colnames <- c("variable", "% change","base value","new value",
                 "net impact w/o housing","change: impact w/o housing", 
                 "net impact w/ housing","change: impact w/ housing",
                 "net impact w/ salvage", "change: impact w/ salvage",
                 "IFOC gain cow/year", "change: IOFC gain cow",
                 "IFOC gain cwt", "change: IOFC gain cwt",
                 "milk - feed", "change: milk - feed",
                 "labor + repair", "change: labor + repair",
                 "capital cost", "change: capital cost",
                 "the rest", "change: the rest")

c_dummy <- matrix(rep(NA,length(robustness_colnames)),nrow=1)
colnames(c_dummy) <- robustness_colnames 
c_empty_table <- data.frame(Column1 = numeric(0)) 
c_empty_table <- rbind(c_empty_table, c_dummy)[NULL,] 

robustness_order1 <- c("variable", "% change","base value","new value",
               "net impact w/ salvage", "change: impact w/ salvage")


c_varnames <- c("cost_robot","cost_housing_cow", "repair",
                   "robot_years","salvage_robot", "hr_sv_milking",
                   "milk_change")

c_labels <- c("Estimated cost per robot", "Related housing changes needed per cow",
                 "Estimated annual change in milking system repair",
                 "Robots: years of useful life",
                 "Value of the robots after useful life",
                 "Anticipated savings in milking & chore labor",
                 "Projected change in milk production") 


