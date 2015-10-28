
# Loaded once. Shared across all sessions.

conv_factor <- 2.2046  # conversion factor from kg to pound 

input_colnames <- c("input_id", "milk_cow_day","milk_change") # THIS IS INCOMPLETE! 

c_colnames <- c("variable", "% change","base value","new value",
                 "net impact w/o housing","change: impact w/o housing", 
                 "net impact w/ housing","change: impact w/ housing",
                 "net impact w/ salvage", "change: impact w/ salvage",
                 "IFOC gain cow/year", "change: IOFC gain cow",
                 "IFOC gain cwt", "change: IOFC gain cwt",
                 "milk - feed", "change: milk - feed",
                 "labor + repair", "change: labor + repair",
                 "capital cost", "change: capital cost",
                 "the rest", "change: the rest")

c_dummy <- matrix(rep(NA,length(c_colnames)),nrow=1)
colnames(c_dummy) <- c_colnames 
c_empty_table <- data.frame(Column1 = numeric(0)) 
c_empty_table <- rbind(c_empty_table, c_dummy)[NULL,] 

c_order1 <- c("variable", "% change","base value","new value",
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


s_varnames <- c("cost_robot","cost_housing_cow", "milk_change",
                "scc_change","pellets")

# s_labels <- c("Estimated cost per robot", "Related housing changes needed per cow",
#               "Projected change in milk production",
#               "Estimated percent change in SCC (%)", "Pellets fed in robot booth") 

s_labels <- c("Increased investment",
              "Use less pellets",
              "New barn ($120k/stall)")

s_colnames <- c("scenario", 
                "% change: robot", "% change: housing", "% change: milk",
                "% change: scc", "% change: pellets",
                "new val: robot", "new val: housing", "new val: milk",
                "new val: scc", "new val: pellets",
                "net impact w/o housing","change: impact w/o housing", 
                "net impact w/ housing","change: impact w/ housing",
                "net impact w/ salvage", "change: impact w/ salvage",
                "IFOC gain cow/year", "change: IOFC gain cow",
                "IFOC gain cwt", "change: IOFC gain cwt",
                "milk - feed", "change: milk - feed",
                "labor + repair", "change: labor + repair",
                "capital cost", "change: capital cost",
                "the rest", "change: the rest")

s_dummy <- matrix(rep(NA,length(s_colnames)),nrow=1)
colnames(s_dummy) <- s_colnames 
s_empty_table <- data.frame(Column1 = numeric(0)) 
s_empty_table <- rbind(s_empty_table, s_dummy)[NULL,] 

s_order1 <- c("scenario", "% change: robot", "% change: housing", "% change: milk",
              "% change: scc", "% change: pellets", "change: impact w/ salvage")



