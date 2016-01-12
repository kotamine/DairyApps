
# bsTooltip(id = "dataEntry", title = "Please enter your data inputs.",
#           options = list(container = "body"))
div(
  
  bsTooltip(id="default_data","Set user input data back to default setting"),
  bsTooltip(id="data_download","Download user input data"),
  bsTooltip(id="data_upload", "Upload previously downloaded user input data file"), 
  bsTooltip(id="remove", "Remove the uploaded user input data file"), 
  
  lapply(seq_along(common_variables_min_step$note), function(i) {
    if (!is.na(common_variables_min_step$note[i])) {
    bsTooltip(id=paste(common_variables_min_step$variable[i]),  
              common_variables_min_step$note[i])
    }
  }), 
  
  lapply(base_profiles, function(x) {
    lapply(seq_along(profile_specific_variables_min_step$note), function(i) {
      if (!is.na( profile_specific_variables_min_step$note[i])) {  
      bsTooltip(id=paste0(profile_specific_variables_min_step$variable[i],x),  
              profile_specific_variables_min_step$note[i])
      }
  })
  }),
  
  lapply(base_profiles, function(x) {
    for (var in c("milk_lb_alt_day","milk_day_cow_alt")) {
      bsTooltip(id=paste0(var,x), 
                title = "According to the DMI formula of National Research Council (NRC)")
    }
  }), 
  
  bsTooltip(id="herd_increaseRobots", "Typical herd size of 66-74 cows/robot"),  
  bsTooltip(id="n_robotRobots","Typical range of 55-65 milking cows/robot"),
  bsTooltip(id="cost_robotRobots", "Typical range of $185,000 - $230,000"),
  bsTooltip(id="useful_yearsRobots", "Typical range is 7 - 15 years"),
  bsTooltip(id="salvage_milking1Robots", "Typical range of 10-30% of purchase price"),
  bsTooltip(id="softwareRobots", "Estimated range of $20 - $60 per cow/yr"),
  bsTooltip(id="repairRobots", "Typical range from $5,000 - $9,000/robot")
  
)
