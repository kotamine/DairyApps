
# bsTooltip(id = "dataEntry", title = "Please enter your data inputs.",
#           options = list(container = "body"))
div(
  
  bsTooltip(id="default_data","Set user input data back to default setting"),
  bsTooltip(id="data_download","Download user input data"),
  bsTooltip(id="resettableInput", "Upload previously downloaded user input data file"), 
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
  bsTooltip(id="repairRobots", "Typical range from $5,000 - $9,000/robot"),
  
  lapply(base_profiles, function(x) {
    lapply(seq_along(partial_budget_notes$note), function(i) {
      if (!is.na(partial_budget_notes$note[i])) {  
        bsTooltip(id=paste0(partial_budget_notes$variable[i],x),  
                  partial_budget_notes$note[i])
      }
    })
  }),
  
  # ----- Popovers ------ 
  lapply(base_profiles, function(x) {
    bsPopover(id = paste0("IOFC",x), title="Income Over Feed Cost",
              content = paste("As one of the most important indicators, IOFC shows 
              the margin of milk revenue after subtracting feed cost.",br(), 
              "Note: all values in the dashboard are the difference 
              from the current operation.") )
    
    bsPopover(id = paste0("NAI",x), title="Annualized Net Impact",
              content = paste("This is the final result of the calculation and the sum of
              five components shown to the right.", br(), 
              "Note: all values in the dashboard are shown in the form of annual-equivalent 
              income or payment over the planning horizon."))
    
    bsPopover(id = paste0("milk_feed",x), title="Milk Income minus Feed Cost",
              content = "Milk revenues and feed costs are respectively major parts of the
              revenue and expenses. It often helps to think in terms of the margin.")

        bsPopover(id = paste0( "labor_repair",x), title="Labor Cost plus Repair",
              content = "There is likely a trade-off between the cost of milking and chores
                and the cost of repair under the intensive machinery.")
    
    bsPopover(id = paste0("capital",x), title="Recovery Cost of Capital",
              content = "Knowing the cost of capital helps knowing how much investment 
              you can afford. Without, you could miss the right moment to invest in your facility.")
    
    bsPopover(id = paste0("misc",x), title="Other Expenses",
              content = "The three components above are major components. 
              This is the sum of minor components.")
    
    bsPopover(id = paste0("inflation",x), title="Inflation Adjustments",
              content = "Other components are listed for the first budget year. 
              This adjustment represents the annual-equivalent benefit/loss when those numbers are 
              expressed for the entire planning horizon.")
    
    bsPopover(id = paste0("plot1",x), title="Figure: Milk Income minus Feed Cost",
              content = "The figure shows Milk Income minus Feed Cost under  
              the current and alternative milking systems", trigger = "click")
    
    bsPopover(id = paste0("plot2",x), title="Figure: Labor Cost plus Repair",
              content = "The figure shows Labor Cost plus Repair under
              the current and alternative milking systems", trigger = "click")
    
    bsPopover(id = paste0("plot3",x), title="Figure: Recovery Cost of Capital",
              content = "The figure shows Recovery Cost of Capital under
              the current and alternative milking systems", trigger = "click")
    
    bsPopover(id = paste0("cashflow_small_chart",x), title="After-tax Cash Flow", 
              content = paste("This chart shows the cash flow for the year-by-year sequence of after-tax impacts.
              Go to the", ems("Cash Flow"), "tab for more details."), trigger = "click")  
     
    
    bsPopover(id = paste0("cashflow_chart",x), title = "Cash Flow Chart",
              content ="",  trigger = "click") 
    
    bsPopover(id = paste0("PB_plot_pos_neg_impact",x), title="",
              content = "")
    
    bsPopover(id = paste0("PB_plot_before_tax_impact",x), title="",
              content = "")
    
    bsPopover(id = paste0("PB_plot_after_tax_impact",x),  title="",
              content = "")
    
    bsPopover(id = paste0("PB_plot_after_tax_impact_const",x),  title="",
              content = "")
    
    bsPopover(id = paste0("breakeven_chart",x),  title="",
              content = "")
    
  })
)
