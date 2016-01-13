# addPopover(session, id = "dataEntry", title = "Data Entry",
#            content = paste0("You will enter your data inputs in this page."), 
#            options = list(container = "body"))

# ----- Popovers ------ 

addPopover(session, id =  "calculation_switch", title = "ON for Analysis, OFF for Break",
           content ="Set it 'On' to view the results and 
           'OFF' to supress calculations while resting.") 

lapply(base_profiles, function(x) { 
  
  addPopover(session, id = paste0("IOFC",x), title="Income Over Feed Cost",
          content = "Improving the margin of milk revenue over feed cost
          is essential to financial sustainability.",
          options = list(container = "body")) 
  
  addPopover(session, id = paste0("NAI",x), title="Annualized Net Impact",
            content = "The sum of five components shown to the right is 
            the final result of the calculation.", 
            options = list(container = "body")) 
  
  addPopover(session, id = paste0("milk_feed",x), title="Milk Income minus Feed Cost",
            content = "It often helps to think these major components of revenues and expenses
            in terms of the margin.", 
            options = list(container = "body")) 
  
  addPopover(session, id = paste0( "labor_repair",x), title="Labor Cost plus Repair",
            content = "There is likely a trade-off between the labor cost saving
            and the additional repair cost.")
  
  addPopover(session, id = paste0("capital",x), title="Recovery Cost of Capital",
            content = "Knowing the cost of capital helps you assess how much investment 
            you can afford.")
  
  addPopover(session, id = paste0("misc",x), title="Other Expenses",
            content = "This is the sum of minor components other than 
            the three components above.")
  
  addPopover(session, id = paste0("inflation",x), title="Inflation Adjustments",
            content = "This adjustment bridges the gap between the first year impact 
            and the annual-equivalent impact over the planning horizon.")
  
  addPopover(session, id = paste0("plot1",x), title="Figure: Milk Income minus Feed Cost",
            content = "Milk Income and Feed Cost, compared to 
            the current operation.")
  
  addPopover(session, id = paste0("plot2",x), title="Figure: Labor Cost plus Repair",
            content = "Reduced Labor Cost and Increased Repair Cost, compared to
            the current operation.")
  
  addPopover(session, id = paste0("plot3",x), title="Figure: Recovery Cost of Capital",
            content = "Relative costs of financing a Milking System and Housing")
  
  addPopover(session, id = paste0("cashflow_small_chart",x), title="After-tax Cash Flow", 
            content = paste("The chart shows the cash flow for the year-by-year sequence of after-tax impacts.
                            See the", ems("Cash Flow"), "tab for details."))
  
 
  addPopover(session, id =  paste0("budget_year",x), title = " ",
             content ="txt") 
  
  addPopover(session, id = paste0("cashflow_chart",x), title = "Cash Flow Chart",
            content ="txt") 
  
  addPopover(session, id = paste0("PB_plot_pos_neg_impact",x), title="Positive minus Negative Impact",
            content = "txt")
  
  addPopover(session, id = paste0("PB_plot_before_tax_impact",x), title="Before-tax Impact",
            content = "txt")
  
  addPopover(session, id = paste0("PB_plot_after_tax_impact",x),  title="After-tax Impact",
            content = "txt")
  
  addPopover(session, id = paste0("PB_plot_after_tax_impact_const",x),  title="Annualization",
            content = "txt")
  
  addPopover(session, id = paste0("breakeven_chart",x),  title="Breakeven Wage",
            content = "txt")
  
})


