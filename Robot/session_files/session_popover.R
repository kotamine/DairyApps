# addPopover(session, id = "dataEntry", title = "Data Entry",
#            content = paste0("You will enter your data inputs in this page."), 
#            options = list(container = "body"))

# ----- Popovers ------ 

addPopover(session, id =  "calculation_switch", title = paste(emquo("ON"), "for Analysis,", 
            emquo("OFF"), "for Break"),
           content =paste("Set this", ems("ON"), "to view the results and", 
                          ems("OFF"), "to supress while resting."))   

lapply(base_profiles, function(x) { 
  
  addPopover(session, id = paste0("IOFC",x), title="Income Over Feed Cost",
          content = "A benchmark measure for improving financial sustainability.",
          options = list(container = "body")) 
  
  addPopover(session, id = paste0("NAI",x), title="Annualized Net Impact",
            content = "The sum of five components shown to the right.", 
            options = list(container = "body")) 
  
  addPopover(session, id = paste0("milk_feed",x), title="Milk Income minus Feed Cost",
            content = "The margin where the two big components count.", 
            options = list(container = "body")) 
  
  addPopover(session, id = paste0( "labor_repair",x), title="Labor Cost plus Repair",
            content = "A likely trade-off between the labor cost saving
            and the additional repair cost.")
  
  addPopover(session, id = paste0("capital",x), title="Recovery Cost of Capital",
            content = "The total investment seen as a piece-by-piece annual expense.")
  
  addPopover(session, id = paste0("misc",x), title="Other Expenses",
            content = "The sum of minor components other than 
            the three components above.")
  
  addPopover(session, id = paste0("inflation",x), title="Inflation Adjustments",
            content = "The bridge between the first year impact 
            and the annualized impact over the planning horizon.")
  
  addPopover(session, id = paste0("plot1",x), title="Figure: Milk Income minus Feed Cost",
            content = paste(ems("Milk Income"), "and", ems("Feed Cost,"), "compared to 
            the current operation."))
  
  addPopover(session, id = paste0("plot2",x), title="Figure: Labor Cost plus Repair",
            content = paste(ems("Reduced Labor Cost"), "and", ems("Increased Repair Cost,"), 
            "compared to the current operation." ))
  
  addPopover(session, id = paste0("plot3",x), title="Figure: Recovery Cost of Capital",
            content = paste("Relative costs of financing a", ems("Milking System"), "and", ems("Housing.")))
  
  addPopover(session, id = paste0("cashflow_small_chart",x), title="After-tax Cash Flow", 
            content = paste("After-tax cash flow shown year-by-year. 
                            See the", ems("Cash Flow"), "tab for details."))
  
 
  addPopover(session, id =  paste0("budget_year",x), title = "Nail the Moving Target", 
             content =paste("The items under", ems("Positive Impacts"), "and", ems("Negative Impacts"), 
            "will update in response.  Note that annualized values stay constant."))     

  addPopover(session, id = paste0("PB_plot_pos_neg_impact",x),   
             title=paste("Read the Movement"), 
            content = paste(ems("Increased Revenue Total"),"and", ems("Increased Expense Total"), 
                            "are projected to grow at the inflation rate for IOFC margin, while", 
                             ems("Decreased Expense Total"), "is assumed to grow at
                          the inflation rate for labor.")
            )
  
  addPopover(session, id = paste0("PB_plot_before_tax_impact",x), 
             title=paste("What the hey is",  strong(emquo("Inflation Adjustments")),"?"),
            content = paste("Suppose that we have a (constant) number for", ems("Annualized before-tax impact,"), 
                            "calculated elsewhere. Then we can take the difference between",  
                            ems("Positive-minus-Negative Impact"), "and that constant. 
                            Such is the bridge we refer to as", strong(emquo("Inflation Adjustments.")) 
                            )) 
  
  addPopover(session, id = paste0("PB_plot_after_tax_impact",x),
             title="Before Annualization",
             content="When wiggles get wigglier and entangled...")
  
  addPopover(session, id = paste0("PB_plot_after_tax_impact_const",x),  
             title="After Annualization",
             content="Straighten up the wiggles (properly)!")

  addPopover(session, id=paste0("bw_wage_after_tax",x), title="Breakeven Wage Rate",
             content=paste("Any starting wage rage higher than the calculated value
              implies profits"))

  addPopover(session, id=paste0("bw_wage_inflation_after_tax",x), title="Breakeven Wage Inflation Rate",
             content=paste("Any wage inflation rate higher than the calculated value 
                           implies profits"))

  addPopover(session, id = paste0("breakeven_chart",x),  
             title="What wage would justify your investment?", placement = "top", options = list(container = "body"),
            content = "If the baseline wage trajectory stays above the two breakeven trajectories,
                       the investment is predicted to be profitable, holding everything else constant.")
  
  addPopover(session, id = paste0("cashflow_chart",x), title = "The Tale of Cash Flow",
             content =paste("In early years of investment, typically depreciation pushes down the",
                            ems("operating income"), "and pushes up", ems("after-tax cash flow"),
                            "(which is through its tax deduction). In later years when the", 
                            ems("operating income"), "turns positive, tax obligation dampens the",
                            ems("after-tax cash flow."))) 
  
})


