


# This sets the default value for additional_labor and additional_cost when hidden from the user
updateNumericInput(session, "additional_labor",NULL,value=450,step=50,min=0)
updateNumericInput(session, "additional_cost",NULL,value=200,step=50,min=0)
# 
# # Cash flow related variables; initially hidden from the user 
# updateNumericInput(session, "inflation_robot",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "inflation_margin",NULL,value=0.2,step=.25,min=0)
# updateNumericInput(session, "inflation_labor",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "inflation_general",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "inflation_general",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "down_housing",NULL,value=100000, min=0,step=20000)
# updateNumericInput(session, "down_robot1",NULL,value=0, min=0,step=20000)
# updateNumericInput(session, "down_robot2",NULL,value=40000, min=0,step=20000)
# updateNumericInput(session, "r_housing",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "r_robot1",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "r_robot2",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "n_yr_housing",NULL,value=24, min=0, step=1)
# updateNumericInput(session, "n_yr_robot1",NULL,value=12, min=0, step=1)
# updateNumericInput(session, "n_yr_robot2",NULL,value=12, min=0, step=1)
# updateNumericInput(session, "salvage_housing",NULL,value=0, min=0, step=5000)
# updateNumericInput(session, "horizon",NULL,value=30, min=1, step=5)
# updateNumericInput(session, "hurdle_rate",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "tax_rate",NULL,value=40, min=0, step=2)


# Temporary fix between horizon year and housing year 
observeEvent(rv$housing_years,{
  updateNumericInput(session, "horizon",NULL,value=rv$housing_years, min=1, step=1)
  updateSliderInput(session, "budget_year", "Select budget year",value=1, min=1,max=rv$housing_years)
})

# Temporary fix for a single interest rate
observeEvent(input$interest,{
  rv$copy_r_housing <- input$interest
  rv$copy_r_milking1 <- input$interest
  rv$copy_r_milking2 <- input$interest
  updateNumericInput(session, "r_housing",NULL,value=input$interest, min=0, step=.25)
  updateNumericInput(session, "r_robot1",NULL,value=input$interest, min=0, step=.25)
  updateNumericInput(session, "r_robot2",NULL,value=input$interest, min=0, step=.25)
})

# Show/hide DMI calculations 
shinyjs::onclick("customDMI",
                 shinyjs::toggle(id="DMI_inputs", anim = TRUE)
)

# Show/hide Robots vs Parlors summary tables 
shinyjs::onclick("tableProfile",
                 shinyjs::toggle(id="tableProfileSummary", anim = TRUE)
)

# Show/hide Robots vs Parlors profile explanations
shinyjs::onclick("readProfile",
                 shinyjs::toggle(id="ref_readProfile", anim = TRUE)
)



# observe({
#   browser() 
#   if (input$cash_flow_on=="ON") {
#   shinyjs::show(id="cash_flow_details")
#   } else {
#     shinyjs::hide(id="cash_flow_details")
#   }
# })

#   observe({
#     toggle(id="DMI_inputs", condition = input$customDMI, anim = TRUE)
#   })

observeEvent(input$coeff_reset,{ 
  updateNumericInput(session, "milk_cow_coeff",NULL,value=0.4,min=0,step=0.1)
  updateNumericInput(session, "milk_fat",NULL,value=3.65,min=0,step=0.2)
  updateNumericInput(session, "milk_fat_coeff",NULL,value=15,min=0,step=0.5)
  updateNumericInput(session, "adj_milk_cow_coeff",NULL,value=0.372,min=0,step=0.1)
  updateNumericInput(session, "body_weight_coeff1",NULL,value=0.0968,min=0,step=0.005)
  updateNumericInput(session, "body_weight_coeff2",NULL,value=0.75,min=0,step=0.05)
  updateNumericInput(session, "lactation_coeff1",NULL,value=-0.192,step=0.01)
  updateNumericInput(session, "lactation_coeff2",NULL,value=3.67,min=0,step=0.05)
}) 


# Enable calculation button in Economic Analysis when Data Entry tabs are viewed by the user 
observe({
  if (input$budget==0) {
    if (!is.null(rv$DMI_change) & !is.null(rv$DMI_day) & 
        !is.null(rv$herd_size2) & !is.null(rv$increased_insurance) &
        !is.null(rv$anticipated_hours_milking) & !is.null(rv$milk_lb_alt_day)
    ) {
      updateButton(session, "budget", disabled = FALSE, style = "primary", icon = "")
    } 
    else {
      return()
    }
  } else {
    return()
  }
})


# Weighted Average Cost of Capital 
WACC <- reactive({  
  rv$WACC<- ((input$down_housing + input$down_milking1) * input$hurdle_rate +
               + (rv$loan_housing * input$r_housing + rv$loan_milking1 * input$r_milking1)*
               (1-input$tax_rate/100))/(rv$cost_housing + rv$cost_milking)
  rv$WACC
})


