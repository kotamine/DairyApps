
# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increase replaced by  rb$herd_increase
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "down_housing", "down_milking1", "down_milking2",
#     "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
#     "salvage_housing", "salvage_milking1", 
#     "milking_years") and input$NAI

#  ------ Dashboard portion of  Robot vs Parlor analysis -----------
observe({
  if (is.na(rb$inc_rev_total)) { return() }
  
  input$NAI # respond to input$NAI
  
isolate({
  
  rb$tax_factor <- (1-(rb$NAI_spec=="after tax")*input$tax_rate/100)
  
  if (rb$NAI_spec=="before tax") {
    rb$NAI <- rb$net_annual_impact_before_tax
  } else {
    rb$NAI <- rb$net_annual_impact_after_tax
  }
  
  rb$IOFC <- (input$milk_cow_day * input$price_milk/100 - rb$DMI_day * input$cost_DM )*330 * rb$tax_factor
  
  rb$IOFC2 <- (rb$milk_day_cow_alt * input$price_milk/100 + 
                 - rb$DMI_projected * input$cost_DM - rb$pellets * rb$cost_pellets/2000)*330 *
    rb$tax_factor  
  
  rb$IOFC_cwt <- rb$IOFC /365 /input$milk_cow_day * 330 * rb$tax_factor
  
  rb$IOFC2_cwt <- rb$IOFC2 /365 /rb$milk_day_cow_alt * 330 * rb$tax_factor
  
  rb$milk_current <- 
    input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                    +  input$scc_premium/100 * input$scc_average/1000) *
    rb$tax_factor
  
  rb$milk_robot <- (rb$herd_size2 * 330 * rb$milk_day_cow_alt *
                      (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*(1-rb$scc_change/100)/1000)) *
    rb$tax_factor 
  
  rb$labor_current <-  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365 *
    rb$tax_factor 
  
  rb$labor_robot <- ((rb$anticipated_hours_heat + rb$anticipated_hours_milking) * input$labor_rate *365 + 
                       + (rb$increase_rc_mgt - rb$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
                       + input$additional_labor * rb$herd_increase) * rb$tax_factor 
  
  rb$feed_current <-  rb$DMI_day * input$cost_DM * 330 * input$herd_size * rb$tax_factor
  
  rb$feed_robot <- (rb$DMI_projected * input$cost_DM + rb$pellets *
                      rb$cost_pellets/2000) * 330 * rb$herd_size2 * rb$tax_factor 
  
  rb$milk_feed <-  (-(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current )
  
  rb$labor_repair <- -(rb$labor_robot - rb$labor_current + rb$inc_exp_repair) 
  
  rb$inflation <- - pmt(input$interest/100, rb$housing_years, 
                        npv(input$interest/100, rb$table_cash_flow$revenue_minus_expense[-1])) +
    - (rb$inc_rev_total + rb$dec_exp_total - rb$inc_exp_total) 
  
  rb$capital <- -rb$capital_cost_total + 
    +(rb$NAI_spec=="after tax")*(rb$tax_interest + rb$tax_depreciation)
  
  rb$misc <- rb$NAI - (rb$milk_feed + rb$labor_repair + rb$capital + rb$inflation) 
  
  rb$capital_recovery_robot2 <- rb$capital_recovery_robot +
    - (rb$NAI_spec=="after tax")*rb$tax_deduction_robot
  rb$capital_recovery_housing2 <- rb$capital_recovery_housing +
    - (rb$NAI_spec=="after tax")*rb$tax_deduction_housing
  
})

})


# --- Dashboard features ---

## Render: Isolated from base input changes 

output$c_NAI <- renderUI({
    isolate(base_NAI <- rv$NAI )
    dash_NAI(rb$NAI,cutoff=0, compare=base_NAI)
}) 

output$c_IOFC <- renderUI({
  isolate(IOFC_unit <- input$IOFC)
  if (IOFC_unit=="per cow") {
    isolate({
      base_IOFC <- rv$IOFC
      base_IOFC2 <- rv$IOFC2
    })
    dash_IOFC(rb$IOFC, rb$IOFC2, basis=IOFC_unit, 
              compare=base_IOFC, compare2=base_IOFC2)
  } else {
    isolate({
      base_IOFC_cwt <- rv$IOFC_cwt
      base_IOFC2_cwt <- rv$IOFC2_cwt
    })
    dash_IOFC(rb$IOFC_cwt, rb$IOFC2_cwt, basis=IOFC_unit, 
              compare=base_IOFC_cwt, compare2=base_IOFC2_cwt)
  }
})   


output$c_milk_feed <- renderUI({ 
  isolate(diff <- rb$milk_feed - rv$milk_feed ) 
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      rb$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})   

output$c_labor_repair <- renderUI({
  isolate(diff <- rb$labor_repair - rv$labor_repair)
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rb$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_captial_cost <- renderUI({
  isolate( diff <- rb$capital_cost - rv$capital_cost ) 
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      rb$capital_cost %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_misc <- renderUI({
  isolate( diff <- rb$misc - rv$misc ) 
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      rb$misc %>% formatdollar2() %>% strong %>% h4(), 
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_plot1 <- renderPlot({ 
  dash_plot1(rb$feed_current,rb$feed_robot,rb$milk_current,rb$milk_robot)
})

output$c_plot2 <- renderPlot({
  dash_plot2(rb$inc_exp_repair,rb$labor_current,rb$labor_robot) 
})

output$c_plot3 <- renderPlot({ 
  isolate( NAI_type <- input$NAI )
  dash_plot3(rb$inc_exp_capital_recovery,rb$capital_recovery_housing,
             rb$robot_end_PV, NAI_type)  
})






