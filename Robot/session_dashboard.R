

# ------ Dashboard features ------

NAI <- reactive({  browser()
  if (input$NAI=="w/o housing") {
    NAI <- impact_without_housing()
  } 
  else if (input$NAI=="w/ housing") {
    NAI <- impact_with_housing()
  } else {
    NAI <- impact_with_robot_salvage()
  }
  rv$NAI <- NAI
  
  # This is used later for alerting base value change in robustness analysis  
  createAlert(session, "c_input_change", "ref_c_input_change", 
              content = "New base values. 
              Press [Calculate] to updated the results.",
              append = FALSE) 
  rv$NAI
})

capital_cost <- reactive({  browser()
  if(input$NAI=="w/o housing") {
    rv$capital_cost <- -inc_exp_capital_recovery()
  } else if (input$NAI=="w/ housing") {
    rv$capital_cost <- -(inc_exp_capital_recovery() + capital_recovery_housing())
  } else  {
    rv$capital_cost <- -(inc_exp_capital_recovery() + capital_recovery_housing()) +
      + robot_end_PV()
  } 
  rv$capital_cost
}) 

milk_current <- reactive({  browser()
  input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                  +  input$scc_premium/100 * input$scc_average/1000) 
})

milk_robot <- reactive({  browser()
  herd_size2() * 330 * (input$milk_cow_day + input$milk_change) *
    (input$price_milk/100 + input$scc_premium/100 * input$scc_average*(1-input$scc_change/100)/1000) 
})

labor_current <- reactive({  browser()
  (input$hr_heat_detection + input$hours_milking) * input$labor_rate*365
})

labor_robot <- reactive({  browser()
  (input$anticipated_hours_heat + anticipated_hours_milking()) * input$labor_rate *365 + 
    + (input$increase_rc_mgt - input$decrease_lab_mgt) * input$labor_rate_rc_mgt * 365 +
    + input$additional_labor * input$herd_increase 
}) 

feed_current <- reactive({  browser()
  DMI_day() * input$cost_DM * 330 * input$herd_size
})

feed_robot <- reactive({  browser()
  (DMI_projected() * input$cost_DM + input$pellets * input$cost_pellets/2000) * 330 * herd_size2()
})

milk_feed <- reactive({  browser()
  -(feed_robot() - feed_current()) + milk_robot() -  milk_current() 
})

labor_repair <- reactive({  browser()
  -(labor_robot() - labor_current() +inc_exp_repair())
})

misc <- reactive({  browser()
  NAI() - (milk_feed() + labor_repair() + capital_cost())
})

## rendering to UI 

output$IOFC <- renderUI({
  if (input$IOFC=="per cow") {
    dash_IOFC(IOFC(), IOFC2(), basis=input$IOFC)
  } else {
    dash_IOFC(IOFC_cwt(), IOFC2_cwt(), basis=input$IOFC)
  }
})  

output$NAI <- renderUI({
  dash_NAI(NAI(),cutoff=0)
}) 

output$milk_feed <- renderUI({
  validate(
    need(!is.na(milk_feed()),"NA")
  )
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      milk_feed() %>% formatdollar2() %>% strong() %>% h4(),
      h5("Milk Income - Feed Cost"), h5("under robot"))
})  

output$labor_repair <- renderUI({
  validate(
    need(!is.na(labor_repair()),"NA")
  )
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      labor_repair() %>% formatdollar2() %>% strong() %>% h4(),
      h5("Labor + Repair Cost"), h5("under robot"))
})  

output$captial_cost <- renderUI({
  validate(
    need(!is.na(capital_cost()),"NA")
  )
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      capital_cost() %>% formatdollar2() %>% strong() %>% h4(),
      h5("Cost of Capital"),  h5("under robot"))
})  

output$misc <- renderUI({
  validate(
    need(!is.na(misc()),"NA")
  )
  div(class="well well-sm", style= "background-color: #C2B280; color:white;", 
      misc() %>% formatdollar2() %>% strong %>% h4(), 
      h5("The Rest"), h5("under robot"))
})  

output$plot1 <- renderPlot({ 
  dash_plot1(feed_current(),feed_robot(),milk_current(),milk_robot())
})

output$plot2 <- renderPlot({
  dash_plot2(inc_exp_repair(),labor_current(),labor_robot()) 
})

output$plot3 <- renderPlot({ 
  dash_plot3(inc_exp_capital_recovery(),capital_recovery_housing(),robot_end_PV(),input$NAI)  
})

