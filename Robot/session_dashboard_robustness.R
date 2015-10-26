

# --- Dashboard features ---

rb_NAI <- reactive({
  if (input$NAI=="w/o housing") {
    rb$NAI <- rb$impact_without_housing
  } 
  else if (input$NAI=="w/ housing") {
    rb$NAI <- rb$impact_with_housing
  } else {
    rb$NAI <- rb$impact_with_robot_salvage
  }
  rb$NAI
})

rb_capital_cost <- reactive({
  if(input$NAI=="w/o housing") {
    rb$capital_cost <- -rb$inc_exp_capital_recovery
  } else if (input$NAI=="w/ housing") {
    rb$capital_cost <- -(rb$inc_exp_capital_recovery + rb$capital_recovery_housing)
  } else  {
    rb$capital_cost <- -(rb$inc_exp_capital_recovery + rb$capital_recovery_housing) +
      + rb$robot_end_PV
  } 
  rb$capital_cost
}) 

output$c_IOFC <- renderUI({
  if (input$IOFC=="per cow") {
    dash_IOFC(rb$IOFC, rb$IOFC2, basis=input$IOFC, 
              compare=IOFC(), compare2=IOFC2())
  } else {
    dash_IOFC(rb$IOFC_cwt, rb$IOFC2_cwt, basis=input$IOFC, 
              compare=IOFC_cwt(), compare2=IOFC2_cwt())
  }
})  

output$c_NAI <- renderUI({
  dash_NAI(rb_NAI(),cutoff=0, compare=NAI())
}) 


output$c_milk_feed <- renderUI({
  rb$milk_feed <- -(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current   
  diff <- rb$milk_feed - rv$milk_feed
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      rb$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})   

output$c_labor_repair <- renderUI({
  rb$labor_repair <- -(rb$labor_robot - rb$labor_current + rb$inc_exp_repair)
  diff <- rb$labor_repair - rv$labor_repair
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rb$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_captial_cost <- renderUI({
  diff <- rb_capital_cost() - capital_cost()
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      rb_capital_cost() %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_misc <- renderUI({
  NAI <- rb_NAI()
  milk_feed <- -(rb$feed_robot - rb$feed_current) + rb$milk_robot -  rb$milk_current 
  labor_repair <- -(rb$labor_robot - rb$labor_current +rb$inc_exp_repair)
  capital_cost <- rb_capital_cost()
  
  rb$misc <- NAI - (milk_feed + labor_repair + capital_cost)
  diff <- rb$misc - rv$misc 
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
  dash_plot3(rb$inc_exp_capital_recovery,rb$capital_recovery_housing,rb$robot_end_PV, input$NAI)  
})

