

# --- Dashboard features ---

## Render: Isolated from base input changes 

output$c_NAI <- renderUI({
    isolate(base_NAI <- NAI())
    dash_NAI(rb$NAI,cutoff=0, compare=base_NAI)
}) 

output$c_IOFC <- renderUI({
  isolate(IOFC_unit <- input$IOFC)
  if (IOFC_unit=="per cow") {
    isolate({
      base_IOFC <- IOFC()
      base_IOFC2 <- IOFC2()
    })
    dash_IOFC(rb$IOFC, rb$IOFC2, basis=IOFC_unit, 
              compare=base_IOFC, compare2=base_IOFC2)
  } else {
    isolate({
      base_IOFC_cwt <- IOFC_cwt()
      base_IOFC2_cwt <- IOFC2_cwt()
    })
    dash_IOFC(rb$IOFC_cwt, rb$IOFC2_cwt, basis=IOFC_unit, 
              compare=base_IOFC_cwt, compare2=base_IOFC2_cwt)
  }
})   


output$c_milk_feed <- renderUI({ 
  isolate(diff <- rb$milk_feed - milk_feed()) 
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #1569C7; color:white;", 
      rb$milk_feed %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})   

output$c_labor_repair <- renderUI({
  isolate(diff <- rb$labor_repair - labor_repair())
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #FF926F; color:white;", 
      rb$labor_repair %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_captial_cost <- renderUI({
  isolate( diff <- rb$capital_cost - capital_cost() ) 
  validate( 
    need(!is.na(diff), "NA")
  ) 
  div(class="well well-sm", style= "background-color: #64E986; color:white;", 
      rb$capital_cost %>% formatdollar2() %>% strong() %>% h4(),
      diff %>% formatdollar2b() %>% strong() %>% h4())
})  

output$c_misc <- renderUI({
  isolate( diff <- rb$misc - misc() ) 
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


