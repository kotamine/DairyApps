

# --- Calculations of variables (stored as functions), followed by rendering to User Interface ---
# ----- calculations -----

# --- Farm Finaces ---

herd_size2 <- reactive({ 
  rv$herd_size2 <- input$herd_size + input$herd_increase
  rv$herd_size2
})

# This sets the default value for additional_labor and additional_cost when hidden from the user
updateNumericInput(session, "additional_labor",NULL,value=450,step=50,min=0)
updateNumericInput(session, "additional_cost",NULL,value=200,step=50,min=0)

robot_invest <- reactive({ 
  rv$robot_invest <- input$n_robot * input$cost_robot
  rv$robot_invest
}) 

cost_housing <- reactive({ 
  rv$cost_housing <- input$cost_housing_cow * herd_size2()
  rv$cost_housing 
})

total_investment <- reactive({ 
  rv$total_investment <- total_investment_cow()  * herd_size2()
  rv$total_investment
})

total_investment_cow <- reactive({ 
  rv$total_investment_cow <-  input$cost_housing_cow + robot_invest()/herd_size2()
  rv$total_investment_cow
})

# --- Maintenance ---
housing_years <- reactive({ 
  rv$housing_years <- input$n_robot_life * input$robot_years
  rv$housing_years 
})

increased_insurance <- reactive({ 
  rv$increased_insurance <- total_investment()
  rv$increased_insurance  
})

# --- Labor Savings ---
anticipated_hours_milking <- reactive({ 
  rv$anticipated_hours_milking <- input$hours_milking - input$hr_sv_milking
  rv$anticipated_hours_milking  
})

# --- Milk Outputs ---
milk_day_cow_robot <- reactive({
  rv$milk_day_cow_robot <- input$milk_cow_day + input$milk_change
  rv$milk_day_cow_robot
})

milk_lb_robot_day <- reactive({ 
  rv$milk_lb_robot_day <- milk_day_cow_robot() * herd_size2()/input$n_robot 
  rv$milk_lb_robot_day   
})

# --- Feed --- 
DMI_change <- reactive({ 
  rv$DMI_change <- DMI_projected() - DMI_day()
  rv$DMI_change  
})


# Show/hide DMI calculations 
shinyjs::onclick("customDMI",
                 shinyjs::toggle(id="DMI_inputs", anim = TRUE)
)

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

adj_milk_cow_day <- reactive({ 
  rv$adj_milk_cow_day <- input$milk_cow_day * input$milk_cow_coeff +  
    + input$milk_cow_day * input$milk_fat/100 * input$milk_fat_coeff
  rv$adj_milk_cow_day 
})

adj_milk_cow_day2 <- reactive({   # adj_milk_cow_day with the herd size under using robots
  rv$adj_milk_cow_day2 <- milk_day_cow_robot() * input$milk_cow_coeff + 
    + milk_day_cow_robot() * input$milk_fat/100 * input$milk_fat_coeff 
  rv$adj_milk_cow_day2
})

stage_lactation <- reactive({ 
  rv$stage_lactation <- 1 - exp( input$lactation_coeff1 * (input$lcatation_week + input$lactation_coeff2)) 
  rv$stage_lactation 
})

DMI_day <- reactive({ 
  rv$DMI_day <-  stage_lactation() * (adj_milk_cow_day()/conv_factor * input$adj_milk_cow_coeff + 
                                        +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  rv$DMI_day
})

DMI_projected <- reactive({ 
  rv$DMI_projected <-  stage_lactation() * (adj_milk_cow_day2()/conv_factor * input$adj_milk_cow_coeff + 
                                              +  input$body_weight_coeff1* (input$body_weight/conv_factor)^input$body_weight_coeff2) * conv_factor
  rv$DMI_projected 
})

# --- Replacement ---
#  nothing here

# --- Utilities --- 
#  nothing here

# --- Inflations ---
#  nothing here

# ------ rendering to UI ------

##  -- one could try to shorten the code below but it doesn't work with reactive values
##  -- eval(parse(text=XXX)) is an option, but it is said to create security problems. 
#
# variables_to_render <- c("herd_size2", "robot_invest", "cost_housing")
#  
# lapply(variables_to_render, 
#        function(x) { 
#          output[[paste0(x)]] <- renderUI({ paste0(x,"()") })
#        }
#        )

output$herd_size2 <- renderUI({
  herd_size2() %>% formatcomma() %>% helpText()
})

output$robot_invest <- renderUI({
  robot_invest() %>% formatcomma() %>% helpText() 
})

output$cost_housing <- renderUI({
  cost_housing() %>% formatcomma() %>% helpText() 
})

output$total_investment <- renderUI({
  total_investment()  %>% formatcomma()  %>% helpText() 
})

output$total_investment_cow <- renderUI({
  total_investment_cow() %>% formatcomma() %>% helpText() 
})

output$housing_years <- renderUI({
  housing_years()  %>% formatcomma() %>% helpText() 
})

output$increased_insurance <- renderUI({
  increased_insurance()  %>% formatcomma() %>% helpText() 
})

output$anticipated_hours_milking <- renderUI({
  anticipated_hours_milking()  %>% formatcomma()  %>% helpText() 
})

output$milk_lb_robot_day <- renderUI({
  milk_lb_robot_day()  %>% formatcomma() %>% helpText() 
})

output$DMI_change <- renderUI({
  DMI_change()  %>% round(3) %>% helpText()
})

output$rep_milk_cow_day <- renderUI({
  input$milk_cow_day %>% formatcomma() %>% helpText() 
})

output$rep_milk_change <- renderUI({
  input$milk_change  %>% formatcomma() %>% helpText() 
})

output$DMI_change_copy <- renderUI({
  DMI_change() %>% round(3) %>% helpText()
})

output$adj_milk_cow_day <- renderUI({
  adj_milk_cow_day()  %>% round(2) %>% helpText()
})

output$stage_lactation <- renderUI({
  stage_lactation()  %>% round(4) %>% helpText()
})

output$DMI_day <- renderUI({
  DMI_day()  %>% round(3) %>% helpText()
})

output$DMI_projected <- renderUI({
  DMI_projected()  %>% round(3) %>% helpText()
})



