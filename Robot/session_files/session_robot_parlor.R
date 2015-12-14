

## --- This was an alternative version but it is slow ---
# current_profile <-  reactive({
#   switch(input$profile_choice, 
#          "Barn Only"="_pr1",
#          "Retrofit Parlors"="_pr2",
#          "New Parlors"="_pr3",
#          "Robots"="_pr4")
# })
# 
# 
# update_profile_vars  <- function(varnames, mins, steps, varname_ref=NULL) {
#   for (i in 1:length(varnames)) 
#   { x <- varnames[i]
#   isolate({ 
#     if (!is.null(input[[paste0(x)]])) {  
#       updateNumericInput(session, paste0(x),NULL, 
#                          value=input[[paste0(x,varname_ref)]], 
#                          min=mins[i],
#                          step=steps[i])
#     }
#   }) 
#   }
# }
# 
# 
# # update all variables in Data Entry when input$profile_choice changes 
# observe({
#   if (input$robot_parlor=="OFF") { return() }
#   input$profile_choice
#   isolate({
#   if (input$profile_choice=="Barn Only") {
#     update_profile_vars(vars_all_profiles,
#                         mins_vars_all_profiles, 
#                         steps_vars_all_profiles, 
#                         varname_ref="_pr1")
#   }
#   else if (input$profile_choice=="Retrofit Parlors") {
#     update_profile_vars(vars_all_profiles,
#                         mins_vars_all_profiles, 
#                         steps_vars_all_profiles, 
#                         varname_ref="_pr2")
#     
#   } else if (input$profile_choice=="New Parlors") {
#     update_profile_vars(vars_all_profiles,
#                         mins_vars_all_profiles, 
#                         steps_vars_all_profiles, 
#                         varname_ref="_pr3")
#     
#   } else {
#     update_profile_vars(vars_all_profiles,
#                         mins_vars_all_profiles, 
#                         steps_vars_all_profiles, 
#                         varname_ref="_pr4")
#     
#   }
#   })
# })
# 
# # update variables in Data Entry when their counterparts change in Robot vs Parlors
#   for (varname_ref in c("_pr1","_pr2","_pr3","_pr4")) { 
#     lapply(vars_all_profiles, function(x) { 
#       observeEvent(input[[paste0(x,varname_ref)]],{  
#       shinyjs:: disable(paste0(x,varname_ref))
#         on.exit(shinyjs:: enable(paste0(x,varname_ref)))
#       if (current_profile() !=varname_ref) return()
#       if (is.null(input[[paste0(x)]])) return() 
#       if (input[[paste0(x,varname_ref)]]==input[[paste0(x)]]) return()
# #         browser()
#       i <- which(vars_all_profiles==x)   
#       updateNumericInput(session, paste0(x),NULL, 
#                          value=input[[paste0(x,varname_ref)]], 
#                          min=mins_vars_all_profiles[i],
#                          step=steps_vars_all_profiles[i])
#     })   
#   })
# } 
#  
# 
# for (varname_ref in c("_pr1","_pr2","_pr3","_pr4")) { 
#   lapply(vars_all_profiles, function(x) { 
#     observeEvent(input[[paste0(x)]],{  
#       shinyjs:: disable(paste0(x))
#       on.exit(shinyjs:: enable(paste0(x)))
#       if (input$robot_parlor=="OFF") return() 
#       if (current_profile() !=varname_ref) return()
#       if (is.null(input[[paste0(x,varname_ref)]])) return() 
#         # browser()
#         i <- which(vars_all_profiles==x)   
#         updateNumericInput(session, paste0(x,varname_ref),NULL, 
#                            value=input[[paste0(x)]],
#                            min=mins_vars_all_profiles[i],
#                            step=steps_vars_all_profiles[i])
#       
#     })    
#   }) 
# } 



##  ---- Initial version ----
update_profile_vars  <- function(varnames, mins, steps, varname_ref=NULL) { 
  for (i in 1:length(varnames)) 
  { x <- varnames[i]
  isolate( x_pr <- input[[paste0(x,varname_ref)]])
  if (!is.null(x_pr)) {  
    
    updateNumericInput(session, paste0(x),NULL, 
                       value=input[[paste0(x,varname_ref)]], 
                       min=mins[i],
                       step=steps[i])
  }
  }
}

update_profile_vars2  <- function(varnames, mins, steps, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  { x <- varnames[i]
  isolate( x_pr <- input[[paste0(x,varname_ref)]])
  if (!is.null(x_pr)) {  
    
    updateNumericInput(session, paste0(x,varname_ref),NULL, 
                       value=input[[paste0(x)]], 
                       min=mins[i],
                       step=steps[i])
  }
  }
}


# update variables in Data Entry when their counterparts change in Robot vs Parlors
observe({
  
  if (input$robot_parlor=="OFF") { return() }
  
  if (input$profile_choice=="Barn Only") {
    update_profile_vars(vars_all_profiles,
                        mins_vars_all_profiles, 
                        steps_vars_all_profiles, 
                        varname_ref="_pr1")
  }
  else if (input$profile_choice=="Retrofit Parlors") {
    update_profile_vars(vars_all_profiles,
                        mins_vars_all_profiles, 
                        steps_vars_all_profiles, 
                        varname_ref="_pr2")
    
  } else if (input$profile_choice=="New Parlors") {
    update_profile_vars(vars_all_profiles,
                        mins_vars_all_profiles, 
                        steps_vars_all_profiles, 
                        varname_ref="_pr3")
    
  } else {
    update_profile_vars(vars_all_profiles,
                        mins_vars_all_profiles, 
                        steps_vars_all_profiles, 
                        varname_ref="_pr4")
    
  }
})



# update variables in Robot vs Parlors when their counterparts change in Data Entry 
observe({
  
  isolate(profile <- input$profile_choice)
  if (profile=="Barn Only") {
    update_profile_vars2(vars_all_profiles,
                         mins_vars_all_profiles, 
                         steps_vars_all_profiles, 
                         varname_ref="_pr1")
    
  }
  else if (profile=="Retrofit Parlors") {
    update_profile_vars2(vars_all_profiles,
                         mins_vars_all_profiles, 
                         steps_vars_all_profiles, 
                         varname_ref="_pr2")
    
  } else if (profile=="New Parlors") {
    update_profile_vars2(vars_all_profiles,
                         mins_vars_all_profiles, 
                         steps_vars_all_profiles, 
                         varname_ref="_pr3")
    
  } else {
    
    update_profile_vars2(vars_all_profiles,
                         mins_vars_all_profiles, 
                         steps_vars_all_profiles, 
                         varname_ref="_pr4")
    
  }
})



# Create an alert in any change in data input
observe({
  
  lapply(vars_all_profiles, function(x) {
    input[[paste0(x,"_pr1")]]
  }) 
  
  lapply(vars_all_profiles, function(x) {
    input[[paste0(x,"_pr2")]]
  })   
  
  lapply(vars_all_profiles, function(x) {
    input[[paste0(x,"_pr3")]]
  })   
  
  lapply(vars_all_profiles, function(x) {
    input[[paste0(x,"_pr4")]]
  })   
  
  createAlert(session, "p_input_change", "ref_p_input_change", 
              content = "New data inputs. 
            Press ``Calculate'' to updated the results.",
              append = FALSE) 
})


# Update section B: Financing Schedule by Profile
observe({
  if (input$robot_parlor=="OFF") { return() }
  
  rv$copy_cost_housing_pr1 <-  input$cost_housing_cow_pr1 *(input$herd_size + input$herd_increase_pr1)
  rv$loan_housing_pr1 <- rv$copy_cost_housing_pr1 - input$down_housing_pr1
  rv$copy_r_housing_pr1 <- input$interest
  
  rv$housing_years_pr2 <- input$milking_years_pr2
  rv$copy_cost_housing_pr2 <-  input$cost_housing_cow_pr2 *(input$herd_size + input$herd_increase_pr2)
  rv$copy_cost_milking1_pr2 <- input$cost_parlors_pr2
  rv$loan_housing_pr2 <- rv$copy_cost_housing_pr2 - input$down_housing_pr2
  rv$loan_milking1_pr2 <-rv$copy_cost_milking1_pr2 - input$down_milking1_pr2
  rv$copy_r_housing_pr2 <- input$interest
  rv$copy_r_milking1_pr2 <- input$interest
  
  rv$housing_years_pr3 <- input$milking_years_pr3
  rv$copy_cost_housing_pr3 <-  input$cost_housing_cow_pr3 *(input$herd_size + input$herd_increase_pr3)
  rv$copy_cost_milking1_pr3 <- input$cost_parlors_pr3
  rv$loan_housing_pr3 <-  rv$copy_cost_housing_pr3 - input$down_housing_pr3
  rv$loan_milking1_pr3 <- rv$copy_cost_milking1_pr3 - input$down_milking1_pr3
  rv$copy_r_housing_pr3 <- input$interest
  rv$copy_r_milking1_pr3 <- input$interest
  
  rv$housing_years_pr4 <- input$n_robot_life * input$robot_years
  rv$copy_cost_housing_pr4 <-  input$cost_housing_cow_pr4 *(input$herd_size + input$herd_increase_pr4)
  rv$copy_cost_milking1_pr4 <- input$n_robot * input$cost_robot
  rv$copy_cost_milking2_pr4 <-  rv$copy_cost_milking1_pr4*(1+input$inflation_robot/100)^input$robot_years
  rv$loan_housing_pr4 <- rv$copy_cost_housing_pr4 - input$down_housing_pr4
  rv$loan_milking1_pr4 <- rv$copy_cost_milking1_pr4 - input$down_milking1_pr4
  rv$loan_milking2_pr4 <- rv$copy_cost_milking2_pr4 - input$down_milking2_pr4
  rv$copy_r_housing_pr4 <- input$interest
  rv$copy_r_milking1_pr4 <- input$interest
  rv$copy_r_milking2_pr4 <- input$interest
  rv$copy_robot_years_pr4 <- input$robot_years
  rv$copy_n_robot_pr4 <- input$n_robot 
  rv$copy_cost_robot_pr4 <- input$cost_robot*(1+input$inflation_robot/100)^input$robot_years
  rv$salvage_milking2_pr4 <-  input$salvage_milking1_pr4*(1+input$inflation_robot/100)^input$robot_years
  rv$yr_invest_milking2_pr4 <- input$robot_years
  
})



# creating an emptry table that reactively renders
rp$table_before_tax <- matrix(NA) %>% data.frame()
rp$table_after_tax  <- matrix(NA) %>% data.frame()
rp$table_operating_income  <-  matrix(NA) %>% data.frame()
rp$table_after_tax_cash_flow  <-  matrix(NA) %>% data.frame()

use_profile_vars  <- function(varnames, varname_ref=NULL) {
  for (i in 1:length(varnames)) 
  {   x <- varnames[i]
  rp[[paste0(x)]] <- input[[paste0(x,varname_ref)]]
  }
}

# Calculate rp$table_profiles
observeEvent(input$robot_parlor_calculate, {
  n_years_max <- max(input$milking_years_pr1,
                     input$milking_years_pr2,
                     input$milking_years_pr3,
                     input$robot_years_pr4*input$n_robot_life) + 1
  
  varnames <- c("Net Annual Impact", "Milk Income minus Feed Cost","Labor and Repair Cost", 
                "Cost of Capital", "Others","Inflation Adjustments", 
                "IOFC per cow","IOFC per cwt",  "Breakeven Wage", "Breakeeven Wage Inflation")
  
  rp$table_before_tax <- cbind(rep(NA,length(varnames)))
  rp$table_after_tax  <-  cbind(rep(NA,length(varnames)))
  rp$table_operating_income <- cbind(Year=c(1:n_years_max)-1) 
  rp$table_after_tax_cash_flow  <- cbind(Year=c(1:n_years_max)-1)
  
  closeAlert(session, "ref_p_input_change")
  choices <- c("Barn Only","Retrofit Parlors","New Parlors","Robots")
  profile_ref <- c("_pr1", "_pr2", "_pr3", "_pr4")
  for (p in 1:4) {
    
    profile <- choices[p]
    
    use_profile_vars(vars_all_profiles, profile_ref[p])
    
    source(file.path("session_files", "session_calculations_robot_parlor.R"), local=TRUE)
    
    if (n_years_max > length(rp$table_cash_flow$operating_income)) {
      tmp_zero <- rep(0, n_years_max - length(rp$table_cash_flow$operating_income))
      rp$table_operating_income <- cbind(rp$table_operating_income, 
                                         profile = c(rp$table_cash_flow$operating_income,tmp_zero))
      rp$table_after_tax_cash_flow <- cbind(rp$table_after_tax_cash_flow,
                                            profile = c(rp$table_cash_flow$after_tax_cash_flow,tmp_zero))
    } else {
      rp$table_operating_income <- cbind(rp$table_operating_income, 
                                         profile  = rp$table_cash_flow$operating_income)
      rp$table_after_tax_cash_flow <- cbind(rp$table_after_tax_cash_flow,
                                            profile  = rp$table_cash_flow$after_tax_cash_flow)      
    } 
    
    rp$NAI_spec <- "before tax" 
    source(file.path("session_files", "session_dashboard_robot_parlor.R"), local=TRUE)
    
    rp$table_before_tax <-  cbind(rp$table_before_tax, 
                                  profile =c(rp$NAI, rp$milk_feed,  rp$labor_repair, rp$capital, 
                                             rp$misc,  rp$inflation, 
                                             rp$IOFC2 - rp$IOFC, 
                                             rp$IOFC2_cwt - rp$IOFC_cwt,
                                             rp$bw_wage_before_tax,
                                             rp$bw_wage_inflation_before_tax)) 
    
    rp$NAI_spec <- "after tax"
    source(file.path("session_files", "session_dashboard_robot_parlor.R"), local=TRUE)
    
    rp$table_after_tax <-   cbind(rp$table_after_tax, 
                                  profile =c(rp$NAI, rp$milk_feed,  rp$labor_repair, rp$capital, 
                                             rp$misc,  rp$inflation, 
                                             rp$IOFC2 - rp$IOFC, 
                                             rp$IOFC2_cwt - rp$IOFC_cwt,
                                             rp$bw_wage_after_tax,
                                             rp$bw_wage_inflation_after_tax)) 
  }
  
  rownames(rp$table_before_tax) <- varnames
  rownames(rp$table_after_tax)  <- varnames
  colnames(rp$table_before_tax) <- c("Year",choices)
  colnames(rp$table_after_tax) <- c("Year",choices)
  colnames(rp$table_operating_income) <- c("Year",choices)
  colnames(rp$table_after_tax_cash_flow) <- c("Year",choices)
  rp$table_before_tax <- rp$table_before_tax[,-1] %>% data.frame() 
  rp$table_after_tax <- rp$table_after_tax[,-1] %>% data.frame() 
  rp$table_operating_income <- rp$table_operating_income %>% data.frame() 
  rp$table_after_tax_cash_flow <- rp$table_after_tax_cash_flow %>% data.frame() 
  
})



output$table_before_tax <- DT::renderDataTable({
  if (length(rp[["table_before_tax"]])==0) return()
  tbl <- rp[["table_before_tax"]]
  tbl[7:9,] <- tbl[7:9,]*100
  tbl[10,] <- tbl[10,]*1000
  tbl <- round(tbl)
  tbl[7:9,] <- tbl[7:9,]/100
  tbl[10,] <- tbl[10,]/1000
  
  DT::datatable(tbl, 
                options = list(
                  scrollX = TRUE,
                  pageLength = 10,
                  bLengthChange =FALSE,
                  searching = FALSE
                )) %>% 
    formatCurrency(colnames(tbl[-10]))
})


output$table_after_tax <- DT::renderDataTable({
  if (length(rp[["table_after_tax"]])==0) return()
  tbl <- rp[["table_after_tax"]]
  tbl[7:9,] <- tbl[7:9,]*100
  tbl[10,] <- tbl[10,]*1000
  tbl <- round(tbl)
  tbl[7:9,] <- tbl[7:9,]/100
  tbl[10,] <- tbl[10,]/1000
  
  DT::datatable(tbl, 
                options = list(
                  scrollX = TRUE,
                  pageLength = 10,
                  bLengthChange =FALSE,
                  searching = FALSE
                )) %>% 
    formatCurrency(colnames(tbl[-10]))
})


output$table_operating_income  <- DT::renderDataTable({
  if (length(rp[["table_operating_income"]])==0) return()
  tbl <- round(rp[["table_operating_income"]])
  L <- length(tbl$Year)
  DT::datatable(tbl, 
                rownames = FALSE,
                options = list(
                  scrollX = TRUE,
                  pageLength = L,
                  bLengthChange =FALSE,
                  searching = FALSE)) %>% 
    formatCurrency(colnames(tbl)[-1])
})


output$table_after_tax_cash_flow  <- DT::renderDataTable({
  if (length(rp[["table_after_tax_cash_flow"]])==0) return()
  tbl <- round(rp[["table_after_tax_cash_flow"]])
  L <- length(tbl$Year)
  DT::datatable(tbl, 
                rownames = FALSE,
                options = list(
                  scrollX = TRUE,
                  pageLength = L,
                  bLengthChange =FALSE,
                  searching = FALSE)) %>% 
    formatCurrency(colnames(tbl)[-1])
})



output$profile_cashflow_chart <- renderGvis({
  if (length(rp[["table_after_tax_cash_flow"]])==0) return()
  tbl <- round(rp[["table_after_tax_cash_flow"]]) 
  varnames <- colnames(tbl)[-1]  
  gvisLineChart(tbl, xvar="Year", 
                yvar= varnames,
                options=list(
                  title="After-tax Cash Flow", 
                  vAxis="{title:'Net Annual Impact ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom",
                  width=800, height=400
                ))
})

output$profile_operating_income_chart <- renderGvis({
  if (length(rp[["table_operating_income"]])==0) return()
  
  tbl <- round(rp[["table_operating_income"]]) 
  varnames <- colnames(tbl)[-1]  
  gvisLineChart(tbl, xvar="Year", 
                yvar= varnames,
                options=list(
                  title="Before-tax Operating Income", 
                  vAxis="{title:'Net Annual Impact ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom",
                  width=800, height=400
                ))
})

output$profile_impacts <- renderGvis({ 
  if (length(rp[["table_after_tax"]])==0) return()
  tbl <- data.frame(
    profile=c("Barn Only","Retrofit Parlors","New Parlors","Robots"),
    before.tax=as.numeric(round(rp$table_before_tax[1,])),
    after.tax =as.numeric(round(rp$table_after_tax[1,]))
  ) 
  
  gvisColumnChart(tbl, xvar="profile",
                  yvar=c("before.tax","after.tax"),
                  options=list(title="Net Annual Impact ($)",
                               titleTextStyle="{fontSize:16}",
                               legend="top")
  )
})


