


varnames_production_change <- c("herd_increase", "repair","insurance","anticipated_hours_heat","increase_rc_mgt",
  "decrease_lab_mgt", "milk_change","scc_change","software", 
  "pellets","cost_pellets","change_turnover","change_electricity",
  "change_water", "change_chemical")

mins_production_change <- c(rep(0,12), -100,rep(0,3))
steps_production_change <- c(10, 500, 0.1, 0.2, 0.05, 0.1,
                            0.1, 2, 0.25, 5, 2, 
                            2, 0.25, 0.25, 0.25, 0.25)

update_profile_vars  <- function(varnames, mins, steps, varname_ref=NULL) {
  lapply (varnames, function(x) {
    n <- which(varnames==paste0(x))
    updateNumericInput(session, paste0(x),NULL, 
                       value=input[[paste0(x,varname_ref)]],
                       min=mins[n],
                       step=steps[n])
  }) 
}

update_profile_vars2  <- function(varnames, mins, steps, varname_ref=NULL) {
  lapply (varnames, function(x) {
    n <- which(varnames==paste0(x))
    updateNumericInput(session, paste0(x,varname_ref),NULL, 
                       value=input[[paste0(x)]],
                       min=mins[n],
                       step=steps[n])
  }) 
}

# update variables in Data Entry when their counterparts change in Robot vs Parlors
observe({
  if (input$robot_parlor=="OFF") { return() }
  if (input$profile_choice=="Barn Only") {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr1")
  }
  else if (input$profile_choice=="Retrofit Parlors") {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr2")
  } else if (input$profile_choice=="New Parlors") {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr3")
  } else {
    update_profile_vars(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr4")
  }
})

# update variables in Robot vs Parlors when their counterparts change in Data Entry 
observe({
  if (input$robot_parlor=="OFF") { return() }
  if (input$profile_choice=="Barn Only") {
    update_profile_vars2(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr1")
  }
  else if (input$profile_choice=="Retrofit Parlors") {
    update_profile_vars2(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr2")
  } else if (input$profile_choice=="New Parlors") {
    update_profile_vars2(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr3")
  } else {
    update_profile_vars2(varnames_production_change,
                        mins_production_change, 
                        steps_production_change, 
                        varname_ref="_pr4")
  }
})





       
         
fluidRow(column(width=4, helpText("Years of Useful Life")),
         column(width=2, uiOutput("housing_years_pr4")), # This becomes the planning horizon
         column(width=2, numericInput("milking_years1_pr4",NULL,value=15,min=0,step=1)),
         column(width=2, uiOutput("housing_years2_pr4"))
), 

numericInput("yr_invest_milking1_pr4",NULL,value=0,min=0,step=1))
numericInput("yr_invest_milking2_pr4",NULL,value=15,min=0,step=1)


fluidRow(column(width=4,  helpText("Housing Investment per cow ($)")), 
         column(width=2, numericInput("cost_housing_cow_pr4",NULL,value=9500,min=0,step=500))
), 
fluidRow(column(width=4,  helpText("Investment amount ($)")), 
         column(width=2, uiOutput("cost_housing_pr4")),
         column(width=2, numericInput("cost_milking1_pr4",NULL,value=360000,min=0,step=20000)),
         column(width=2, numericInput("cost_milking2_pr4",NULL,value=450084,min=0,step=20000))
), 
fluidRow(column(width=4, helpText("Down payment ($)")),
         column(width=2,  
                numericInput("down_housing_pr4",NULL,value=100000, min=0,step=20000)),
         column(width=2,  
                numericInput("down_milking1_pr4",NULL,value=0, min=0,step=20000)),
         column(width=2,  
                numericInput("down_milking2_pr4",NULL,value=0, min=0,step=20000))
), 
fluidRow(column(width=4,  helpText("Loan amount ($)")),
         column(width=2,  uiOutput("loan_housing_pr4")),
         column(width=2,  uiOutput("loan_milking1_pr4")),
         column(width=2,  uiOutput("loan_milking2_pr4"))
),
fluidRow(column(width=4,  helpText("Interest rate (%)")),
         column(width=2,  numericInput("r_housing_pr4",NULL,value=4, min=0, step=.25)),
         column(width=2,  numericInput("r_milking1_pr4",NULL,value=4, min=0, step=.25)),
         column(width=2,  numericInput("r_milking2_pr4",NULL,value=4, min=0, step=.25))
), 
fluidRow(column(width=4,  helpText("Loan period (years)")),
         column(width=2,  numericInput("n_yr_housing_pr4",NULL,value=24, min=0, step=1)),
         column(width=2,  numericInput("n_yr_milking1_pr4",NULL,value=12, min=0, step=1)),
         column(width=2,  numericInput("n_yr_milking2_pr4",NULL,value=12, min=0, step=1))
),
fluidRow(column(width=4,  helpText("Salvage value ($)")),
         column(width=2,  numericInput("salvage_housing_pr4",NULL,value=0, min=0, step=5000)),
         column(width=2,  numericInput("salvage_milking1_pr4",NULL,value=45000, min=0, step=5000)),
         column(width=2,  uiOutput("salvage_milking2_pr4"))
)


