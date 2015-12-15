

refProfile <-  function(x) {
  switch(x, 
         "Robots"=base_profiles[1],
         "Retrofit Parlors"=base_profiles[2],
         "New Parlors"=base_profiles[3],
         "Retrofit/Robots"=combo_profiles[1],
         "Retrofit/New"=combo_profiles[2]
  )
}

changeVariablesCapital<- function(x) {   
  # capital: herd increase, (total herd size), robot/parlor investment unit cost,  unit, (robot/parlor total), 
  # related housing changes per cow,(related housing total),  (total investment), (total investement per cow) 
  div(   
    tabPanel(paste(x), 
             div(style="background-color:#4863A0; color:white;",
                 fluidRow(column(width=8, 
                                 h5(strong("Item"),align="center")),
                          column(width=4, h5(strong("User Data"), 
                                             align="center"))
                 )
             ), br(),
             fluidRow(
               column(width=8,
                      helpText("Anticipated increase in milking herd (cows)")),
               column(width=4, 
                      numericInput(paste0("herd_increase",refProfile(x)),NULL,value=0,step=10))
             ), 
             fluidRow(column(width=8, helpText("Additional labor expense with herd expansion ($/cow)")),
                      column(width=4,  numericInput(paste0("additional_labor",refProfile(x)),
                                                    NULL,value=450,step=50,min=0))
             ), 
             fluidRow(column(width=8, helpText("Other expense with herd expansion ($/cow)")),
                      column(width=4,  numericInput(paste0("additional_cost",refProfile(x)),
                                                    NULL,value=200,step=50,min=0))
             ),
             fluidRow(column(width=8, 
                             helpText("Projected milking herd size (cows)")),
                      column(width=4, uiOutput(paste0("herd_size2",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText(paste("Unit cost for", x, "($)"))),
                      column(width=4, numericInput(paste0("cost_robot",refProfile(x)),
                                                   NULL,value=180000,min=50000,step=10000))
             ),
             fluidRow(column(width=8, helpText(paste("Number of", x,"(units)"))),
                      column(width=4, numericInput(paste0("n_robot",refProfile(x)),NULL,value=2,min=0,step=1))
             ),
             fluidRow(column(width=8, helpText("Total investment for", x, "alone ($)")),
                      column(width=4, uiOutput(paste0("cost_milking",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText("Housing changes per cow ($)")),
                      column(width=4, numericInput(paste0("cost_housing_cow",refProfile(x)),
                                                   NULL,value=9500,min=0,step=500))
             ),
             fluidRow(column(width=8, helpText("Total housing changes ($)")),
                      column(width=4,   uiOutput(paste0("cost_housing",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText("Total investment ($)")),
                      column(width=4,   uiOutput(paste0("total_investment",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText("Toal investment per cow ($/cow)")),
                      column(width=4,   uiOutput(paste0("total_investment_cow",refProfile(x))))
             )
    )
  )
}    

changeVariablesMaintenace <- function(x) {   
  # capital:  repiar, life of milking system, cycles, (planning horizon), insurance value, insurance
  div(   
    tabPanel(paste(x), 
             div(style="background-color:#4863A0; color:white;",
                 fluidRow(column(width=8, 
                                 h5(strong("Item"),align="center")),
                          column(width=4, h5(strong("User Data"), 
                                             align="center"))
                 )
             ), br(),
             fluidRow(column(width=8, helpText("Estimated annual change in milking system repair ($)")),
                      column(width=4, numericInput(paste0("repair",refProfile(x)),
                                                   NULL,value=7000,min=0,step=500))
             ),
             fluidRow(column(width=8, helpText(paste0(x, " useful life (years)"))),
                      column(width=4, numericInput("useful_years",NULL,value=15, min=0, step=1))
             ),
             fluidRow(column(width=8, helpText(paste("Sets of ", x, " in planing horizon"))),
                      column(width=4, radioButtons(paste0("n_sets",refProfile(x)),NULL, choices=c("one"=1, "two"=2), selected=2, inline=TRUE))
             ),
             fluidRow(column(width=8, helpText("Planning horizon to be used in calculation")),
                      column(width=4, uiOutput(paste0("planning_horizon",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText(paste("Salvage value of", x, "in today's term ($)"))),
                      column(width=4, numericInput("salvage_milking1",NULL,value=45000,min=0,step=1000))
             ),
             fluidRow(column(width=8, helpText("Increased insurance value of milking system & housing")),
                      column(width=4, uiOutput(paste0("increased_insurance",refProfile(x))))
             ), 
             fluidRow(column(width=8, helpText("Insurance rate per $1000 value (%)")),
                      column(width=4, numericInput(paste0("insurance_rate",refProfile(x)),
                                                   NULL,value=0.5,min=0,step=0.1))
             )
    )
  ) 
}            


# uiIf <- function(x, x1, condition, contents, contents_null=NULL) { 
#   lapply(x, function(x1) {  
#     if(condition) {
#       contents
#     } else {
#       if (!is.null(contents_null)) contents_null
#     }
#   })
# }

changeVariablesMilkfeed<- function(x) {
  #   milkfeed: milk increase, (projected milk per day),
  #   scc change, projected change dry matter, pellets, pellets cost, software, turnover rate
  div(
    tabPanel(paste(x), 
             div(style="background-color:#4863A0; color:white;",
                 fluidRow(column(width=8, 
                                 h5(strong("Item"),align="center")),
                          column(width=4, h5(strong("User Data"), 
                                             align="center"))
                 )
             ), br(),
             fluidRow(column(width=8, helpText("Projected change in milk production (lbs/cow/day)")),
                      column(width=4, numericInput(paste0("milk_change",refProfile(x)),NULL,value=10,step=2))
             ),
             lapply(x, function(x1) { 
               if(x1==base_profiles[1]) {
                 div(fluidRow(column(width=8, helpText("Projected milk output throgh Robots (lbs/robot/day)")),
                              column(width=4, uiOutput(paste0("milk_lb_alt_day",refProfile(x1))))
                 ), 
                 fluidRow(column(width=8, helpText("Projected change in dry matter intake (DMI) per day (lbs DM/day)")),
                          column(width=4, uiOutput(paste0("DMI_change",refProfile(x))))
                 ), 
                 fluidRow(column(width=8, helpText("Pellets fed in robot booth (lb/cow/day)")),
                          column(width=4, numericInput(paste0("pellets",refProfile(x)),NULL,value=11,min=0,step=1))
                 ), 
                 fluidRow(column(width=8, helpText("Extra cost for pellets fed in robot booth ($/ton)")),
                          column(width=4, numericInput(paste0("cost_pellets",refProfile(x)),NULL,value=20,min=0,step=2))
                 )) 
               } else {
                 fluidRow(column(width=8, helpText("Projected milk output (lbs/cow/day)")),
                          column(width=4, uiOutput(paste0("milk_alt_day",refProfile(x1))))
                 )
               }
             }),    
             fluidRow(column(width=8, helpText("Estimated percent change in SCC (%)")),
                      column(width=4, numericInput(paste0("scc_change",refProfile(x)),NULL,value=-5, min=-100, step=0.25))
             ),
             fluidRow(column(width=8, helpText("Reproduction and herd health value of software ($/cow/year)")),
                      column(width=4, numericInput(paste0("software",refProfile(x)),NULL,value=35,min=0, step=1))
             ),
             fluidRow(column(width=8, helpText("Anticipated change in annual turnover rate (%)")),
                      column(width=4,  numericInput(paste0("change_turnover",refProfile(x)),NULL,value=-1.0,step=0.25))
             )
    ))
}


changeVariablesLaborenergy<- function(x) {
  #   labor: labor saving milking, (anticipated milking chore), labor saving heat detection,
  #   increased records management, reduced labor management, electricity, water, chemical  
  div(
    tabPanel(paste(x),  
             div(style="background-color:#4863A0; color:white;",
                 fluidRow(column(width=8, 
                                 h5(strong("Item"),align="center")),
                          column(width=4, h5(strong("User Data"), 
                                             align="center"))
                 )
             ), br(),
             fluidRow(column(width=8, helpText("Anticipated savings in milking & chore labor (hours/day)")),
                      column(width=4, numericInput(paste0("hr_sv_milking",refProfile(x)),NULL,value=10.82, min=0, step=.2))
             ),
             fluidRow(column(width=8, helpText("Anticipated hours of milking & chore labor (hours/day)")),
                      column(width=4, uiOutput(paste0("anticipated_hours_milking",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText("Current hours of heat detection (hours/day)")),
                      column(width=4, numericInput(paste0("hr_heat_detection",refProfile(x)),NULL,value=0.65,min=0,step=.05))
             ),
             fluidRow(column(width=8, helpText("Anticipated hours of heat detection (hours/day)")),
                      column(width=4, numericInput(paste0("anticipated_hours_heat",refProfile(x)),
                                                   NULL,value=0.25,min=0,step=0.05))
             ),
             fluidRow(column(width=8, helpText("Increased hours of records management (hours/day)")),
                      column(width=4, numericInput(paste0("increase_rc_mgt",refProfile(x)),NULL,value=0.6,step=0.1))
             ),
             fluidRow(column(width=8, helpText("Reduced hours of labor management (hours/day)")),
                      column(width=4, numericInput(paste0("decrease_lab_mgt",refProfile(x)),NULL,value=0.6,min=0,step=0.1))
             ),
             fluidRow(column(width=8, helpText("Anticipated change in electricity cost ($/cow)")),
                      column(width=4, numericInput(paste0("change_electricity",refProfile(x)),NULL,value=8.25,step=0.25))
             ),
             fluidRow(column(width=8, helpText("Anticipated change in water cost ($/cow)")),
                      column(width=4, numericInput(paste0("change_water",refProfile(x)),NULL,value=-3.00,step=0.25))
             ),
             fluidRow(column(width=8, helpText("Anticipated change in chemical cost ($/cow)")),
                      column(width=4, numericInput(paste0("change_chemical",refProfile(x)),NULL,value=1.50,step=0.25))
             )
    ))
} 




changeVariablesFinance<- function(x) {
  #          Finance: year of investment, invetment amount, downpayment, loan, interest, loan period, salvage value
  div( 
    tabPanel("Finance", 
             div(style="background-color: #4863A0; color:white;",
                 fluidRow(column(width=6,  h5(strong("Item"), align="center")),
                          column(width=2,  h5(strong("Housing"), align="center")),
                          column(width=2,  h5(strong(paste(x, "1")), align="center")),
                          column(width=2,  div(id=paste0(refProfile(x),2),h5(strong(paste(x, "2")), align="center")))
                 )), br(),  
             fluidRow(column(width=6,  helpText("Year of investment")),
                      column(width=2,  helpText("0")),
                      column(width=2,  numericInput(paste0("yr_system1",refProfile(x)),NULL,value=0,min=0,step=1)),
                      column(width=2,  div(id=paste0(refProfile(x),2),uiOutput(paste0("yr_system2",refProfile(x)))))
             ), 
             fluidRow(column(width=6,  helpText("Investment amount at the time of investement ($)")),
                      column(width=2,  uiOutput(paste0("copy_cost_housing",refProfile(x)))),
                      column(width=2,  uiOutput(paste0("copy_cost_milking1",refProfile(x)))),
                      column(width=2,  div(id=paste0(refProfile(x),2), uiOutput(paste0("copy_cost_milking2",refProfile(x)))))
             ), 
             div(id=paste0(refProfile(x),"delay",1),
             fluidRow(column(width=6,  helpText(paste("Delayed portion of the investment till", x,"($)"))),
                      column(width=2,  numericInput(paste0("delay_housing1",refProfile(x)),NULL,value=0, min=0,step=20000)),
                      column(width=2,  helpText("-",align="center")),
                      column(width=2,  div(id=paste0(refProfile(x),2),  helpText("-",align="center")))
             )), 
#              div(id=paste0(refProfile(x),"delay",2),
#                  fluidRow(column(width=4,  helpText(paste("Delayed investment of housing till", x,"2 ($)"))),
#                           column(width=2,  numericInput(paste0("delay_housing2",refProfile(x)),NULL,value=0, min=0,step=20000)),
#                           column(width=2,  helpText("-",align="center")),
#                           column(width=2,  helpText("-",align="center"))
#               )), 
             fluidRow(column(width=6,  helpText("Down payment ($)")),
                      column(width=2,  numericInput(paste0("down_housing",refProfile(x)),NULL,value=100000, min=0,step=20000)),
                      column(width=2,  numericInput(paste0("down_milking1",refProfile(x)),NULL,value=0, min=0,step=20000)),
                      column(width=2,  div(id=paste0(refProfile(x),2), 
                                           numericInput(paste0("down_milking2",refProfile(x)),NULL,value=50000, min=0, step=20000)))
             ),              
             fluidRow(column(width=6,  helpText("Loan amount ($)")),
                      column(width=2,   uiOutput(paste0("loan_housing",refProfile(x)))),
                      column(width=2,   uiOutput(paste0("loan_milking1",refProfile(x)))),
                      column(width=2,  div(id=paste0(refProfile(x),2), 
                                           uiOutput(paste0("loan_milking2",refProfile(x)))))
             ),     
             fluidRow(column(width=6,  helpText("Interest rate (%)")),
                      column(width=2,  uiOutput(paste0("r_housing",refProfile(x)))),
                      column(width=2,  uiOutput(paste0("r_milking1",refProfile(x)))),
                      column(width=2,  div(id=paste0(refProfile(x),2), 
                                           uiOutput(paste0("r_milking2",refProfile(x)))))
             ),     
             #              shinyjs::hidden( 
             #                fluidRow(column(width=4,  helpText("Interest rate (%)")),
             #                         column(width=2,  numericInput("r_housing",NULL,value=4, min=0, step=.25)),
             #                         column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
             #                                                           numericInput("r_milking1",NULL,value=4, min=0, step=.25))),
             #                         column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
             #                                                                                 & input.n_robot_life>=2",
             #                                                          numericInput("r_milking2",NULL,value=4, min=0, step=.25)))
             #                )) , 
             fluidRow(column(width=6,  helpText("Loan period (years)")),
                      column(width=2,  numericInput(paste0("n_yr_housing",refProfile(x)),NULL,value=24, min=0, step=1)),
                      column(width=2,  numericInput(paste0("n_yr_milking1",refProfile(x)),NULL,value=12, min=0, step=1)),
                      column(width=2,  div(id=paste0(refProfile(x),2), 
                                           numericInput(paste0("n_yr_milking2",refProfile(x)),NULL,value=12, min=0, step=1)))
             ),  
             fluidRow(column(width=6,  helpText("Salvage value at the time of purchase ($)")),
                      column(width=2,  helpText("0")),
                      column(width=2,  uiOutput(paste0("copy_salvage_milking1",refProfile(x)))),
                      column(width=2,  div(id=paste0(refProfile(x),2), 
                                           uiOutput(paste0("copy_salvage_milking2",refProfile(x)))))
             )
    )
  ) 
}





changeVariablesCombo <- function(x, x1, x2) {
  #       Combination profle choices: timing of investment for switching and the delayed portion of the investment in housing.   
  div( 
    tabPanel("Finance", 
             div(style="background-color: #4863A0; color:white;",
                 fluidRow(column(width=6,  h5(strong("Item"), align="center")),
                          column(width=2,  h5(strong("Housing"), align="center")),
                          column(width=2,  h5(strong(paste(x1)), align="center")),
                          column(width=2,  h5(strong(paste(x2)), align="center"))
                 )), br(),  
             fluidRow(column(width=6,  helpText("Year of investment")),
                      column(width=2,  helpText("0")),
                      column(width=2,  numericInput(paste0("yr_system1",refProfile(x)),NULL,value=0,min=0,step=1)),
                      column(width=2,  numericInput(paste0("yr_system2",refProfile(x)),NULL,value=5,min=0,step=1))
             ), 
             fluidRow(column(width=6,  helpText("Investment amount at the time of investement ($)")),
                      column(width=2,  uiOutput(paste0("copy_cost_housing",refProfile(x)))),
                      column(width=2,  uiOutput(paste0("copy_cost_milking1",refProfile(x)))),
                      column(width=2,  uiOutput(paste0("copy_cost_milking2",refProfile(x))))
             ),
             div(id=paste0(refProfile(x),"delay",1),
                 fluidRow(column(width=6,  helpText(paste("Delayed portion of the investment till", x2," ($)"))),
                          column(width=2,  numericInput(paste0("delay_housing1",refProfile(x)),NULL,value=0, min=0,step=20000)),
                          column(width=2,  helpText("-",align="center")),
                          column(width=2,  helpText("-",align="center"))
                 ))
    )
    )
}

