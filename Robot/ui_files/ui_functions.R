

refProfile <-  function(x) {
  switch(x, 
         "Freestall Barn"="_pr1",
         "Retrofit Parlors"="_pr2",
         "New Parlors"="_pr3",
         "Robots"="_pr4")
}

changeVariablesCapital<- function(x) {   
  # capital: herd increase, (total herd size), robot/parlor investment unit cost,  unit, (robot/parlor total), 
  # related housing changes per cow,(related housing total),  (total investment), (total investement per cow) 
  # repiar,  insurance
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
                      helpText("Anticipated increase in milking herd (animals)")),
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
                             helpText("Projected milking herd size (animals)")),
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
             #               fluidRow(column(width=8, helpText("Investment in Parlors")),
             #                               column(width=4, numericInput("cost_parlors",NULL,value=0,min=0,step=10000)),
             #                               column(width=4, helpText("dollars", align="center"))
             #                      )
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
             ),
             fluidRow(column(width=8, helpText("Insurance rate per $1000 value (%)")),
                      column(width=4, numericInput(paste0("insurance_rate",refProfile(x)),
                                                   NULL,value=0.5,min=0,step=0.1))
             ),
             fluidRow(column(width=8, helpText("Estimated annual change in milking system repair ($)")),
                      column(width=4, numericInput(paste0("repair",refProfile(x)),
                                                   NULL,value=7000,min=0,step=500))
             )
    )
  )
}    

 
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
             fluidRow(column(width=8, helpText("Milk lbs/robot/day (lbs/robot/day)")),
                      column(width=4, uiOutput(paste0("milk_lb_alt_day",refProfile(x))))
             ),
             #              conditionalPanel('input.robot_parlor=="ON" & input.profile_choice!="Robots"',
             #                               fluidRow(column(width=8, helpText("Projected milk lbs/day")),
             #                                        column(width=4, uiOutput("milk_alt_day")),
             #                                        column(width=4, helpText("lbs/day", align="center"))
             #                               ))
             fluidRow(column(width=8, helpText("Estimated percent change in SCC (%)")),
                      column(width=4, numericInput(paste0("scc_change",refProfile(x)),NULL,value=-5, min=-100, step=0.25))
             ),
             fluidRow(column(width=8, helpText("Projected change in dry matter intake (DMI) per day (lbs DM/day)")),
                      column(width=4, uiOutput(paste0("DMI_change",refProfile(x))))
             ),
             fluidRow(column(width=8, helpText("Pellets fed in robot booth (lb/cow/day)")),
                      column(width=4, numericInput(paste0("pellets",refProfile(x)),NULL,value=11,min=0,step=1))
             ),
             fluidRow(column(width=8, helpText("Extra cost for pellets fed in robot booth ($/ton)")),
                      column(width=4, numericInput(paste0("cost_pellets",refProfile(x)),NULL,value=20,min=0,step=2))
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
             div(style="background-color: #616D7E; color:white;",
                 fluidRow(column(width=4,  h5(strong("Item"), align="center")),
                          column(width=2,  h5(strong("Housing"), align="center")),
                          column(width=2,  
                                 conditionalPanel('input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
                                                  h5(strong("Robot 1"), align="center")), 
                                 conditionalPanel('input.robot_parlor=="ON" & input.profile_choice=="Retrofit Parlors"',
                                                  h5(strong("Retrofit"), align="center")),
                                 conditionalPanel('input.robot_parlor=="ON" & input.profile_choice=="New Parlors"',
                                                  h5(strong("Parlor"), align="center"))),
                          column(width=2, 
                                 conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                  & input.n_robot_life>=2",
                                                  h5(strong("Robot 2"), align="center")))
                 )), br(), 
             fluidRow(column(width=4,  helpText("Year of investment")),
                      column(width=2,  helpText("0")),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                        helpText("0"))),
                      column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                       & input.n_robot_life>=2",
                                                       uiOutput("yr_robot2"))) 
             ), 
             fluidRow(column(width=4,  helpText("Investment amount ($)")),
                      column(width=2,  uiOutput("copy_cost_housing")),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                        uiOutput("copy_cost_milking1"))),
                      column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                       & input.n_robot_life>=2",
                                                       uiOutput("copy_cost_milking2")))
             ), 
             fluidRow(column(width=4,  helpText("Down payment ($)")),
                      column(width=2,  
                             numericInput("down_housing",NULL,value=100000, min=0,step=20000)),
                      column(width=2,  
                             conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                              numericInput("down_milking1",NULL,value=0, min=0,step=20000))),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                        & input.n_robot_life>=2",
                                                        numericInput("down_milking2",NULL,value=50000, min=0, step=20000)))
             ), 
             fluidRow(column(width=4,  helpText("Loan amount ($)")),
                      column(width=2,  uiOutput("loan_housing")),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                        uiOutput("loan_milking1"))),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                        & input.n_robot_life>=2",
                                                        uiOutput("loan_milking2")))
             ),
             fluidRow(column(width=4,  helpText("Interest rate (%)")),
                      column(width=2,  uiOutput('copy_r_housing')),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                        uiOutput('copy_r_milking1'))),
                      column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                       & input.n_robot_life>=2",
                                                       uiOutput('copy_r_milking2')))
             ),
             shinyjs::hidden( 
               fluidRow(column(width=4,  helpText("Interest rate (%)")),
                        column(width=2,  numericInput("r_housing",NULL,value=4, min=0, step=.25)),
                        column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                          numericInput("r_milking1",NULL,value=4, min=0, step=.25))),
                        column(width=2, conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                                                & input.n_robot_life>=2",
                                                         numericInput("r_milking2",NULL,value=4, min=0, step=.25)))
               )) , 
             fluidRow(column(width=4,  helpText("Loan period (years)")),
                      column(width=2,  numericInput("n_yr_housing",NULL,value=24, min=0, step=1)),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                        numericInput("n_yr_milking1",NULL,value=12, min=0, step=1))),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                                                & input.n_robot_life>=2",
                                                        numericInput("n_yr_milking2",NULL,value=12, min=0, step=1)))
             ),
             fluidRow(column(width=4,  helpText("Salvage value ($)")),
                      column(width=2,  helpText("0")),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice!='Barn Only')",
                                                        uiOutput("copy_salvage_milking1"))),
                      column(width=2,  conditionalPanel("(input.robot_parlor=='OFF' | input.profile_choice=='Robots') 
                                                                                & input.n_robot_life>=2",
                                                        uiOutput("copy_salvage_milking2")))
                      
             )
    )
  )
}


