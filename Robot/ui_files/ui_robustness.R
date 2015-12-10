# ---------- Robustness ---------------------
div(
  # ---------- Sensitivity Analysis -----------   
  conditionalPanel('input.robust=="Sensitivity"', 
                   tags$hr(), 
                   fluidRow(column(3,h4("Sensitivity Analysis")),
                            column(2,bsButton("sensitivity_calculate","Calculate", style="primary")),
                            column(5,bsAlert("c_input_change"))),
                   a(id = "sensitivity_show","Show/hide sensitivity control"),
                   shinyjs::hidden(
                     div(id = "sensitivity_control",
                         fluidRow(column(5, h5("Variable")),
                                  column(2, offset=0, h5("% Change")),
                                  column(5, h5("Description"))),
                         fluidRow(column(5,
                                         selectInput("c_choice",NULL, width="100%",
                                                     c("Estimated cost per robot"="c1",
                                                       "Related housing changes needed per cow"="c2",
                                                       "Estimated annual change in milking system repair"="c3",
                                                       "Robots: years of useful life"="c4",
                                                       "Value of the robots after useful life"="c5",
                                                       "Anticipated savings in milking & chore labor"="c6",
                                                       "Projected change in milk production"="c7"
                                                     ))),
                                  column(2,
                                         numericInput("c_val",NULL, value=20, step=10)
                                  ),
                                  column(3,
                                         uiOutput("c_text")
                                  ) 
                         ),
                         div(bsAlert("c_toggle"),align="center")
                     ))
  ),
  # ---------- Scenarios Analysis -----------         
  conditionalPanel('input.robust=="Scenarios"', 
                   conditionalPanel('input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
                                    tags$hr(), 
                                    fluidRow(column(3,h4("Scenario Analysis")),
                                             column(2,bsButton("scenario_calculate","Calculate", style="primary")),
                                             column(5,bsAlert("s_input_change"))),
                                    a(id = "scenario_show","Show/hide scenario control"),
                                    shinyjs::hidden(
                                      div(id = "scenario_control",
                                          fluidRow(column(5,
                                                          selectInput("s_choice","Scenario", width="100%",
                                                                      choices=c("Increased investment"="s1",
                                                                                "Use less pellets"="s2",
                                                                                "New barn ($120k/stall)"="s3"
                                                                      ))),
                                                   column(5,bsAlert("s_toggle"))), 
                                          fluidRow(
                                            column(5,offset=1,
                                                   h5("Variable")),
                                            column(2,
                                                   h5("% Change")),
                                            column(3,
                                                   fluidRow(column(4,h5("Base")),
                                                            column(4,h5("New")),
                                                            column(4,h5("Unit"))))),
                                          conditionalPanel("input.s_choice=='s1'",
                                                           fluidRow(
                                                             column(5,offset=1,
                                                                    helpText("Estimated cost per robot")),
                                                             column(2, 
                                                                    numericInput("s_cost_robot", NULL, value=25, step=10)),
                                                             column(3,
                                                                    uiOutput("s_txt_cost_robot"))
                                                           )
                                          ),
                                          fluidRow(
                                            column(5,offset=1,
                                                   helpText("Related housing changes needed per cow")),
                                            column(2, 
                                                   numericInput("s_cost_housing_cow", NULL, value=-95, step=10)),
                                            column(3,
                                                   uiOutput("s_txt_cost_housing_cow"))
                                          )
                                          ,
                                          conditionalPanel("input.s_choice=='s2' | input.s_choice=='s3'",
                                                           fluidRow(
                                                             column(5,offset=1,
                                                                    helpText("Projected change in milk production (%)")),
                                                             column(2, 
                                                                    numericInput("s_milk_change", NULL, value=0, step=10)),
                                                             column(3,
                                                                    uiOutput("s_txt_milk_change"))
                                                           )
                                          ),
                                          conditionalPanel("input.s_choice=='s3' ",
                                                           fluidRow(
                                                             column(5,offset=1,
                                                                    helpText("Estimated percent change in SCC")),
                                                             column(2, 
                                                                    numericInput("s_scc_change", NULL, value=0, step=10)),
                                                             column(3,
                                                                    uiOutput("s_txt_scc_change"))
                                                           )
                                          ),
                                          conditionalPanel("input.s_choice=='s2'",
                                                           fluidRow(
                                                             column(5,offset=1,
                                                                    helpText("Pellets fed in robot booth")),
                                                             column(2, 
                                                                    numericInput("s_pellets", NULL, value=0, step=10)),
                                                             column(3,
                                                                    uiOutput("s_txt_pellets"))
                                                           )
                                          )
                                      ))
                   ),
                   conditionalPanel('input.robot_parlor!="OFF" & input.profile_choice!="Robots"',
                                    div(helpText("No scenario has been developed for this investment profile, yet."), align="center")
                   )
  )
)