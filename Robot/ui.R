library(shiny)
library(shinyBS)
# library(shinydashboard)
# library(googlesheets)
suppressPackageStartupMessages(library(dplyr))

#source("helpers.R")

shinyUI(
     navbarPage(
          "Robotic Milking System",
          tabPanel("Introduction",
                   fluidRow(column(width=1),
                            column(width=10,
                                   h4("Why robots?"),
                                   p("..."),
                                   h4("Where to start?"),
                                   p("..."),
                                   h4("How will this tool help me?"),
                                   p("..."),
                                   helpText("* Do we want to put some picture here?")
                            ))
          ),
          tabPanel(
               "Data Entry",
#                h5("Please enter data on your farm and assumptions for the analysis. The default values are set for..."), 
#                br(),
               navlistPanel(
                    "Data and Assumptions",
                    tabPanel(
                         "Farm Finance", 
                         fluidRow(
                              column(
                                   width=1),
                              column(
                                   width=10,
                                        fluidRow(column(width=6, offset=, h5(strong("Item"), align="center")),
                                                 column(width=3, h5(strong("User Data"), align="center")), 
                                                 column(width=3, h5(strong("Unit"), align="center"))
                                        ),
#                                         fluidRow(column(width=12, h5("Herd and financial assumptions", align="left"))
#                                         ),
                                        fluidRow(column(width=6, helpText("Current herd size (milking & dry)")),
                                                 column(width=3, numericInput("herd_size",NULL,value=120,min=30,step=10)),
                                                 column(width=3, helpText("animals", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Anticipated increase in milking herd with robots")),
                                                 column(width=3, numericInput("herd_increase",NULL,value=0,step=10)),
                                                 column(width=3, helpText("animals", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Herd size with robots")),
                                                 column(width=3, helpText("calc. value")),
                                                 column(width=3, helpText("units", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Number of robots")),
                                                 column(width=3, numericInput("n_robot",NULL,value=2,min=0,step=1)),
                                                 column(width=3, helpText("units", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Estimated cost per robot")),
                                                 column(width=3, numericInput("cost_robot",NULL,value=180000,min=50000,step=10000)),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Total investment for the robots alone")),
                                                 column(width=3,  helpText("calc. value")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Related housing changes needed per cow")),
                                                 column(width=3, numericInput("cost_housing_cow",NULL,value=9500,min=0,step=500)),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Related housing changes needed")),
                                                 column(width=3,  helpText("calc. value")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Total investment for the robots and housing")),
                                                 column(width=3,  helpText("calc. value")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Toal investment per cow")),
                                                 column(width=3,  helpText("calc. value")),
                                                 column(width=3, helpText("dollars", align="center"))
                                        ),
                                        fluidRow(column(width=6, helpText("Interest rate required on the overall investment")),
                                                 column(width=3,  numericInput("interest",NULL,value=3.0,min=0.0,step=0.1)),
                                                 column(width=3, helpText("percent", align="center"))
                                        ), br(), br() 
                                   
                              )),
                         icon=icon("dollar")),
                    tabPanel("Maintenance", p("summary"),  icon=icon("wrench")), 
                    tabPanel("Labor Savings", p("table"),  icon=icon("male")),
                    tabPanel("Milk Outputs", p("table"), icon=icon("bell-o")),
                    tabPanel("Feed", p("table"), icon=icon("truck")),
                    tabPanel("Replacement", p("table"), icon=icon("eyedropper")),
                    tabPanel("Utilties", p("table"), icon=icon("lightbulb-o")),
                    tabPanel("Inflations", p("table"), icon=icon("money"))         
                    )
          ),
          tabPanel("Economic Analysis"),
          navbarMenu("More",
                     tabPanel("Sensitivity"),
                     tabPanel("Scenarios"),
                     tabPanel("Cash Flow")),
          tabPanel("About",
                   h5("Credits"),
                   h5("Contacts"),
                   h5("More resources"))
     ))




