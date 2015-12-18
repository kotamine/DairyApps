
div(
# # An alternative way to show/hide a section 
# checkboxInput("customDMI","Show calculations of projected DMI change",value=FALSE),

# conditionalPanel("input.customDMI",
a(id = "customDMI","Show/hide calculations of projected DMI change"),
shinyjs::hidden(
  div(id = "DMI_inputs",
      div(style="background-color: #616D7E; color:white;",
          fluidRow(column(width=6, h5(strong("Item"), align="center")),
                   column(width=3, h5(strong("Value"), align="center")), 
                   column(width=3, h5(strong("Coefficient"), align="center"))
          )), br(),
      fluidRow(column(width=6, helpText("Milk per cow per day")),
               column(width=3, uiOutput("milk_cow_day_copy")),
               column(width=3, numericInput("milk_cow_coeff",NULL,value=0.4,min=0,step=0.1))
      ),
      fluidRow(column(width=6, helpText("Milk fat content (%)")),
               column(width=3, numericInput("milk_fat",NULL,value=3.65,min=0,step=0.2)),
               column(width=3, numericInput("milk_fat_coeff",NULL,value=15,min=0,step=0.5))
      ),
      fluidRow(column(width=6, helpText("Milk/cow/day adjusted to 4% fat ")),
               column(width=3, uiOutput("adj_milk_cow_day")), 
               column(width=3, numericInput("adj_milk_cow_coeff",NULL,value=0.372,min=0,step=0.1))
      ),
      fluidRow(column(width=6, helpText("Milking herd avg body weight (lb)")),
               column(width=2, numericInput("body_weight",NULL,value=1500,min=1000,step=50)),
               column(width=2,numericInput("body_weight_coeff1",NULL,value=0.0968,min=0,step=0.005)),
               column(width=2,numericInput("body_weight_coeff2",NULL,value=0.75,min=0,step=0.05))
      ),
      fluidRow(column(width=6, helpText("Lactation weeks")),
               column(width=2, numericInput("lcatation_week",NULL,value=24,min=0,step=1)),
               column(width=2,numericInput("lactation_coeff1",NULL,value=-0.192,min=0,step=0.01)),
               column(width=2,numericInput("lactation_coeff2",NULL,value=3.67,min=0,step=0.05))
      ),
      fluidRow(column(width=6, helpText("Stage of lactation adjustment")),
               column(width=3, uiOutput("stage_lactation"))
      ),
      fluidRow(column(width=6, helpText("Current DMI per day")),
               column(width=3, uiOutput("DMI_day"))
      ), br(),
      fluidRow(column(width=6, helpText("Projected change in milk production (lbs/cow/day)")),
               column(width=3, uiOutput("rep_milk_change"))
      ),
      fluidRow(column(width=6, 
                      conditionalPanel('input.robot_parlor=="OFF" | input.profile_choice=="Robots"',
                                       helpText("Projected DMI per day with robots")),
                      conditionalPanel('input.robot_parlor=="ON" & input.profile_choice!="Robots"',
                                       helpText("Projected DMI per day with parlor investment"))),
               column(width=3, uiOutput("DMI_projected"))
      ),
      fluidRow(column(width=3, offset=9,
                      span(actionButton("coeff_reset","reset"),align="center"))
      ),
      br(), br()
  ))
)