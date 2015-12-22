# ----- Dashboard -----

uiDashboard <- function(x) {  
div(  
  fluidRow(
  column(4,
         h4("Results",align="center"),
         fluidRow(
           column(6,
                  uiOutput(paste0("IOFC",refProfile(x))),
                  radioButtons(paste0("IOFC",refProfile(x)),NULL,
                               choices=c("per cow","per cwt"), selected="per cwt")
                  ),
           column(6,
                   uiOutput(paste0("NAI",refProfile(x))),
                  radioButtons(paste0("NAI",refProfile(x)),NULL,
                               choices=c("before tax","after tax"),selected="after tax")
                  )
           ),
          htmlOutput(paste0("cashflow_small_chart",refProfile(x)))
         ),
  column(8,
         div(
           h4("Components",align="center"),
           fluidRow(
           column(4,
                  plotOutput(paste0("plot1",refProfile(x)), height = 200),
                  uiOutput(paste0("milk_feed",refProfile(x)))),
           column(4,
                  plotOutput(paste0("plot2",refProfile(x)), height = 200),
                  uiOutput(paste0("labor_repair",refProfile(x)))),
           column(4,
                  plotOutput(paste0("plot3",refProfile(x)), height = 200),
                  uiOutput(paste0("captial_cost",refProfile(x))))
         ),
         fluidRow(column(6,uiOutput(paste0("misc",refProfile(x)))),
                  column(6,uiOutput(paste0("inflation",refProfile(x))))
         ),
        align="center")
  ))
) 
}  

