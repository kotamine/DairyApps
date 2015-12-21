# ----- Dashboard -----

uiDashboard <- function(x) { 
div( 
  fluidRow(
  column(4,
         fluidRow(
           h5("Results"),
           column(6,
                  uiOutput(paste0("IOFC",refProfile(x))),
                  radioButtons(paste0("IOFC",NULL,choices=c("per cow","per cwt"), selected="per cwt")),
           column(6,
                  uiOutput(paste0("NAI",refProfile(x)))),
                  radioButtons(paste0("NAI",NULL, choices=c("before tax","after tax"),selected="after tax")) 
         ),
          htmlOutput(paste0("cashflow_chart",refProfile(x)))
         )),
  column(8,
         div(fluidRow(
           h5("Components"),
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
         ), align="center")
  )
) 
)
} 

