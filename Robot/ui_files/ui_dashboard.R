# ----- Dashboard -----

uiDashboard <- function(x) {   
  div(    
    lapply(x, function(x1) {  
      if (!grepl("_se",x)) {
        fluidRow(column(6, offset=3,
                        div(class="well well-sm", h4(paste(refProfileName(x))),align="center")))
      } 
    }), 
    fluidRow( 
      column(4, 
             h3("Results",align="center"),
             fluidRow(
               column(6,
                      uiOutput(paste0("IOFC",x)),
                      div(id=paste0('radio_IOFC',x),radioButtons(paste0("IOFC",x),NULL,
                                   choices=c("per cow","per cwt"), selected="per cwt"))
               ),
               column(6,
                      uiOutput(paste0("NAI",x)),
                      div(id=paste0('radio_NAI',x), radioButtons(paste0("NAI",x),NULL,
                                   choices=c("before tax","after tax"),selected="after tax"))
               )
             ),
             lapply(x, function(x1) {  
               if (!grepl("_se", x))  div(htmlOutput(paste0("cashflow_small_chart",x1)), align="center")
             })   
      ),   
      column(8,
             div(
               h3("Components",align="center"),
               fluidRow(
                 column(4,
                        uiOutput(paste0("milk_feed",x))
                 ),
                 column(4,
                        uiOutput(paste0("labor_repair",x)) 
                 ),
                 column(4,
                        uiOutput(paste0("capital",x)) 
                 )
               ),
               fluidRow(column(6,uiOutput(paste0("misc",x))),
                        column(6,uiOutput(paste0("inflation",x)))
               ),
               lapply(x, function(x1) {  
                 if (!grepl("_se", x)) {
                   fluidRow(column(4, htmlOutput(paste0("plot1",x), height = 200)),
                            column(4, htmlOutput(paste0("plot2",x), height = 200)),
                            column(4, htmlOutput(paste0("plot3",x), height = 200))
                   )
                 }
               }), 
               align="center") 
      )), br(), br() 
  )  
}    

