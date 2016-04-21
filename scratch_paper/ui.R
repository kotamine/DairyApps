library(shiny)
library(mongolite)
library(DT)


shinyUI(pageWithSidebar(
  headerPanel("Scratch Paper"),
  sidebarPanel(
   numericInput("var1", "Var 1", value=1, step=2),
   actionButton("action","Action"),
   numericInput("var1_copy","Var 1 Copy", value=1, step=5),
   radioButtons("radio1","Choices",choices=c("a","b")), 
   lapply(c(1:3),function(x)  numericInput(paste0("moreVar", x), paste("More Var",x), value=x, step=1))
  ),
  
  mainPanel(
    uiOutput("showVar1"), br(),
    uiOutput("showVar2"), br(),
    uiOutput("showVar3"), br(),
    uiOutput("showVar4"), br(), 
    uiOutput("showVar5"), br(),
    uiOutput("showVar6"), br(),
    uiOutput("showVar7"), br(),
    uiOutput("showVar8"), br(),
    DT::dataTableOutput("dt1")
    )
))
