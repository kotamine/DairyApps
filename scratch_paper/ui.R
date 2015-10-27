shinyUI(pageWithSidebar(
  
  headerPanel("Scratch Paper"),
  
  sidebarPanel(
   numericInput("var1", "Var 1", value=1, step=2),
   actionButton("action","Action")
  ),
  
  mainPanel(
    uiOutput("showVar1"), br(),
    uiOutput("showVar2"), br(),
    uiOutput("showVar3"), br(),
    uiOutput("showVar4")
  )
))