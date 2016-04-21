library(shiny)
library(DT)

attach(iris)

customHeaderPanel <- function(title,windowTitle=title){
  tagList(
    tags$head(
      tags$title(windowTitle),
      tags$link(rel="stylesheet", type="text/css",
                href="app.css"),
      tags$h1(a(href="www.someURLlogoLinksto.com"))
    )
  )
}

shinyServer(function(input, output,session) {
  
  var3 <- 1000
  rv <- reactiveValues(var4=0, var5=0, var6=0, var7=0, var8=0)
  
  
  changeVar1 <- reactive({
    
    isolate({
      tmp <- rv$var4  # recursive use of rv$var4
      })
    
    rv$var4 <- tmp + input$var1
    
    input$var1 + 1
  })
  
  var2 <- reactive({
    input$action
     isolate(
      input$var1 + 101 
     )
  })
  
  
  observeEvent(input$action, {
    var3 <- var3 + 100
    # var3 cannot be overwritten!
  })
  
  observeEvent(rv$var4, { 
     rv$var5 <- rv$var5 + 100 
  })
  
  var6 <- reactive({
    input$action
     rv$var6 <-  input$var1 + var2() + 1
     rv$var6
  })
  
  tmp_var7 <- reactive({ 
    rv$var7 <- rv$var6 + input$var1  # it doesn't react 
    1
  })
  
  observe({ 
    rv$var8 <- rv$var6 + input$var1
  })
  
  output$showVar1 <- renderUI({
    if (is.na(changeVar1())) {
      return() 
    }
    h3(changeVar1())
  })
  
  output$showVar2 <- renderUI({

    if (is.null(var2())) {
      return() 
    }
    h3(var2())
  })
  
  output$showVar3 <- renderUI({
    h3(var3)
  })
  
  output$showVar4 <- renderUI({
    h3(rv$var4)
  })
  
  output$showVar5 <- renderUI({
    h3(rv$var5)
  })
  
  output$showVar6 <- renderUI({
    h3(var6())
  })
  
  output$showVar7 <- renderUI({
      h3(rv$var7)  
  })
  
  output$showVar8 <- renderUI({
    h3(rv$var8)  
  })
  
  observe({
    # browser()
    if (1 < 0 ) {
      # never triggered
      input$var1
      stop('negative')
    }
  })
    
  observe({
  updateNumericInput(session,"var1", "Var 1", value=input$var1_copy, step=2)
  })
  
  observe({
  updateNumericInput(session,"var1_copy","Var 1 Copy", value=input$var1, step=5)
  })
  
 output$dt1 <- DT::renderDataTable({
   datatable(
     iris, extensions = 'Buttons', options = list(
       dom = 'Bfrtip',
       buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
     )
   )
 }) 
  
}) 