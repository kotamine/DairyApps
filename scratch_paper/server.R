library(shiny)


shinyServer(function(input, output) {
  
  var3 <- 1000
  rv <- reactiveValues(var4=0, var5=0)
  
  
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
})