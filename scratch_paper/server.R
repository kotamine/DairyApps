library(shiny)
<<<<<<< Updated upstream
library(DT)

attach(iris)
=======
library(googleAuthR)
library(googleID)

options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email", 
                                        "https://www.googleapis.com/auth/userinfo.profile"))

# setwd("~/Documents/shiny/Shiny-Tests/scratch_paper")
# shiny::runApp(port=7209)

CLIENT_ID      <-  "639152778381-ft8ujn57j0mgh4p75lqpnmlr9ih7g6nn.apps.googleusercontent.com"
CLIENT_SECRET  <-  "xyxdwMCgkUAwqbMaX0LfpoL6"
CLIENT_URL     <-  "https://kotamine.shinyapps.io/scratch_paper/"
#CLIENT_URL     <-  'http://127.0.0.1:7209'  # URL on my laptop


options("googleAuthR.webapp.client_id" = CLIENT_ID)
options("googleAuthR.webapp.client_secret" = CLIENT_SECRET)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/userinfo.email",
                                          "https://www.googleapis.com/auth/userinfo.profile"))
# 
>>>>>>> Stashed changes

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
    googleAuthR::gar_auth(new_user=TRUE)

  })
  
  
  
  ## Get auth code from return URL
  access_token  <- reactiveAccessToken(session)
  
  ## Make a loginButton to display using loginOutput
  output$loginButton <- renderLogin(session, access_token())
  
  output$user_name <- renderText({
           browser()
          if (!is.null(access_token())) {
            user <- with_shiny(get_user_info, access_token())
            print(user)
            paste(user$emails)
          } else {
            NULL
          }
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