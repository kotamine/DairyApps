# ------------- New Post --------------------
# title="New Post", id="postTab",
div(
  div(id = "post_form",
      textInput("post_name","App Name", value=""),
      selectInput("post_category","Category", 
                  choices=c("Milk","Forage","Labor","Social")),
      h5(strong("Description")), 
      uiOutput("resetable_post"), 
      br(), br(),
      actionButton("gmail1","Google Account","primary"), br(), br(),    
      textInput("user_name","User Name"),
      textInput("email_address","Email Address"),
      actionButton("post_send", "Send","primary"),
      shinyjs::hidden(
        span(id = "submitMsg", "Sending...", style = "margin-left: 15px;")
      )
  ),
  shinyjs::hidden(
    div(id = "error",
        div(br(), tags$b("Error: "), span(id = "errorMsg")),
        style = "color: red;"
    )
  ),
  # hidden input field 
  shinyjs::hidden(textInput("status","", value="Active")),
  shinyjs::hidden(numericInput("edits","", value=0)),
  shinyjs::hidden(numericInput("current_views","", value=0)),
  shinyjs::hidden(numericInput("cumulative_views","", value=0)),
  shinyjs::hidden(numericInput("current_comments","", value=0)),
  shinyjs::hidden(numericInput("cumulative_comments","", value=0)),
  shinyjs::hidden(numericInput("average_interest","",value=0))
)
