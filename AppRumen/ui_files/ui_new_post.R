# ------------- New Post --------------------
div(
  div(id = "post_form",
      shinyjs::disabled(textInput("post_name","App Name", value="")),
      shinyjs::disabled(selectInput("post_category","Category", 
                  choices=c("Milk","Forage","Labor","Social"))),
      h5(strong("Description")), 
      uiOutput("resetable_post"), 
      br(), br(),
      shinyjs::disabled(actionButton("post_send", "Send","primary")),
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
  shinyjs::hidden(numericInput("completeness","", value=5))
)
