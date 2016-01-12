# ------------- Details --------------------
# bsCollapsePanel(
#   "Details", style = "success",
div(
  div(id = "post_form",
      shinyjs::hidden(
        div(id = "loadMsg", wellPanel("Loading..."), align="center")
      ), 
      div(id="details_contents",
          uiOutput("selectedPost"),
          br(),
          div(id="show_comment_box",
              fluidRow(column(5,
                              actionButton("edit","Edit (author only)","primary")
              ), 
              
              column(5, 
                     a(id = "a_view_archive_comments","Show/hide comments in archive")
              )),
              shinyjs::hidden(
                div(id = "view_archive_comments",                  
                    br(),
                    numericInput("n_archive_comments","Number of archived comments",
                                 min=1, max=100, value=10, step=5),
                    uiOutput("selectedArchiveComments")
                    
                )
              ),
              br(), br(),
              h5(strong("Comment")), 
              uiOutput("resetable_comment"),
              selectInput("novelty","Novelty", 
                          choices=c("That's a new idea!"=1,
                                    "Tweak a similar App!"=2,
                                    "There's an App for that!"=3)),
              conditionalPanel("input.novelty>1",
                               textInput("app_link","Name of a similar App",value="NA")),
              sliderInput("interest","Interest",min=1,max=5,step=1,value=3),
              tags$head(tags$style(type="text/css", "#post {height: 100px}")),
              br(),
              actionButton("gmail2","Google Account", "primary"), br(),br(), 
              textInput("comment_user_name","User Name"),
              textInput("comment_email_address","Email Address"),
              
              actionButton("comment_send", "Send","primary")
          ),
          shinyjs::hidden(
            span(id = "submitMsg2", "Sending...", style = "margin-left: 15px;")
          )
      )
  ),
  shinyjs::hidden(
    div(id = "error2",
        div(br(), tags$b("Error: "), span(id = "errorMsg2")),
        style = "color: red;"
    )
  )
  
)
