bsCollapse(id = "collapseMain", 
           multiple = FALSE,
           open = "Posts", 
           bsCollapsePanel("Posts", style = "info",
                           checkboxGroupInput("filterStatus", "Status", 
                                              choices=c("Active","Completed","Resolved","Discontinued"),
                                              selected = c("Active"), inline = TRUE), 
                           checkboxGroupInput("filterCategory", "Category", choices=vars_category,
                                              selected = c("Milk","Forage","Labor","Social"), inline = TRUE),
                           fluidRow(column(5,numericInput("n_boxes","Number of Posts", value=10, min=0,step=5,max=100)),
                                    column(7,selectInput("sortPost","Sort by",
                                                         choices=c("Most recently posted","Most recently commented",
                                                                   "Most commented", "Most viewed",
                                                                   "Highest interests")))
                           ),
                           fluidRow(
                             uiOutput("postboxes")
                           )
           ),
           bsCollapsePanel("Details", style = "success",
                           div(id = "post_form",
                               shinyjs::hidden(
                                 div(id = "loadMsg", wellPanel("Loading..."), align="center")
                               ), 
                               div(id="details_contents",
                                   uiOutput("selectedPost"),
                                   br(),
                                   div(id="show_comment_box",
                                       fluidRow(column(5, div(id="like_0",
                                                              shinyjs::disabled(bsButton("like","Like", style="primary"))),
                                                       shinyjs::hidden(div(id="like_1",
                                                                           bsButton("unlike","Undo: Like",  style="primary")))),
                                                column(5,div(id="follow_0", 
                                                             shinyjs::disabled(bsButton("follow","Follow", style="primary"))),
                                                       shinyjs::hidden(div(id="follow_1",
                                                                           bsButton("unfollow","Undo: Follow",  style="primary"))))),
                                       br(), 
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
                                       # sliderInput("interest","Interest",min=1,max=5,step=1,value=3),
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
)