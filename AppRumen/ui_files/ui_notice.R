div( 
  h3("Messages"),
  tabsetPanel(id="Message_tab", title="Message", 
              tabPanel("Message Box",
                       DT::dataTableOutput("table_notice_message")),
              tabPanel("Content",
                       uiOutput("message_content"),
                       h5("Reply:"),
                       uiOutput('resettable_reply_content'),
                       actionButton("send_reply","Send","primary")
              ),
              # shinyjs::hidden(div(id="create_new_message",
              tabPanel("New",
                       textInput("msg_title","Title:",value=NULL),
                       shinyjs::disabled(textInput("msg_user","To:",value=NULL)), 
                       uiOutput('resettable_msg_content'),
                       actionButton("send_msg","Send","primary")
              # ))
              ),
              tabPanel("Sent",
                       DT::dataTableOutput("table_notice_sent")
              )
  ),
  br(),
  h3("Comments received"),
  DT::dataTableOutput("table_notice_comment"),
  br(),
  h3("Progress in your posts"),
  DT::dataTableOutput("table_notice_progress"),
  br(),
  h3("Updates of posts you follow"),
  DT::dataTableOutput("table_notice_follow"),
  br(),
  h3("Posts you may be interested in following"),
  DT::dataTableOutput("table_recom_posts"),
  br(),
  h3("People you may be interested in following"),
  DT::dataTableOutput("table_recom_users"),
  br(),br()
)


