div( #id="collapseNotice",
                # bsCollapsePanel(
                  # "Message", style = "info",
                  tabsetPanel(id="Message", title="Message", 
                    tabPanel("Table",
                            uiOutput("table_notice_message")),
                    tabPanel("Content",
                            uiOutput("message_conentent"),
                            textInput("message_to","Message to:",value=NULL),
                            uiOutput("resetable_message"),
                            bsButton("send_message","Send")
                    )
                  ),
                  hr(),
                # ),
                # bsCollapsePanel(
                 # "Updates", style = "info",
                # )
                h4("Comments received"),
                uiOutput("table_notice_comment"),
                hr(),
                h4("Updates in following posts"),
                uiOutput("table_notice_follow"),
                hr(),
                h4("Progress in your posts"),
                uiOutput("table_notice_progress")
)


