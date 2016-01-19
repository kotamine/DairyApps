bsCollapse(id = "collapsePeople", open = "People",
           bsCollapsePanel(
             "People", style = "info",
             checkboxGroupInput("filterProfessions", "Profession", 
                                choices=vars_profession, 
                                selected = vars_profession,  
                                inline=TRUE), 
             checkboxGroupInput("filterInterests", "Interests", 
                                choices=vars_interests,
                                selected =vars_interests, 
                                inline = TRUE), 
             fluidRow(column(5,numericInput("n_boxes_people","Number of People", value=10, min=0,step=5,max=100)),
                      column(7,selectInput("sortPeople","Sort by",
                                           choices=c("Most recently joined","Most posted",
                                                     "Most commented", "Most viewed",
                                                     "Most followed")))),
             fluidRow(
               uiOutput("peopleboxes")
             ) 
           ),
           bsCollapsePanel(
             "Details", style = "warning",
             uiOutput("selectedUser"),
             fluidRow(column(5, bsButton("message_user", "Send a message to this User", style='primary')),
                      div(id="follow_user_0",
                          shinyjs::disabled(bsButton("follow_user","Follow this User", style="primary"))),
                      shinyjs::hidden(div(id="follow_user_1",
                                          bsButton("unfollow_user","Undo: Follow",  style="primary"))))
           )
)  