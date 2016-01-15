bsCollapse(id = "collapsePeople", open = "People",
           bsCollapsePanel(
             "People", style = "info",
             checkboxGroupInput("filterProfessions", "Profession", 
                                choices=c("Student"=1,
                                          "Producer"=2,
                                          "Industry"=3, 
                                          "Extension"=4,
                                          "Other"=5), 
                                selected = c(1:5),  
                                inline=TRUE), 
             checkboxGroupInput("filterInterests", "Interests", 
                                choices=c("Generating ideas"=1,
                                          "Collaborating"=2,
                                          "Dairy Productivity"=3,
                                          "Producer Outreach"=4, 
                                          "Youth Education"=5,
                                          "Public Outreach"=6,
                                          "Networking"=7,
                                          "Other"=8),
                                selected = c(1:8), 
                                inline = TRUE), 
             fluidRow(column(5,numericInput("n_boxes_people","Number of People", value=10, min=0,step=5,max=100)),
                      column(7,selectInput("sortPeople","Sort by",
                                           choices=c("Most recently posted","Most recently commented",
                                                     "Most commented", "Most viewed",
                                                     "Highest interests")))),
             fluidRow(
               uiOutput("peopleboxes")
             ) 
           ),
           bsCollapsePanel(
             "Details", style = "warning",
             uiOutput("selectedUser"),
             bsButton("filter_user", "Filter Posts by this User", style='primary')
           )
) 