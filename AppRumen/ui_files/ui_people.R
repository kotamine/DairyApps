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
                                choices=c("Generating new ideas"=1,
                                          "Collaborating with others"=2,
                                          "Outreach/Education"=3, 
                                          "Learning more about dairy"=4,
                                          "Networking"=5,
                                          "Other"=6),
                                selected = c(1:5), 
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
             uiOutput("selectedUser")
           )
)