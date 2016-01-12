# ------------- Posts --------------------
div(
  checkboxGroupInput("filterStatus", "Status", 
                     choices=c("Active","Completed","Resolved","Discontinued"),
                     selected = c("Active"), inline = TRUE), 
  checkboxGroupInput("filterCategory", "Category", choices=c("Milk","Forage","Labor","Social"),
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
)
