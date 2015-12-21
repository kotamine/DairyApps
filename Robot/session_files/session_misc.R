
c_empty_table <- df_null(c_colnames) 
s_empty_table <- df_null(s_colnames) 
p_empty_table <- df_null(p_colnames) 


# Show/hide DMI calculations 
shinyjs::onclick("customDMI",
                 shinyjs::toggle(id="DMI_inputs", anim = TRUE)
)

# Show/hide Partial Budget Plots
shinyjs::onclick("PB_plot_show_1",
                 shinyjs::toggle(id="id_PB_plot_show_1", anim = TRUE)
)

shinyjs::onclick("PB_plot_show_2",
                 shinyjs::toggle(id="id_PB_plot_show_2", anim = TRUE)
)

# Show/hide Cash Flow formula
shinyjs::onclick("CF_formula_show_1",
                 shinyjs::toggle(id="id_CF_formula_show_1", anim = TRUE)
)

shinyjs::onclick("CF_formula_show_2",
                 shinyjs::toggle(id="id_CF_formula_show_2", anim = TRUE)
)


observeEvent(input$coeff_reset,{ 
  updateNumericInput(session, "milk_cow_coeff",NULL,value=0.4,min=0,step=0.1)
  updateNumericInput(session, "milk_fat",NULL,value=3.65,min=0,step=0.2)
  updateNumericInput(session, "milk_fat_coeff",NULL,value=15,min=0,step=0.5)
  updateNumericInput(session, "adj_milk_cow_coeff",NULL,value=0.372,min=0,step=0.1)
  updateNumericInput(session, "body_weight_coeff1",NULL,value=0.0968,min=0,step=0.005)
  updateNumericInput(session, "body_weight_coeff2",NULL,value=0.75,min=0,step=0.05)
  updateNumericInput(session, "lactation_coeff1",NULL,value=-0.192,step=0.01)
  updateNumericInput(session, "lactation_coeff2",NULL,value=3.67,min=0,step=0.05)
}) 


observeEvent(input$dashboard,{
       updateTabsetPanel(session,"prMilk",input$dashboard)
       updateTabsetPanel(session,"prLabor",input$dashboard)
       updateTabsetPanel(session,"prFinance",input$dashboard)
       updateTabsetPanel(session,"prMaintenance",input$dashboard)
       updateTabsetPanel(session,"prCapital",input$dashboard)
     })
  
     observeEvent(input$prCapital,{
     updateTabsetPanel(session,"prLabor",input$prCapital)
     updateTabsetPanel(session,"prMilk",input$prCapital)
     updateTabsetPanel(session,"prFinance",input$prCapital)
     updateTabsetPanel(session,"prMaintenance",input$prCapital)
     updateTabsetPanel(session,"dashboard",input$prCapital)
   })

      observeEvent(input$prLabor,{
     updateTabsetPanel(session,"prCapital",input$prLabor)
     updateTabsetPanel(session,"prMilk",input$prlabor)
     updateTabsetPanel(session,"prFinance",input$prlabor)
     updateTabsetPanel(session,"prMaintenance",input$prlabor)
     updateTabsetPanel(session,"dashboard",input$prLabor)
   })

    observeEvent(input$prMilk,{
     updateTabsetPanel(session,"prCapital",input$prMilk)
     updateTabsetPanel(session,"prLabor",input$prMilk)
     updateTabsetPanel(session,"prFinance",input$prMilk)
     updateTabsetPanel(session,"prMaintenance",input$prMilk)
     updateTabsetPanel(session,"dashboard",input$prMilk)
   })
   
   observeEvent(input$prFinance,{
     updateTabsetPanel(session,"prCapital",input$prFinance)
     updateTabsetPanel(session,"prLabor",input$prFinance)
     updateTabsetPanel(session,"prMilk",input$prFinance)
     updateTabsetPanel(session,"prMaintenance",input$prFinance)
     updateTabsetPanel(session,"dashboard",input$prFinance)
   })
   
   observeEvent(input$prMaintenance,{
     updateTabsetPanel(session,"prCapital",input$prMaintenance)
     updateTabsetPanel(session,"prLabor",input$prMaintenance)
     updateTabsetPanel(session,"prFinance",input$prMaintenance)
     updateTabsetPanel(session,"prMilk",input$prMaintenance)
     updateTabsetPanel(session,"dashboard",input$prMaintenance)
   })

# The following provides the default value for additional_labor and additional_cost when hidden from the user
# updateNumericInput(session, "additional_labor",NULL,value=450,step=50,min=0)
# updateNumericInput(session, "additional_cost",NULL,value=200,step=50,min=0)
# 
# # Cash flow related variables; initially hidden from the user 
# updateNumericInput(session, "inflation_robot",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "inflation_margin",NULL,value=0.2,step=.25,min=0)
# updateNumericInput(session, "inflation_labor",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "inflation_general",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "inflation_general",NULL,value=1.5,step=.25,min=0)
# updateNumericInput(session, "down_housing",NULL,value=100000, min=0,step=20000)
# updateNumericInput(session, "down_robot1",NULL,value=0, min=0,step=20000)
# updateNumericInput(session, "down_robot2",NULL,value=40000, min=0,step=20000)
# updateNumericInput(session, "r_housing",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "r_robot1",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "r_robot2",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "n_yr_housing",NULL,value=24, min=0, step=1)
# updateNumericInput(session, "n_yr_robot1",NULL,value=12, min=0, step=1)
# updateNumericInput(session, "n_yr_robot2",NULL,value=12, min=0, step=1)
# updateNumericInput(session, "salvage_housing",NULL,value=0, min=0, step=5000)
# updateNumericInput(session, "horizon",NULL,value=30, min=1, step=5)
# updateNumericInput(session, "hurdle_rate",NULL,value=4, min=0, step=.25)
# updateNumericInput(session, "tax_rate",NULL,value=40, min=0, step=2)




# # Show/hide Robots vs Parlors summary tables 
# shinyjs::onclick("tableProfile",
#                  shinyjs::toggle(id="tableProfileSummary", anim = TRUE)
# )
# 
# # Show/hide Robots vs Parlors profile explanations
# shinyjs::onclick("readProfile",
#                  shinyjs::toggle(id="ref_readProfile", anim = TRUE)
# )
# 

# ## download cashflow, debt, depreciation tables
# output$dl_button_cash_flow <- renderUI({
#   if (!is.null(df$table_cash_flow) & !is.null(df$table_debt) & !is.null(df$table_depreciation))
#   {   downloadButton('download_table_cash_flow', 'Download')
#   } else {
#     NULL
#   } 
# })


# 
# # Download the tables as an Excel file   
# output$data_download <- downloadHandler(
#   filename = "user_input_data.xlsx",  
#   
#   content = function(file) { 
#     wb <- XLConnect::loadWorkbook(file, create = TRUE)
#     XLConnect::createSheet(wb, name = "selected_profile")
#     XLConnect::createSheet(wb, name = "all_profiles")
#     XLConnect::writeWorksheet(wb, selected_profile(), sheet = "selected_profile") 
#     XLConnect::writeWorksheet(wb, all_profiles(), sheet = "all_profiles") 
#     XLConnect::saveWorkbook(wb)
#   } 
# )  
# 
# 
# selected_profile <- function() {
#   df <- df_null(c("variable","value")) 
#   
#   for(i in 1:length(vars_selected_profile)) {
#     df[i,] <-  c(paste(vars_selected_profile[i]),
#                  input[[paste(vars_selected_profile[i])]])
#   } 
#   return(df)
# }
# 
# 
# all_profiles <- function() {
#   df <- df_null(c("variable","value")) 
#   
#   ii <- 1
#   for (varname_ref in c( "_pr1", "_pr2", "_pr3", "_pr4"))  {
#     for(i in 1:length(vars_all_profiles)) {
#       if (!is.null(input[[paste0(vars_all_profiles[i], varname_ref)]])) 
#       {
#         df[ii,] <-  c(paste0(vars_all_profiles[i],varname_ref),
#                       input[[paste0(vars_all_profiles[i], varname_ref)]])
#         ii <- ii + 1
#       }
#     } 
#   }
#   return(df)
# }
# 
# 

# # Upload an Excel file, validate it, and then update tables
# observe({
#   if (is.null(input$data_upload)) { return() }
#   
#   inFile <- input$data_upload
#   if (is.null(inFile)) {
#     return(NULL) 
#   }
#   
#   isolate({
#     
#     closeAlert(session, "ref_upload_alert")
#     
#     # Check file type: This is automatically handled when launched to the web
#     if (strsplit(inFile$name, "\\.")[[1]][2] !="xlsx") {
#       createAlert(session, "upload_alert", "ref_upload_alert", 
#                   content = "Not a Excel workbook: 
#                   please upload a proper file.", 
#                   append = TRUE)
#       return()
#     }
#     
#     wb <- XLConnect::loadWorkbook(inFile$datapath) 
#     sheets <- XLConnect::getSheets(wb)
#     
#     # Check the number of sheets
#     if (length(sheets)!=2) {
#       createAlert(session, "upload_alert", "ref_upload_alert", 
#                   content = "Wrong number of sheets: 
#                   please upload a proper file.",
#                   append = TRUE)
#       return()
#     }
#     
#     rv$user_data_1 <- read.xlsx(inFile$datapath, sheetIndex = 1) 
#     rv$user_data_2 <- read.xlsx(inFile$datapath, sheetIndex = 2) 
#   })
#   
# })
# 
# observeEvent(input$defalut_data,{
#   
#   rv$user_data_1 <- default_data_1
#   rv$user_data_2 <- default_data_2
#   
# })
# 
# 
# # change to default data or uploaded data triggers updates of inputs 
# observe({ 
#   if (is.null(rv$user_data_1)) { return() }
#   rv$user_data_1 
#   rv$user_data_2 
#   
#   isolate({
#     # Replace "." symbole with space " "
#     colnames(rv$user_data_1) <- gsub("\\."," ",colnames(rv$user_data_1))
#     colnames(rv$user_data_2) <- gsub("\\."," ",colnames(rv$user_data_2))
#     
#     colnames_1 <- c("variable","value")
#     colnames_2 <- c("variable","value")
#     
#     # Check colname names
#     if (!(all(colnames(rv$user_data_1) %in% colnames_1) &
#           all(colnames_1 %in% colnames(rv$user_data_1)) &
#           all(colnames(rv$user_data_2) %in% colnames_2) &
#           all(colnames_2 %in% colnames(rv$user_data_2)))) {
#       createAlert(session, "upload_alert", "ref_upload_alert", 
#                   content = "Wrong column names: 
#                   please upload  a proper file.",
#                   append = TRUE)
#       return()
#     }
#     
#     rownames_1 <- vars_selected_profile
#     rownames_2 <- c()
#     ii <- 1
#     for (varname_ref in c( "_pr1", "_pr2", "_pr3", "_pr4"))  {
#       for(i in 1:length(vars_all_profiles)) {
#         if (!is.null(input[[paste0(vars_all_profiles[i], varname_ref)]])) {
#           rownames_2[ii] <-  c(paste0(vars_all_profiles[i], varname_ref))
#           ii <- ii + 1
#         }
#       } 
#     }
#     
#     # Check row names of the first column
#     if(!(all(rv$user_data_1[,1] %in% rownames_1) & 
#          all(rv$user_data_2[,1] %in% rownames_2) )) {
#       createAlert(session, "upload_alert", "ref_upload_alert", 
#                   content = "Wrong row names in the first column: 
#                   please upload a proper file.",
#                   append = TRUE)
#       
#       return()
#     } 
#     
#     closeAlert(session, "ref_upload_alert")
#     
#     # Some processing is needed to treat numeric and non-numeric variables separately
#     idx_1 <-  c(1:length(rv$user_data_1[,"variable"]))[rv$user_data_1[,"variable"] 
#                                                        %in% c("robot_parlor","profile_choice", "dep_method")]
#     # "robot_parlor","profile_choice", "dep_method" are assumed to be ordered in that way
#     
#     data_1 <- matrix(rv$user_data_1[ -idx_1,"value"],nrow=1) %>% data.frame()
#     colnames(data_1) <-   rv$user_data_1[ -idx_1,"variable"]
#     
#     
#     # Update data with uploaded data 
#     updateRadioButtons(session, "robot_parlor","Robots vs Parlors Comparison",
#                        selected=rv$user_data_1[idx_1[1],"value"], choices=c("OFF","ON"), inline=TRUE)  
#     
#     updateSelectInput(session,"profile_choice","Select Investment Profile", 
#                       selected=rv$user_data_1[idx_1[2],"value"], 
#                       choices=c("Barn Only","Retrofit Parlors","New Parlors","Robots"))
#     
#     updateRadioButtons(session, "dep_method", "Depreciation accounting method:", 
#                        selected=rv$user_data_1[idx_1[3],"value"], 
#                        choices=c("Accelerated GDS"="d1","Straight-line ADS"="d2"))
#     
#     updateNumericInput(session, "herd_size",NULL,value=data_1$herd_size, min=30,step=10)
#     updateNumericInput(session, "herd_increase",NULL,value=data_1$herd_increase, min=0,step=10)
#     updateNumericInput(session, "additional_labor",NULL,value=data_1$additional_labor, min=0,step=50)
#     updateNumericInput(session, "additional_cost",NULL,value=data_1$additional_cost, min=0,step=50)
#     updateNumericInput(session, "n_robot",NULL,value=data_1$n_robot, min=0,step=1)
#     updateNumericInput(session, "cost_robot",NULL,value=data_1$cost_robot, min=50000,step=10000)
#     updateNumericInput(session, "cost_parlors",NULL,value=data_1$cost_parlors, min=0,step=10000)
#     updateNumericInput(session, "cost_housing_cow",NULL,value=data_1$cost_housing_cow, min=0,step=500) 
#     updateNumericInput(session, "repair",NULL,value=data_1$herd_size, min=0,step=500)
#     updateNumericInput(session, "robot_years",NULL,value=data_1$robot_years, min=0,step=1)
#     updateNumericInput(session, "n_robot_life",NULL,value=data_1$n_robot_life, min=1,step=2)
#     updateNumericInput(session, "milking_years",NULL,value=data_1$milking_years, min=0, step=1)
#     updateNumericInput(session, "salvage_milking1",NULL,value=data_1$salvage_milking1, min=0,step=1000)
#     updateNumericInput(session, "insurance_rate",NULL,value=data_1$insurance_rate, min=0,step=0.1)
#     updateNumericInput(session, "hours_milking",NULL,value=data_1$herd_size, min=0,step=1)
#     updateNumericInput(session, "hr_sv_milking",NULL,value=data_1$hr_sv_milking,  min=0, step=.2)
#     updateNumericInput(session, "hr_heat_detection",NULL,value=data_1$hr_heat_detection, min=0,step=0.5)
#     updateNumericInput(session, "anticipated_hours_heat",NULL,value=data_1$anticipated_hours_heat, min=0,step=0.05)
#     updateNumericInput(session, "labor_rate",NULL,value=data_1$labor_rate, min=0,step=0.25)
#     updateNumericInput(session, "increase_rc_mgt",NULL,value=data_1$increase_rc_mgt, min=0,step=0.1)
#     updateNumericInput(session, "decrease_lab_mgt",NULL,value=data_1$decrease_lab_mgt, min=0,step=0.1)
#     updateNumericInput(session, "labor_rate_rc_mgt",NULL,value=data_1$labor_rate_rc_mgt, min=0,step=0.25)
#     updateNumericInput(session, "price_milk",NULL,value=data_1$price_milk, min=0,step=0.25)
#     updateNumericInput(session, "milk_cow_day",NULL,value=data_1$milk_cow_day, min=0,step=5)
#     updateNumericInput(session, "scc_premium",NULL,value=data_1$scc_premium, min=0,step=0.001)
#     updateNumericInput(session, "scc_average",NULL,value=data_1$scc_average, min=0,step=10000)
#     updateNumericInput(session, "scc_change",NULL,value=data_1$scc_change, min=0,step=0.25)
#     updateNumericInput(session, "software",NULL,value=data_1$software, min=0,step=1)
#     updateNumericInput(session, "cost_DM",NULL,value=data_1$cost_DM, min=0,step=0.005)
#     updateNumericInput(session, "pellets",NULL,value=data_1$pellets, min=0,step=1)
#     updateNumericInput(session, "cost_pellets",NULL,value=data_1$cost_pellets, min=0,step=2)
#     updateNumericInput(session, "milk_cow_coeff",NULL,value=data_1$milk_cow_coeff, min=0,step=0.1)
#     updateNumericInput(session, "milk_fat",NULL,value=data_1$milk_fat, min=0,step=.2)
#     updateNumericInput(session, "milk_fat_coeff",NULL,value=data_1$milk_fat_coeff, min=0,step=.5)
#     updateNumericInput(session, "adj_milk_cow_coeff",NULL,value=data_1$adj_milk_cow_coeff, min=0,step=0.1)
#     updateNumericInput(session, "body_weight",NULL,value=data_1$body_weight,min=1000,step=50)
#     updateNumericInput(session, "body_weight_coeff1",NULL,value=data_1$body_weight_coeff1,min=0,step=0.005)
#     updateNumericInput(session, "body_weight_coeff2",NULL,value=data_1$body_weight_coeff2, min=0,step=0.05)
#     updateNumericInput(session, "lcatation_week",NULL,value=data_1$lcatation_week, min=0,step=1)
#     updateNumericInput(session, "lactation_coeff1",NULL,value=data_1$lactation_coeff1, min=0,step=0.01)
#     updateNumericInput(session, "lactation_coeff2",NULL,value=data_1$lactation_coeff2, min=0,step=0.05)
#     updateNumericInput(session, "culling_rate",NULL,value=data_1$culling_rate,min=0,step=0.1)
#     updateNumericInput(session, "death_rate",NULL,value=data_1$death_rate, min=0,step=0.1)
#     updateNumericInput(session, "cost_heifer",NULL,value=data_1$cost_heifer, min=0,step=100)
#     updateNumericInput(session, "cull_price",NULL,value=data_1$cull_price, min=0,step=50)
#     updateNumericInput(session, "change_turnover",NULL,value=data_1$change_turnover, min=0,step=.25)
#     updateNumericInput(session, "change_electricity",NULL,value=data_1$change_electricity, min=0,step=.25)
#     updateNumericInput(session, "change_water",NULL,value=data_1$change_water, min=0,step=.25)
#     updateNumericInput(session, "change_chemical",NULL,value=data_1$change_chemical, min=0,step=.25)
#     updateNumericInput(session, "inflation_robot",NULL,value=data_1$inflation_robot,step=.25)
#     updateNumericInput(session, "inflation_margin",NULL,value=data_1$inflation_margin, step=.25)
#     updateNumericInput(session, "inflation_labor",NULL,value=data_1$inflation_labor, step=.25)
#     updateNumericInput(session, "interest",NULL,value=data_1$interest, min=0, step=.1)
#     updateNumericInput(session, "hurdle_rate",NULL,value=data_1$hurdle_rate, min=0, step=.1)
#     updateNumericInput(session, "tax_rate",NULL,value=data_1$tax_raet, min=0, step=2)
#     updateNumericInput(session, "down_housing",NULL,value=data_1$down_housing, min=0, step=20000)
#     updateNumericInput(session, "down_milking1",NULL,value=data_1$down_milking1, min=0, step=20000)
#     updateNumericInput(session, "down_milking2",NULL,value=data_1$down_milking2, min=0, step=20000)
#     updateNumericInput(session, "r_housing",NULL,value=data_1$r_housing, min=0, step=.25)
#     updateNumericInput(session, "r_milking1",NULL,value=data_1$r_milking1, min=0, step=.25)
#     updateNumericInput(session, "r_milking2",NULL,value=data_1$r_milking2, min=0, step=.25)
#     updateNumericInput(session, "n_yr_housing",NULL,value=data_1$n_yr_housing, min=0, step=1)
#     updateNumericInput(session, "n_yr_milking1",NULL,value=data_1$n_yr_milking1, min=0, step=1)
#     updateNumericInput(session, "n_yr_milking2",NULL,value=data_1$n_yr_milking2, min=0, step=1)
#     
#     
#     
#     for (varname_ref in c( "_pr1", "_pr2", "_pr3", "_pr4"))  {
#       for(i in 1:length(vars_all_profiles)) {
#         if (!is.null(input[[paste(vars_all_profiles[i], varname_ref)]])) 
#           updateNumericInput(session, paste0(vars_all_profiles[i], varname_ref), NULL,
#                              value=rv$user_data_2[[paste0(vars_all_profiles[i], varname_ref)]], 
#                              min= mins_vars_all_profiles[i],step=steps_vars_all_profiles[i] )
#       } 
#     }
#   })
# })
# 

