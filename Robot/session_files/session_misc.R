
# c_empty_table <- df_null(c_colnames) 
# s_empty_table <- df_null(s_colnames) 
# p_empty_table <- df_null(p_colnames) 





# Show/hide DMI calculations 
shinyjs::onclick("customDMI",
                 shinyjs::toggle(id="DMI_inputs", anim = TRUE)
)

lapply(base_profiles, function(x) {
  
  # Show/hide Second set of Robot or Parlors
  observe({
    if (input[[paste0("n_sets",x)]]=="2") { 
      for (i in c(1:11)) shinyjs::show(paste0(x,2,i)) # Located in ui_data_entry_functions
      for (i in c(1:6))  shinyjs::show(paste0(x,"_CF",2,i)) # Located in ui_cash_flow
      for (i in c(1:1))  shinyjs::show(paste0(x,"_PB",2,i)) # Located in ui_partial_budget
    } else {
      for (i in c(1:11))  shinyjs::hide(paste0(x,2,i))
      for (i in c(1:6))   shinyjs::hide(paste0(x,"_CF",2,i))
      for (i in c(1:1))   shinyjs::hide(paste0(x,"_PB",2,i)) 
    }
  })
  
  # Show/hide delayed investment 
  observeEvent(input[[paste0("yr_system1",x)]], {
    if (input[[paste0("yr_system1",x)]]>0) {
      shinyjs::show(paste0(x,"delay",1))
    } else {
      shinyjs::hide(paste0(x,"delay",1))
    }
  })  
  
  # Show/hide Partial Budget Plots
  shinyjs::onclick(paste0("PB_plot_show_1",x),
                   shinyjs::toggle(id=paste0("id_PB_plot_show_1",x), anim = TRUE)
  )
  
  shinyjs::onclick(paste0("PB_plot_show_2",x),
                   shinyjs::toggle(id=paste0("id_PB_plot_show_2",x), anim = TRUE)
  )
  
  # Show/hide Cash Flow formula
  shinyjs::onclick(paste0("CF_formula_show_1",x),
                   shinyjs::toggle(id=paste0("id_CF_formula_show_1",x), anim = TRUE)
  )
  
  shinyjs::onclick(paste0("CF_formula_show_2",x),
                   shinyjs::toggle(id=paste0("id_CF_formula_show_2",x), anim = TRUE)
  )
})

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


# Make profile choice equivalent across various tabs
list_tabs <- c("dashboard","prMilk", "prLabor","prFinance","prMaintenance","prCapital",
               "partial_budget","cash_flow","sensitivity")

lapply(list_tabs, function(z) {
  observeEvent(input[[paste(z)]], {
    # browser()
    # if (any(sapply(base_profiles, function(x) length(ans[[x]]$net_annual_impact_before_tax)==0))) return()
    lapply(list_tabs, function(w) {
      if (z!=w)  updateTabsetPanel(session, paste(w), input[[paste(z)]])
      # shinyjs::disable(paste0('radio_',z))

    })
  })
})


# Make IOFC and NAI choices equivalent across various profiles 
# list_profiles <- c(base_profiles,base_profiles_se)

# my_update_dashboard <- function(var, choices) {
#   lapply(list_profiles, function(z) {
#     observeEvent(input[[paste0(var,z)]], {
#       shinyjs::disable(paste0('radio_',var,z))
#       lapply(list_profiles, function(w) {
#         updateRadioButtons(session, paste(var),NULL,
#                            choices=choices, selected=input[[paste0(var,z)]])
#         if (z!=w)  {
#           updateRadioButtons(session, paste0(var,w),NULL,
#                              choices=choices, selected=input[[paste0(var,z)]])
#         }
#         
#       })
#     })
#   })
# }

list_profiles <- list(base_profiles,base_profiles_se)

my_update_dashboard <- function(var, choices) {
  for (list in list_profiles) {
    ifelse(all(list==base_profiles), disable<-TRUE, disable<-FALSE)
  lapply(list, function(z) {
    observeEvent(input[[paste0(var,z)]], {
      if (disable) shinyjs::disable(paste0('radio_',var,z))
      lapply(list, function(w) {
        updateRadioButtons(session, paste(var),NULL,
                           choices=choices, selected=input[[paste0(var,z)]])
        if (z!=w)  {
          updateRadioButtons(session, paste0(var,w),NULL,
                             choices=choices, selected=input[[paste0(var,z)]])
        }
        
      })
    })
  })
  }
}

my_update_dashboard("IOFC", c("per cow","per cwt"))
my_update_dashboard("NAI", c("before tax", "after tax"))


## download cashflow, debt, depreciation tables
lapply(base_profiles, function(x) {
  output[[paste0("dl_button_cash_flow",x)]] <- renderUI({
    need(!is.null(ans[[x]]$table_cash_flow) & !is.null(ans[[x]]$table_debt) &
           !is.null(ans[[x]]$table_depreciation)) %>% validate()
    downloadButton('download_table_cash_flow', 'Download')
  })
  
  
})



# ----------- Download and Upload of User Data -----------------
# Download the tables as an Excel file   
output[["data_download"]] <- downloadHandler(
  filename = "user_input_data.xlsx",  
  
  content = function(file) { 
    wb <- XLConnect::loadWorkbook(file, create = TRUE)
    XLConnect::createSheet(wb, name = "common_variables")
    XLConnect::createSheet(wb, name = "profile_specific_variables")
    XLConnect::writeWorksheet(wb, get_common_variables(), sheet = "common_variables") 
    XLConnect::writeWorksheet(wb, get_profile_specific_variables(), sheet = "profile_specific_variables") 
    XLConnect::saveWorkbook(wb)
  } 
)



get_common_variables <- function() {
  df <- df_null(c("variable","label","value")) 
  common_vars <- c(list_inputs_shared,list_inputs_feed)
  label_vars <- c(label_inputs_shared,label_inputs_feed)
  
  for(i in seq_along(common_vars)) {
    df[i,] <-  c(common_vars[i],label_vars[i],
                 input[[paste(common_vars[i])]])
  } 
  return(df)
}


get_profile_specific_variables <- function() {
  df <- lapply(base_profiles, refProfileName) %>% unlist %>% df_null()
  
  for(i in seq_along(list_inputs_profile)) {
    df[i,] <- lapply(base_profiles, function(x) input[[paste0(list_inputs_profile[i],x)]]) %>% unlist()
  }
  df <- cbind(variable=list_inputs_profile, label=label_inputs_profile,df)
  
  return(df)
}


# Upload an Excel file, validate it, and then update tables
observe({
  if (is.null(input$data_upload)) { return() }
  
  inFile <- input$data_upload
  if (is.null(inFile)) {
    return(NULL) 
  }
  
  isolate({
    
    closeAlert(session, "ref_upload_alert")
    
    # Check file type: This is automatically handled when launched to the web
    if (strsplit(inFile$name, "\\.")[[1]][2] !="xlsx") {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Not a Excel workbook: 
                  please upload a proper file.", 
                  append = TRUE)
      return()
    }
    
    wb <- XLConnect::loadWorkbook(inFile$datapath) 
    sheets <- XLConnect::getSheets(wb)
    
    # Check the number of sheets
    if (length(sheets)!=2) {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Wrong number of sheets: 
                  please upload a proper file.",
                  append = TRUE)
      return()
    }
    
    user_data$common_variables <- myRead.xlsx(inFile$datapath, sheetIndex = 1) 
    user_data$profile_specific_variables <- myRead.xlsx(inFile$datapath, sheetIndex = 2) 
  })
  
})

observe({
  input$case1
  input$default_data
  
  isolate({
    # ** Make this depend on the case the user selects
    user_data$common_variables <- default_common_case_1
    user_data$profile_specific_variables  <- default_profile_specific_case_1 
  })
})


# Change to default data or uploaded data triggers updates of inputs 
observe({ 
  need(!is.null(user_data$common_variables) &
         !is.null(user_data$profile_specific_variables), "NA") %>% validate() 
  
  isolate({
    # Replace "." symbole with space " "
    colnames(user_data$common_variables) <- gsub("\\."," ",colnames(user_data$common_variables))
    colnames(user_data$profile_specific_variables) <- gsub("\\."," ",colnames(user_data$profile_specific_variables))
    
    colnames_1 <- c("variable", "label", "value")
    colnames_2 <- c("variable", "label", unlist(lapply(base_profiles, refProfileName))) 
    
    # Check colnames
    if (!(all(colnames(user_data$common_variables) %in% colnames_1) &
          all(colnames_1 %in% colnames(user_data$common_variables)) &
          all(colnames(user_data$profile_specific_variables) %in% colnames_2) &
          all(colnames_2 %in% colnames(user_data$profile_specific_variables)))) {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Wrong column names: 
                  please upload  a proper file.",
                  append = TRUE)
      return()
    }
    
    rownames_1 <- c(list_inputs_shared, list_inputs_feed)
    rownames_2 <- c(list_inputs_profile)
    
    # Check row names of the first column
    if(!(all(user_data$common_variables[,1] %in% rownames_1) & 
         all(user_data$profile_specific_variables[,1] %in% rownames_2) )) {
      createAlert(session, "upload_alert", "ref_upload_alert", 
                  content = "Wrong row names in the first column: 
                  please upload a proper file.",
                  append = TRUE)
      
      return()
    } 
    
    closeAlert(session, "ref_upload_alert")
    
    #  Update data with uploaded data 
    updateRadioButtons(session, "dep_method", "Depreciation accounting method:", 
                       selected=user_data$common_variables["dep_method","value"], 
                       choices=c("Accelerated GDS"="d1","Straight-line ADS"="d2"))
    
    # Update data with uploaded data 
    for(i in seq_along(rownames_1)) {
      loc_var <- input[[rownames_1[i]]]
      if (!is.null( loc_var)) 
      {  if (is.numeric( loc_var)) {
        updateNumericInput(session, rownames_1[i], NULL,
                           value=user_data$common_variables[rownames_1[i],"value"] %>% as.numeric(), 
                           min= common_variables_min_step[i,"min"] %>% as.numeric(),
                           step=common_variables_min_step[i,"step"] %>% as.numeric(),
                           max= common_variables_min_step[i,"max"] %>% as.numeric())
      } 
      }
    } 
    
    lapply(base_profiles, function(x) {
      x_col <- which(refProfileName(x)==colnames(user_data$profile_specific_variables))
      
      for(i in seq_along(rownames_2)) {
        loc_var <- input[[paste0(rownames_2[i],x)]]
        if (!is.null( loc_var))
        {  if (is.numeric( loc_var)) {
          
          updateNumericInput(session, paste0(rownames_2[i],x), NULL,
                             value=user_data$profile_specific_variables[rownames_2[i],x_col] %>% as.numeric(),
                             min=profile_specific_variables_min_step[i,"min"] %>% as.numeric(),
                             step=profile_specific_variables_min_step[i,"step"] %>% as.numeric(),
                             max=profile_specific_variables_min_step[i,"max"] %>% as.numeric())
        } else if (rownames_2[i]=="n_sets") {
          updateRadioButtons(session, paste0("n_sets",x), NULL, choices=c("one"=1, "two"=2),
                             selected=as.character(user_data$profile_specific_variables["n_sets", x_col]), inline=TRUE) 
        }
        } 
      }
    }) 
    
  })
})


