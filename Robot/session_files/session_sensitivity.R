






# 
# sensitivity_labels <- c("Change in Milk Output","Milking/Chore Labor Wage", "Inflation: Robot/Parlor",
#                         "Inflation: IOFC Margin","Inflation: Labor Wage")
sensitivity_labels <- c("Change in Milk Output","Milking/Chore Labor Wage", "Cost: housing per cow",
                        "Cost: Robot/Parlor", "Lifespan: Robot/Parlor",
                        "Interest: housing","Interest: Robot/Parlor", "Inflation: Robot/Parlor",
                        "Inflation: IOFC Margin","Inflation: Labor Wage")
# sensitivity_format <- c("formatcomma", "formatdollar_2d", "round_pct", "round_pct", "round_pct")
# sensitivity_unit <- c("lb", "", "", "", "")
sensitivity_format <- c("formatcomma", "formatdollar_2d","formatdollar","formatdollar","formatcomma",
                        "round_pct", "round_pct","round_pct", "round_pct", "round_pct")
sensitivity_unit <- c("lb", "", "", "", "years", "", "", "", "", "")
sensitivity_slider_ini <- c(-40, 40, 40, 40, -40, 40, 40, 40, -40, 40) 

lapply(base_profiles, function(x) {  
  
#   sensitivity_vars <- c(paste0("milk_change",x),"labor_rate", "inflation_robot",
#                         "inflation_margin","inflation_labor")
    sensitivity_vars <- c(paste0("milk_change",x),"labor_rate", paste0("cost_housing_cow",x), 
                        paste0("cost_robot",x), paste0("useful_years",x),
                        paste0("r_housing",x), paste0("r_milking1",x), "inflation_robot",
                        "inflation_margin","inflation_labor") 
    if (x!="Robots")  sensitivity_vars[4] <- paste0("cost_parlors",x)
   

  lapply(seq_along(sensitivity_vars), function(i) {
    
    # Create sensitivity variable info and control 
    output[[paste0("sensitivity_vars",x,i)]] <- renderUI({
      
      var <- input[[sensitivity_vars[i]]]
      input[[paste0("sensitivity_slider",x,i)]]

      val0 <- do.call(sensitivity_format[i], list(var)) %>% paste(sensitivity_unit[i])
      val1 <- do.call(sensitivity_format[i], list(var * (1 + input[[paste0("sensitivity_slider",x,i)]]/100))) %>% 
        paste(sensitivity_unit[i])
      ans[[paste0(x,"_se",i)]]$val0 <- val0
      ans[[paste0(x,"_se",i)]]$val1 <- val1
      
      if (is.null(input[[paste0("sensitivity_slider",x,i)]])) {
        value <- sensitivity_slider_ini[i]
      } else {
        value <- input[[paste0("sensitivity_slider",x,i)]]
      }
      
      div(
        div(style="background-color:white; color:#4863A0;",
            h5(sensitivity_labels[i], align="center")), 
        helpText("Base:", val0, align="center"), 
        helpText("New :", val1, align="center"), 
        sliderInput(paste0("sensitivity_slider",x,i), "% Change",min=-200, max=200, step=20, value = value), 
        div(actionButton(paste0("sensitivity_action",x,i), "Details"), align="center") 
      )  
    }) 
    
    observeEvent(input[[paste0("sensitivity_slider",x,i)]], priority=1000, {
      shinyjs:: disable(paste0("sensitivity_slider",x,i))
    })
    

    # Calculate results for a given profile and sensitivity variable  
    ans[[paste0(x,"_se_calc",i)]] <- reactive({ 
      
      need(length(ans[[x]]$net_annual_impact_before_tax)>0 &&
             length(input[[paste0("sensitivity_slider",x,i)]])>0, "NA") %>% validate()
      input[[paste0("sensitivity_slider",i)]] 
      
      isolate({
        X <- paste0(x,"_se",i)
        factor <- input[[paste0("sensitivity_slider",x,i)]]/100     
        ans[[X]] <- list()
        ans[[X]]$milk_change  <- input[[paste0("milk_change",x)]] * (1 + (i==1) * factor)
        ans[[X]]$labor_rate  <- input$labor_rate * (1 + (i==2) * factor)
        ans[[X]]$inflation_robot  <- input$inflation_robot * (1 + (i==3) * factor)
        ans[[X]]$inflation_margin <- input$inflation_margin * (1 + (i==4) * factor)
        ans[[X]]$inflation_labor  <- input$inflation_labor * (1 + (i==5) * factor)
        
        source(file.path("session_files", "session_calculation_steps.R"), local=TRUE)  # Calculates main results
        
        on.exit(shinyjs:: enable(paste0("sensitivity_slider",x,i)))
      }) 
    }) 

    
  #  ------ Dashboard  -----------

    X <- paste0(x,"_se",i)
    source(file.path("session_files", "session_dashboard.R"), local=TRUE)  # Calculates dashboard items
      
    # Update the selected variable in the "Details" 
    observeEvent(input[[paste0("sensitivity_action",x,i)]], {
      shinyjs::show(id=paste0("sensitivity_details",x), anim=TRUE)
      ans[[paste0(x,"_se_details")]] <- i
    }) 
    
  })
  
  shinyjs::onclick(paste0("sensitivity_details_close",x),
                   shinyjs::hide(id=paste0("sensitivity_details",x), anim=TRUE)
  )
  
  # Show dashboard in "Details"
  output[[paste0("sensitivity_dashboard",x)]] <- renderUI({

    i <- ans[[paste0(x,"_se_details")]]
    X <- paste0(x,"_se",i)
    need(length(ans[[paste0(x,"_da_","after_tax")]]$NAI)>0,"Please first see Data Entry tab.") %>% validate()
    div( 
      div(style="background-color:white; color:#4863A0;",
     h4(paste0(sensitivity_labels[i],","),  "Change by", input[[paste0("sensitivity_slider",x,i)]],"%", 
        "(Base:",ans[[X]]$val0, "  to  New:",ans[[X]]$val1, ")", align="center")), 
     uiDashboard(X)
    ) 
    
  })
  
    # Create a Plot that shows at various percentage changes
    observeEvent(input[[paste0("sensitivity_plot_button",x)]], {
      
      shinyjs:: disable(paste0("sensitivity_plot_button",x))
      shinyjs:: show(paste0("sensitivity_plot_message",x))
      
      change_vars <- c(c(-5:-1),c(1:5))*2/10 * 100  #input[[paste0("sensitivity_range",x)]] 
      
      calc_type <- "short"
      table_sensitivity_plot_before_tax <- nulls(length(change_vars),length(sensitivity_vars))
      table_sensitivity_plot_after_tax <- nulls(length(change_vars),length(sensitivity_vars))
      
      lapply(seq_along(sensitivity_vars), function(i) {
          
        X <- paste0(x,"_se_all") # Serves as a temporary storage  
          
      for (n in seq_along(change_vars)) {
          
          factor <- change_vars[n]/100     
          ans[[X]] <- list()
          ans[[X]]$milk_change  <- input[[paste0("milk_change",x)]] * (1 + (i==1) * factor)
          ans[[X]]$labor_rate  <- input$labor_rate * (1 + (i==2) * factor)
          ans[[X]]$inflation_robot  <- input$inflation_robot * (1 + (i==3) * factor)
          ans[[X]]$inflation_margin <- input$inflation_margin * (1 + (i==4) * factor)
          ans[[X]]$inflation_labor  <- input$inflation_labor * (1 + (i==5) * factor)
          
          source(file.path("session_files", "session_calculation_steps.R"), local=TRUE)  # Calculates main results
        
          table_sensitivity_plot_before_tax[n,i] <<- ans[[X]]$net_annual_impact_before_tax
          table_sensitivity_plot_after_tax[n,i]  <<- ans[[X]]$ANPV
          }
        })
         
      table_sensitivity_plot_before_tax <- cbind(change_vars,table_sensitivity_plot_before_tax)
      table_sensitivity_plot_after_tax <- cbind(change_vars,table_sensitivity_plot_after_tax)
      colnames(table_sensitivity_plot_before_tax) <- c("Percentage Change",sensitivity_labels)
      colnames(table_sensitivity_plot_after_tax) <- c("Percentage Change",sensitivity_labels)
      
        ans[[paste0(x,"_se")]]$table_sensitivity_plot <-
          list(before_tax= table_sensitivity_plot_before_tax %>% round() %>% data.frame(), 
               after_tax = table_sensitivity_plot_after_tax  %>% round() %>% data.frame())
        
    })
    
    lapply(c(1:2), function(j) { })
    
    output[[paste0("sensitivity_plot",x)]] <- renderGvis({
      tbl <- ans[[paste0(x,"_se")]]$table_sensitivity_plot
      need(length(tbl)>0 & input[[paste0("sensitivity_plot_button",x)]]>0, "") %>% validate() 
      
      on.exit({
        shinyjs:: enable(paste0("sensitivity_plot_button",x))
        shinyjs:: hide(paste0("sensitivity_plot_message",x))
      })
      
      if (input[[paste0("sensitivity_plot_NAI",x)]]=="before tax") {
        tbl2 <- tbl$before_tax 
      } else {
        tbl2 <- tbl$after_tax 
      }
      
      colnames(tbl2) <- lapply(colnames(tbl2), 
                              function(str) gsub("[.]*$|[.]*(?=[.])","",str, perl = TRUE)) %>% unlist()
      
      gvisLineChart(tbl2, xvar=colnames(tbl2)[1],  
                    yvar=colnames(tbl2)[-1], 
                    options=list(title="Sensitivity over a selected range",
                                 vAxis=paste("{title:'Net Annual Impact ", 
                                             input[[paste0("sensitivity_plot_NAI",x)]]," ($)'}"),
                                 hAxis="{title:'% Change'}", 
                                 chartArea ='{width: "50%", height: "65%" }'
                                 # width=800, height=400
                                 )
                    )
    })  
    
   
  # ------------- Summary ------------
  # similar to operations in session_summary.R but defined for each profile x 

  lapply(c('before_tax','after_tax'), function(loc_NAI) {
    sum[[paste0("sensitivity_table_",loc_NAI,x)]] <- reactive({ 

        mat <- nulls(length(summary_table_vars), length(sensitivity_labels))
        rownames(mat) <- summary_table_varnames
        colnames(mat) <- sensitivity_labels
        loc_NAI_split <- sub("_"," ",loc_NAI)
        
        for (j in seq_along(sensitivity_vars)) {
          for (i in seq_along(summary_table_vars)) {
            X <- paste0(x,"_se",j)
            ans[[paste0(x,"_se_calc",j)]]()

            mat[i,j] <- ans[[paste0(X,"_da")]]()[[loc_NAI_split]][[summary_table_vars[i]]]
          }
        }
        
        mat %>% data.frame()
        
      })
    
    output[[paste0("sensitivity_table_",loc_NAI,x)]] <- DT::renderDataTable({

      tbl <- sum[[paste0("sensitivity_table_",loc_NAI,x)]]()
      need(length(tbl) > 0, "NA") %>% validate()
      
      tbl[7:8,] <- tbl[7:8,]*100
      tbl <- round(tbl)
      tbl[7:8,] <- tbl[7:8,]/100
      
      colnames(tbl) <- sensitivity_labels
      
      DT::datatable(tbl,
                    options = list(
                      scrollX = TRUE,
                      pageLength = 8,
                      bLengthChange =FALSE,
                      searching = FALSE
                    )) %>%
        formatCurrency(colnames(tbl))
    })
  })

  
  lapply(c('operating_income','after_tax_cash_flow'), function(loc_var) {
    sum[[paste0("sensitivity_table_",loc_var,x)]]  <- reactive({
      
      need(length(ans[[paste0(x,"_se",2)]]$table_cash_flow)>0,"NA") %>% validate()
      
      n_years_max <- ans[[paste0(x,"_se",1)]]$planning_horizon + 1
      for (i in 2:length(sensitivity_vars)) {
        n_years_max <- max(n_years_max, (ans[[paste0(x,"_se",i)]]$planning_horizon + 1))
      }
      
      mat <- nulls(n_years_max, length(sensitivity_vars))
      colnames(mat) <- sensitivity_labels
      
      for (j in seq_along(sensitivity_vars)) {
        X <- paste0(x,"_se",j)
        tmp_zero <- rep(0, n_years_max - (ans[[X]]$planning_horizon + 1))
        mat[,j] <- c(ans[[X]]$table_cash_flow[[paste(loc_var)]], tmp_zero) %>% round()
      }
      
      mat <- cbind(Year=c(0:(n_years_max-1)), mat)
      mat %>% data.frame()
    })
    
    
    output[[paste0("sensitivity_table_",loc_var,x)]]  <- DT::renderDataTable({ 
      tbl <- sum[[paste0("sensitivity_table_",loc_var,x)]]()
      need(length(tbl) > 0, "NA") %>% validate()
     
      colnames(tbl) <- c("Year", sensitivity_labels)
      
      L <- length(tbl$Year)
      DT::datatable(tbl,
                    rownames = FALSE,
                    options = list(
                      scrollX = TRUE,
                      pageLength = L,
                      bLengthChange =FALSE,
                      searching = FALSE)) %>%
        formatCurrency(colnames(tbl)[-1])
    })

  })
  
  output[[paste0("sensitivity_impacts",x)]] <- renderGvis({ 

      need(length(sum[[paste0("sensitivity_table_after_tax",x)]]()) > 0, "NA") %>% validate()
      
     base_name <- gsub(" ",".",refProfileName(x))
       
      tbl <- data.frame( 
        sen_vars=c("Baseline", sensitivity_labels), 
        before.tax=lapply(as.numeric(
          c(sum[["table_before_tax"]]()[base_name][1,],
          sum[[paste0("sensitivity_table_before_tax",x)]]()[1,])), round) %>% unlist(),
        after.tax =lapply(as.numeric(as.numeric(
          c(sum[["table_after_tax"]]()[base_name][1,],
            sum[[paste0("sensitivity_table_after_tax",x)]]()[1,]))), round) %>% unlist()
      ) 
      
      gvisColumnChart(tbl, xvar="sen_vars",
                      yvar=c("before.tax","after.tax"),
                      options=list( #title="Net Annual Impact ($)",
                        # titleTextStyle="{fontSize:16}",
                        legend="top")
      )
    })
  
  output[[paste0("sensitivity_operating_income_chart",x)]] <- renderGvis({
    tbl <- sum[[paste0("sensitivity_table_operating_income",x)]]()
    need(length(tbl) > 0, "NA") %>% validate()
    
    colnames(tbl) <- lapply(colnames(tbl), 
                            function(str) gsub("[.]*$|[.]*(?=[.])","",str, perl = TRUE)) %>% unlist()
    varnames <- colnames(tbl)[-1]
    gvisLineChart(tbl, xvar="Year",
                  yvar= varnames,
                  options=list(
                    title="Before-tax Operating Income",
                    vAxis="{title:'Net Annual Impact ($)'}",
                    hAxis="{title:'Year'}",
                    chartArea ='{width: "50%", height: "65%" }',
                    width=800, height=400
                  ))
  })
  
  output[[paste0("sensitivity_cashflow_chart",x)]] <- renderGvis({
  
    tbl <- sum[[paste0("sensitivity_table_after_tax_cash_flow",x)]]()
    need(length(tbl) > 0, "NA") %>% validate()
    
    colnames(tbl) <- lapply(colnames(tbl),
                            function(str) gsub("[.]*$|[.]*(?=[.])","",str, perl = TRUE)) %>% unlist()
    varnames <- colnames(tbl)[-1]
    
    gvisLineChart(tbl, xvar="Year",
                  yvar= varnames,
                  options=list(
                    title="After-tax Cash Flow",
                    vAxis="{title:'Net Annual Impact ($)'}",
                    hAxis="{title:'Year'}",
                    chartArea ='{width: "50%", height: "65%" }',
                    width=800, height=400
                  )
                  ) 
  })
  
  
})


