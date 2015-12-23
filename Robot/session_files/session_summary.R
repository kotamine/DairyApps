

# Create summary tables and charts
summary_table_varnames <- c("Net Annual Impact", "Milk Income minus Feed Cost","Labor and Repair Cost",
                            "Cost of Capital", "Others","Inflation Adjustments",
                            "IOFC per cow","IOFC per cwt")

summary_table_vars <- c("NAI","milk_feed","labor_repair","capital","misc","inflation",
                        "IOFC_diff", "IOFC_diff_cwt")

lapply(c('before_tax','after_tax'), function(loc_NAI) {
  sum[[paste0("table_",loc_NAI)]] <- reactive({ 
    
    mat <- nulls(length(summary_table_vars), length(base_profiles))
    rownames(mat) <- summary_table_varnames
    colnames(mat) <- lapply(base_profiles, refProfileName) %>% unlist()
    loc_NAI_split <- sub("_"," ",loc_NAI)
    
    for (j in seq_along(base_profiles)) {
      for (i in seq_along(summary_table_vars)) {
        mat[i,j] <- ans[[paste0(base_profiles[j],"_da")]]()[[loc_NAI_split]][[summary_table_vars[i]]]
      }
    }
    
    mat %>% data.frame()
    
  })
})


lapply(c('operating_income','after_tax_cash_flow'), function(loc_var) {
  sum[[paste0("table_",loc_var)]]  <- reactive({
    
    n_years_max <- ans[[base_profiles[1]]]$planning_horizon + 1
    for (i in 2:length(base_profiles)) {
      n_years_max <- max(n_years_max, (ans[[base_profiles[i]]]$planning_horizon + 1))
    }
    
    mat <- nulls(n_years_max, length(base_profiles))
    colnames(mat) <- lapply(base_profiles, refProfileName) %>% unlist()
    
    tmp_zero <- rep(0, n_years_max - (ans[[base_profiles[1]]]$planning_horizon + 1))
    for (j in seq_along(base_profiles)) {
      mat[,j] <- c(ans[[base_profiles[j]]]$table_cash_flow[[paste(loc_var)]], tmp_zero) %>% round()
    }
    
    mat <- cbind(Year=c(0:(n_years_max-1)), mat)
    mat %>% data.frame()
  })
})


output$summary_table_before_tax <- DT::renderDataTable({
  browser()
  tbl <- sum[["table_before_tax"]]()
  need(length(tbl) > 0, "NA") %>% validate()
  
  tbl[7:8,] <- tbl[7:8,]*100
  tbl <- round(tbl)
  tbl[7:8,] <- tbl[7:8,]/100
  
  DT::datatable(tbl,
                options = list(
                  scrollX = TRUE,
                  pageLength = 8,
                  bLengthChange =FALSE,
                  searching = FALSE
                )) %>%
    formatCurrency(colnames(tbl))
})


output$summary_table_after_tax <- DT::renderDataTable({
  tbl <- sum[["table_after_tax"]]()
  need(length(tbl) > 0, "NA") %>% validate()
  
  tbl[7:8,] <- tbl[7:8,]*100
  tbl <- round(tbl)
  tbl[7:8,] <- tbl[7:8,]/100

  DT::datatable(tbl,
                options = list(
                  scrollX = TRUE,
                  pageLength = 8,
                  bLengthChange =FALSE,
                  searching = FALSE
                )) %>%
    formatCurrency(colnames(tbl))
})


output$summary_table_operating_income  <- DT::renderDataTable({
  tbl <- sum[["table_operating_income"]]()
  need(length(tbl) > 0, "NA") %>% validate()
  
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


output$summary_table_after_tax_cash_flow  <- DT::renderDataTable({
  tbl <- sum[["table_after_tax_cash_flow"]]()
  need(length(tbl) > 0, "NA") %>% validate()
  
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


output$profile_cashflow_chart <- renderGvis({
  tbl <- sum[["table_after_tax_cash_flow"]]()
  need(length(tbl) > 0, "NA") %>% validate()
  
  varnames <- colnames(tbl)[-1]
  gvisLineChart(tbl, xvar="Year",
                yvar= varnames,
                options=list(
                  title="After-tax Cash Flow",
                  vAxis="{title:'Net Annual Impact ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom",
                  width=800, height=400
                ))
})

output$profile_operating_income_chart <- renderGvis({
  tbl <- sum[["table_operating_income"]]()
  need(length(tbl) > 0, "NA") %>% validate()
  
  varnames <- colnames(tbl)[-1]
  gvisLineChart(tbl, xvar="Year",
                yvar= varnames,
                options=list(
                  title="Before-tax Operating Income",
                  vAxis="{title:'Net Annual Impact ($)'}",
                  hAxis="{title:'Year'}",
                  legend="bottom",
                  width=800, height=400
                ))
})

output$profile_impacts <- renderGvis({
  need(length(sum[["table_after_tax"]]()) > 0, "NA") %>% validate()
  
  tbl <- data.frame(
    profile=lapply(base_profiles, refProfileName) %>% unlist(),
    before.tax=as.numeric(round(sum[["table_before_tax"]]()[1,])),
    after.tax =as.numeric(round(sum[["table_after_tax"]]()[1,]))
  )
  
  gvisColumnChart(tbl, xvar="profile",
                  yvar=c("before.tax","after.tax"),
                  options=list( #title="Net Annual Impact ($)",
                    # titleTextStyle="{fontSize:16}",
                    legend="top")
  )
})



