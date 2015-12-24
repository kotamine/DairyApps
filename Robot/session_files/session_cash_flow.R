
## ------ Prepares cash flow, debt, and depreciation tables -------

  
   # browser()
  
  ## ------------ Depreciation Table ------------
  yr_AGDS_milking <- 7
  yr_SLD_milking <- 10
  
  yr_AGDS_housing <- 10
  yr_SLD_housing <- 15
  
  n_years <- max(ans[[X]]$planning_horizon, 
                 yr_AGDS_housing*(input$dep_method=="d1"), 
                 yr_SLD_housing*(input$dep_method=="d2"))  
  
  
  dep_milking <- rep(0,n_years); dep_housing  <- rep(0,n_years)
  
  ## if Accelerated depreciation method is used 
  if (input$dep_method=="d1") {
    # setting salvage = 0 for total depreciation 
    dep_milking[1:yr_AGDS_milking] <- vdb(ans[[X]]$cost_milking1, 0,
                                      yr_AGDS_milking, factor=1.5, sequence=TRUE) 
    if (input[[paste0("n_sets",x)]] ==2) {
      dep_milking[(1+input[[paste0("useful_years",x)]]):(input[[paste0("useful_years",x)]] + yr_AGDS_milking)] <-
        vdb(ans[[X]]$cost_milking2, 0,  yr_AGDS_milking, factor=1.5, sequence=TRUE) 
    }

    dep_housing [1:yr_AGDS_housing] <- vdb(ans[[X]]$cost_housing, 0,
                                           yr_AGDS_housing, factor=1.5, sequence=TRUE) 
  } else {
    
    ## if Straight line depreciation method is used 
    dep_milking[1:yr_SLD_milking] <- (ans[[X]]$cost_milking1 - 0)/yr_SLD_milking
    if (input[[paste0("n_sets",x)]] >=2) {
      dep_milking[(1+input[[paste0("useful_years",x)]]):(input[[paste0("useful_years",x)]] + yr_SLD_milking)] <- 
        (ans[[X]]$cost_milking2 - 0)/yr_SLD_milking
    }
    
    dep_housing[1:yr_SLD_housing] <- (ans[[X]]$cost_housing - 0)/yr_SLD_housing
  }
  
  # add back salvage at the end
    dep_milking[input[[paste0("useful_years",x)]]] <-  -ans[[X]]$salvage_milking_fv1 
    if (input[[paste0("n_sets",x)]] >=2) {
    dep_milking[(2*input[[paste0("useful_years",x)]])] <-  -ans[[X]]$salvage_milking_fv2
  } 
  dep_milking <- c(rep(0,input[[paste0("yr_system1",x)]]),dep_milking)
  table_depreciation <- cbind(c(1:n_years),dep_milking,dep_housing) %>% data.frame() 
  colnames(table_depreciation) <- c("year","depreciation_milking_system","depreciation_housing")
  table_depreciation$total <- table_depreciation$depreciation_milking + table_depreciation$depreciation_housing 
  
  
  ## ------------ Debt Table ------------
    tbl_milking <- debt_table(ans[[X]]$loan_milking1,input[[paste0("r_milking1",x)]]/100, input[[paste0("n_yr_milking1",x)]], n_years, 1) 
  if (ans[[X]]$n_sets == 2) {
    tbl_milking <- tbl_milking + debt_table(ans[[X]]$loan_milking2, input[[paste0("r_milking1",x)]]/100, input[[paste0("n_yr_milking1",x)]], n_years, input[[paste0("useful_years",x)]]+1) 
     tbl_milking[,1] <- tbl_milking[,1]/2
  }
  
  colnames(tbl_milking) <- lapply(colnames(tbl_milking), function(x) { paste0("milking_",x)}) %>% unlist()
  
  tbl_barn <- debt_table(ans[[X]]$loan_housing, input[[paste0("r_housing",x)]]/100, input[[paste0("n_yr_housing",x)]], n_years, 1)
  colnames(tbl_barn) <- lapply(colnames(tbl_barn), function(x) { paste0("barn_",x)}) %>% unlist()
  
  tbl_milking <- c(rep(0,input[[paste0("yr_system1",x)]]),tbl_milking)

  table_debt <- cbind(tbl_milking, tbl_barn[,c(-1)])
  colnames(table_debt) <- c("year",colnames(table_debt)[c(-1)])
  table_debt$interest_total <- table_debt$milking_interest + table_debt$barn_interest 
  table_debt$principal_total <- table_debt$milking_principal + table_debt$barn_principal
  
  
  
  ## ------------ Cash Flow Table ------------
  
  table_cash_flow <- matrix(c(c(1:n_years), rep(rep(0,n_years),11)),ncol=12,byrow=FALSE)  %>% data.frame()
  colnames(table_cash_flow) <- c("year","revenue_minus_expense", "interest_total","depreciation",
                                 "operating_income", "income_tax", "principal_total",
                                 "add_back_depr","downpayment", "salvage", "after_tax_cash_flow",
                                 "before_tax_cash_flow")
  
  table_cash_flow$interest_total <-  -table_debt$interest_total
  table_cash_flow$principal_total <-  -table_debt$principal_total
  table_cash_flow$depreciation <-   -table_depreciation$total
  table_cash_flow$add_back_depr <-  -table_cash_flow$depreciation 
  
  # Modified from the original version which is this:
  # table_cash_flow$revenue_minus_expense <- lapply(c(1:n_years), function(t) {
  #   (ans[[X]]$inc_rev_total - ans[[X]]$inc_exp_total + ans[[X]]$inc_exp_capital_recovery)*(1+ans[[X]]$inflation_margin/100)^(t) +
  #     + dec_exp_total * (1+input$inflation_labor)^(t)
  # }) %>% unlist()
  
  table_cash_flow$revenue_minus_expense <- lapply(c(1:n_years), function(t) {
    (ans[[X]]$inc_rev_total - ans[[X]]$inc_exp_total)*(1+ans[[X]]$inflation_margin/100)^(t-1) +
      + ans[[X]]$dec_exp_total * (1+ans[[X]]$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  if (ans[[X]]$planning_horizon < n_years) {
    table_cash_flow$revenue_minus_expense[(ans[[X]]$planning_horizon+1):n_years] <- 0 
  }
  
  table_cash_flow$operating_income <- table_cash_flow$revenue_minus_expense + table_cash_flow$interest_total +
    + table_cash_flow$depreciation
  
  table_cash_flow$income_tax <-  - table_cash_flow$operating_income * input$tax_rate/100 
  
  # Insert row: year 0
  table_cash_flow <- rbind(0, table_cash_flow)
  
  # downpayments and salvage values
  table_cash_flow$downpayment[1] <- -(input[[paste0("down_milking1",x)]] + input[[paste0("down_housing",x)]])
  if (x=="Robots") {
    table_cash_flow$downpayment[(1 + input[[paste0("useful_years",x)]])] <-   -  input[[paste0("down_milking2",x)]]
    table_cash_flow$salvage[(1 + input[[paste0("useful_years",x)]])] <-  ans[[X]]$salvage_milking_fv1 
    table_cash_flow$salvage[(1 + 2*input[[paste0("useful_years",x)]])] <- ans[[X]]$salvage_milking_fv2 + ans[[X]]$salvage_housing_fv
  } else {
    table_cash_flow$salvage[(1 + input[[paste0("splanning_horizon",x)]])] <-  ans[[X]]$salvage_milking_fv1 + ans[[X]]$salvage_housing_fv
  }
  table_cash_flow$after_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$salvage  +
    + table_cash_flow$operating_income + table_cash_flow$income_tax + 
    + table_cash_flow$principal_total + table_cash_flow$add_back_depr
  
  table_cash_flow$before_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$salvage  +
    + table_cash_flow$principal_total + table_cash_flow$revenue_minus_expense + table_cash_flow$interest_total 
    
  ans[[X]]$table_cash_flow <- table_cash_flow
  ans[[X]]$table_debt <- table_debt
  ans[[X]]$table_depreciation <- table_depreciation
  
   rate <- ans[[X]]$WACC/100
  

  ans[[X]]$NPV <- npv(rate, table_cash_flow$after_tax_cash_flow[-1]) +
    + table_cash_flow$after_tax_cash_flow[1]
  ans[[X]]$ANPV <- -pmt(rate,ans[[X]]$planning_horizon,ans[[X]]$NPV)
  #ans[[X]]$ANPVr <- ans[[X]]$ANPV  * ans[[X]]$deflator
  # IRR is probably not appropriate
  IRR <- irr(table_cash_flow$after_tax_cash_flow) * 100
  if (IRR <=1000) {
    ans[[X]]$IRR <- IRR
  } else {
    ans[[X]]$IRR <- NA
  }
  # MIRR is probably not appropriate
  ans[[X]]$MIRR <- mirr(table_cash_flow$after_tax_cash_flow, input$interest/100, input$interest/100) * 100
  
  ans[[X]]$ROI <-  ans[[X]]$NPV/ (ans[[X]]$total_investment + ans[[X]]$cost_milking2) * 100
  


