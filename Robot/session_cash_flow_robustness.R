##  Mostly copied from session_cash_flow_robot_parlor.R.   
## This file may be merged with the above file later. 



## ------ Prepares cash flow, debt, and depreciation tables -------


isolate({

  ## ------------ Depreciation Table ------------
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots")  {
    yr_AGDS_robot <- 7
    yr_SLD_robot <- 10
  } else { 
    yr_AGDS_robot <- 14  # Depreciation years for parlors
    yr_SLD_robot <-  20
  }
  
  
  yr_AGDS_housing <- 10
  yr_SLD_housing <- 15
  
  n_years <- max(rb$housing_years, 
                 input$n_yr_housing, input$n_yr_milking1,
                 yr_AGDS_robot*(input$dep_method=="d1") +  yr_AGDS_robot*(input$dep_method!="d1"), 
                 yr_AGDS_housing*(input$dep_method=="d1") +  yr_AGDS_housing*(input$dep_method!="d1"))  
  
  
  dep_robot <- rep(0,n_years); dep_housing  <- rep(0,n_years)
  
  ## if Accelerated depreciation method is used 
  if (input$dep_method=="d1") {
    # setting salvage = 0 for total depreciation 
    dep_robot[1:yr_AGDS_robot] <- vdb(rb$cost_milking, 0,
                                      yr_AGDS_robot, factor=1.5, sequence=TRUE) 
    
    if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
      dep_robot[(1+rb$robot_years):(rb$robot_years +yr_AGDS_robot)] <-  vdb(rb$cost_milking2, 0, 
                                                                            yr_AGDS_robot, factor=1.5, sequence=TRUE)
    }
    
    dep_housing [1:yr_AGDS_housing] <- vdb(rb$cost_housing, 0,
                                           yr_AGDS_housing, factor=1.5, sequence=TRUE) 
  } else {
    
    ## if Straight line depreciation method is used 
    dep_robot[1:yr_SLD_robot] <- (rb$cost_milkingt - 0)/yr_SLD_robot
    dep_robot[(1+rb$robot_years):(rb$robot_years + yr_SLD_robot)] <- (rb$cost_milking2 - 0)/yr_SLD_robot
    
    dep_housing[1:yr_SLD_housing] <- (rb$cost_housing - 0)/yr_SLD_housing
  }
  
  # add back salvage at the end
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
    dep_robot[rb$robot_years] <-  -rb$salvage_milking_fv1 
    dep_robot[(2*rb$robot_years)] <-  -rb$salvage_milking_fv2
  } else {
    dep_robot[rb$milking_years] <-  -rb$salvage_milking_fv1 
  }
  dep_housing[rb$housing_years] <-  -rb$salvage_housing_fv 
  
  table_depreciation <- cbind(c(1:n_years),dep_robot,dep_housing) %>% data.frame() 
  colnames(table_depreciation) <- c("year","depreciation_robot","depreciation_housing")
  table_depreciation$total <- table_depreciation$depreciation_robot + table_depreciation$depreciation_housing 
  
  
  ## ------------ Debt Table ------------
  
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
    tbl_robot <- debt_table(rb$loan_milking1, input$r_milking1/100, input$n_yr_milking1, n_years, 1) +
      + debt_table(rb$loan_milking2, input$r_milking2/100, input$n_yr_milking2, n_years, rb$robot_years+1) * 
      (input$n_robot_life>=2)
    tbl_robot[,1] <- tbl_robot[,1]/2
    
  } else {
    tbl_robot <- debt_table(rb$loan_milking1, input$r_milking1/100, input$n_yr_milking1, n_years, 1)
  }
  colnames(tbl_robot) <- lapply(colnames(tbl_robot), function(x) { paste0("robot_",x)}) %>% unlist()
  
  tbl_barn <- debt_table(rb$loan_housing, input$r_housing/100, input$n_yr_housing, n_years, 1)
  colnames(tbl_barn) <- lapply(colnames(tbl_barn), function(x) { paste0("barn_",x)}) %>% unlist()
  
  table_debt <- cbind(tbl_robot, tbl_barn[,c(-1)])
  colnames(table_debt) <- c("year",colnames(table_debt)[c(-1)])
  table_debt$interest_total <- table_debt$robot_interest + table_debt$barn_interest 
  table_debt$principal_total <- table_debt$robot_principal + table_debt$barn_principal
  
  
  
  ## ------------ Cash Flow Table ------------
  
  table_cash_flow <- matrix(c(c(1:n_years), rep(rep(0,n_years),10)),ncol=11,byrow=FALSE)  %>% data.frame()
  colnames(table_cash_flow) <- c("year","revenue_minus_expense", "interest_total","depreciation",
                                 "operating_income", "income_tax", "principal_total",
                                 "add_back_depr","downpayment", "salvage", "after_tax_cash_flow")
  
  table_cash_flow$interest_total <-  -table_debt$interest_total
  table_cash_flow$principal_total <-  -table_debt$principal_total
  table_cash_flow$depreciation <-   -table_depreciation$total
  table_cash_flow$add_back_depr <-  -table_cash_flow$depreciation 
  
  # Modified 
  # table_cash_flow$revenue_minus_expense <- lapply(c(1:n_years), function(t) {
  #   (rb$inc_rev_total - rb$inc_exp_total + rb$inc_exp_capital_recovery)*(1+input$inflation_margin/100)^(t) +
  #     + dec_exp_total * (1+input$inflation_labor)^(t)
  # }) %>% unlist()
  
  table_cash_flow$revenue_minus_expense <- lapply(c(1:n_years), function(t) {
    (rb$inc_rev_total - rb$inc_exp_total)*(1+input$inflation_margin/100)^(t-1) +
      + rb$dec_exp_total * (1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  table_cash_flow$operating_income <- table_cash_flow$revenue_minus_expense + table_cash_flow$interest_total +
    + table_cash_flow$depreciation
  
  table_cash_flow$income_tax <-  - table_cash_flow$operating_income * input$tax_rate/100 
  
  # Insert row: year 0
  table_cash_flow <- rbind(0, table_cash_flow)
  
  # downpayments and salvage values
  table_cash_flow$downpayment[1] <- -(input$down_milking1 + input$down_housing)
  if (input$robot_parlor=="OFF" | input$profile_choice=="Robots") {
    table_cash_flow$downpayment[(1 + rb$robot_years)] <-   -  input$down_milking2 
    table_cash_flow$salvage[(1 + rb$robot_years)] <-  rb$salvage_milking_fv1 
    table_cash_flow$salvage[(1 + 2*rb$robot_years)] <- rb$salvage_milking_fv2 + rb$salvage_housing_fv
  } else {
    table_cash_flow$salvage[(1 + rb$milking_years)] <-  rb$salvage_milking_fv1 + rb$salvage_housing_fv
  }
  table_cash_flow$after_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$salvage  +
    + table_cash_flow$operating_income + table_cash_flow$income_tax + 
    + table_cash_flow$principal_total + table_cash_flow$add_back_depr
  
  rb$table_cash_flow <- table_cash_flow
  rb$table_debt <- table_debt
  rb$table_depreciation <- table_depreciation
  
  rate <- rb$WACC/100
  
  rb$NPV <- npv(rate, table_cash_flow$after_tax_cash_flow[-1]) +
    + table_cash_flow$after_tax_cash_flow[1]
  rb$ANPV <- -pmt(rate, rb$housing_years,rb$NPV)
  rb$ANPVr <- rb$ANPV * rb$deflator
  rb$IRR <- irr(table_cash_flow$after_tax_cash_flow) * 100
  if (rb$IRR>1000) {
    rb$IRR <- NA
  } 
  # change the rates for MIRR ?
  rb$MIRR <- mirr(table_cash_flow$after_tax_cash_flow, input$interest/100, input$interest/100) * 100
  
})


