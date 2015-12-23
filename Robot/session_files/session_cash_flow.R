
# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increaseRobots, input$herd_increaseRetrofit, input$herd_increaseNew
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "cost_housing_cow",
#     "down_housing", "down_milking1", "down_milking2",
#     "salvage_housing", "salvage_milking1", 
#     "planning_horizon", "cost_parlors", "cost_robot", "useful_years", "n_robot")
# 


## ------ Prepares cash flow, debt, and depreciation tables -------

  
   # browser()
  
  ## ------------ Depreciation Table ------------
  yr_AGDS_milking <- 7
  yr_SLD_milking <- 10
  
  yr_AGDS_housing <- 10
  yr_SLD_housing <- 15
  
  n_years <- max(ans[[x]]$planning_horizon, 
                 yr_AGDS_housing*(input$dep_method=="d1"), 
                 yr_SLD_housing*(input$dep_method=="d2"))  
  
  
  dep_milking <- rep(0,n_years); dep_housing  <- rep(0,n_years)
  
  ## if Accelerated depreciation method is used 
  if (input$dep_method=="d1") {
    # setting salvage = 0 for total depreciation 
    dep_milking[1:yr_AGDS_milking] <- vdb(ans[[x]]$cost_milking1, 0,
                                      yr_AGDS_milking, factor=1.5, sequence=TRUE) 
    if (input[[paste0("n_sets",x)]] ==2) {
      dep_milking[(1+input[[paste0("useful_years",x)]]):(input[[paste0("useful_years",x)]] + yr_AGDS_milking)] <-
        vdb(ans[[x]]$cost_milking2, 0,  yr_AGDS_milking, factor=1.5, sequence=TRUE) 
    }

    dep_housing [1:yr_AGDS_housing] <- vdb(ans[[x]]$cost_housing, 0,
                                           yr_AGDS_housing, factor=1.5, sequence=TRUE) 
  } else {
    
    ## if Straight line depreciation method is used 
    dep_milking[1:yr_SLD_milking] <- (ans[[x]]$cost_milking1 - 0)/yr_SLD_milking
    if (input[[paste0("n_sets",x)]] >=2) {
      dep_milking[(1+input[[paste0("useful_years",x)]]):(input[[paste0("useful_years",x)]] + yr_SLD_milking)] <- 
        (ans[[x]]$cost_milking2 - 0)/yr_SLD_milking
    }
    
    dep_housing[1:yr_SLD_housing] <- (ans[[x]]$cost_housing - 0)/yr_SLD_housing
  }
  
  # add back salvage at the end
    dep_milking[input[[paste0("useful_years",x)]]] <-  -ans[[x]]$salvage_milking_fv1 
    if (input[[paste0("n_sets",x)]] >=2) {
    dep_milking[(2*input[[paste0("useful_years",x)]])] <-  -ans[[x]]$salvage_milking_fv2
  } 
  dep_milking <- c(rep(0,input[[paste0("yr_system1",x)]]),dep_milking)
  table_depreciation <- cbind(c(1:n_years),dep_milking,dep_housing) %>% data.frame() 
  colnames(table_depreciation) <- c("year","depreciation_milking_system","depreciation_housing")
  table_depreciation$total <- table_depreciation$depreciation_milking + table_depreciation$depreciation_housing 
  
  
  ## ------------ Debt Table ------------
    tbl_milking <- debt_table(ans[[x]]$loan_milking1,input[[paste0("r_milking1",x)]]/100, input[[paste0("n_yr_milking1",x)]], n_years, 1) 
  if (ans[[x]]$n_sets == 2) {
    tbl_milking <- tbl_milking + debt_table(ans[[x]]$loan_milking2, input[[paste0("r_milking1",x)]]/100, input[[paste0("n_yr_milking1",x)]], n_years, input[[paste0("useful_years",x)]]+1) 
     tbl_milking[,1] <- tbl_milking[,1]/2
  }
  
  colnames(tbl_milking) <- lapply(colnames(tbl_milking), function(x) { paste0("milking_",x)}) %>% unlist()
  
  tbl_barn <- debt_table(ans[[x]]$loan_housing, input[[paste0("r_housing",x)]]/100, input[[paste0("n_yr_housing",x)]], n_years, 1)
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
  #   (ans[[x]]$inc_rev_total - ans[[x]]$inc_exp_total + ans[[x]]$inc_exp_capital_recovery)*(1+input$inflation_margin/100)^(t) +
  #     + dec_exp_total * (1+input$inflation_labor)^(t)
  # }) %>% unlist()
  
  table_cash_flow$revenue_minus_expense <- lapply(c(1:n_years), function(t) {
    (ans[[x]]$inc_rev_total - ans[[x]]$inc_exp_total)*(1+input$inflation_margin/100)^(t-1) +
      + ans[[x]]$dec_exp_total * (1+input$inflation_labor/100)^(t-1)
  }) %>% unlist()
  
  if (ans[[x]]$planning_horizon < n_years) {
    table_cash_flow$revenue_minus_expense[(ans[[x]]$planning_horizon+1):n_years] <- 0 
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
    table_cash_flow$salvage[(1 + input[[paste0("useful_years",x)]])] <-  ans[[x]]$salvage_milking_fv1 
    table_cash_flow$salvage[(1 + 2*input[[paste0("useful_years",x)]])] <- ans[[x]]$salvage_milking_fv2 + ans[[x]]$salvage_housing_fv
  } else {
    table_cash_flow$salvage[(1 + input[[paste0("splanning_horizon",x)]])] <-  ans[[x]]$salvage_milking_fv1 + ans[[x]]$salvage_housing_fv
  }
  table_cash_flow$after_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$salvage  +
    + table_cash_flow$operating_income + table_cash_flow$income_tax + 
    + table_cash_flow$principal_total + table_cash_flow$add_back_depr
  
  table_cash_flow$before_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$salvage  +
    + table_cash_flow$principal_total + table_cash_flow$revenue_minus_expense + table_cash_flow$interest_total 
    
  ans[[x]]$table_cash_flow <- table_cash_flow
  ans[[x]]$table_debt <- table_debt
  ans[[x]]$table_depreciation <- table_depreciation
  
   rate <- ans[[x]]$WACC/100
  

  ans[[x]]$NPV <- npv(rate, table_cash_flow$after_tax_cash_flow[-1]) +
    + table_cash_flow$after_tax_cash_flow[1]
  ans[[x]]$ANPV <- -pmt(rate,ans[[x]]$planning_horizon,ans[[x]]$NPV)
  #ans[[x]]$ANPVr <- ans[[x]]$ANPV  * ans[[x]]$deflator
  # IRR is probably not appropriate
  IRR <- irr(table_cash_flow$after_tax_cash_flow) * 100
  if (IRR <=1000) {
    ans[[x]]$IRR <- IRR
  } else {
    ans[[x]]$IRR <- NA
  }
  # MIRR is probably not appropriate
  ans[[x]]$MIRR <- mirr(table_cash_flow$after_tax_cash_flow, input$interest/100, input$interest/100) * 100
  
  ans[[x]]$ROI <-  ans[[x]]$NPV/ (ans[[x]]$total_investment + ans[[x]]$cost_milking2) * 100
  


