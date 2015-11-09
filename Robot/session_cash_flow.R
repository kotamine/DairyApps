## ------ Prepares cash flow, debt, and depreciation tables -------


# observeEvent(input$calculate_cash_flow, {

isolate({
n_years <- input$horizon

browser()


## ------------ Depreciation Table ------------

yr_AGDS_robot <- 7
yr_SLD_robot <- 10

yr_AGDS_housing <- 10
yr_SLD_housing <- 15

dep_robot <- rep(0,n_years); dep_housing  <- rep(0,n_years)

## if Accelerated depreciation method is used 
if (input$dep_method=="d1") {
  # setting salvage = 0 for total depreciation 
  dep_robot[1:yr_AGDS_robot] <- vdb(rv$robot_invest, 0,
                                    yr_AGDS_robot, factor=1.5, sequence=TRUE) 
  
  dep_robot[(1+input$robot_years):(input$robot_years +yr_AGDS_robot)] <-  vdb(rv$robot_invest2, 0, 
                                                                              yr_AGDS_robot, factor=1.5, sequence=TRUE)
  
  dep_housing [1:yr_AGDS_housing] <- vdb(rv$cost_housing, 0,
                                         yr_AGDS_housing, factor=1.5, sequence=TRUE) 
} else {
  
  ## if Straight line depreciation method is used 
  dep_robot[1:yr_SLD_robot] <- (rv$robot_invest - 0)/yr_SLD_robot
  dep_robot[(1+input$robot_years):(input$robot_years + yr_SLD_robot)] <- (rv$robot_invest2 - 0)/yr_SLD_robot
  
  dep_housing[1:yr_SLD_housing] <- (rv$cost_housing - 0)/yr_SLD_housing
}

# add back salvage at the end
dep_robot[input$robot_years] <-  -rv$salvage_robot1 
dep_robot[(2*input$robot_years)] <-  -rv$salvage_robot2 
dep_housing[rv$housing_years] <-  -rv$salvage_housing

table_depreciation <- cbind(c(1:n_years),dep_robot,dep_housing) %>% data.frame() 
colnames(table_depreciation) <- c("year","depreciation_robot","depreciation_housing")
table_depreciation$total <- table_depreciation$depreciation_robot + table_depreciation$depreciation_housing 


browser()
## ------------ Debt Table ------------

tbl_robot <- debt_table(rv$loan_robot1, input$r_robot1/100, input$n_yr_robot1, n_years, 1) +
  + debt_table(rv$loan_robot2, input$r_robot2/100, input$n_yr_robot2, n_years, input$robot_years+1)
tbl_robot[,1] <- tbl_robot[,1]/2
colnames(tbl_robot) <- lapply(colnames(tbl_robot), function(x) { paste0("robot_",x)}) %>% unlist()

tbl_barn <- debt_table(rv$loan_housing, input$r_housing/100, input$n_yr_housing, n_years, 1)
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
#   (rv$inc_rev_total - rv$inc_exp_total + rv$inc_exp_capital_recovery)*(1+input$inflation_margin/100)^(t) +
#     + dec_exp_total * (1+input$inflation_labor)^(t)
# }) %>% unlist()

table_cash_flow$revenue_minus_expense <- lapply(c(1:n_years), function(t) {
  (rv$inc_rev_total - rv$inc_exp_total)*(1+input$inflation_margin/100)^(t-1) +
    + rv$dec_exp_total * (1+input$inflation_labor/100)^(t-1)
}) %>% unlist()

table_cash_flow$operating_income <- table_cash_flow$revenue_minus_expense + table_cash_flow$interest_total +
  + table_cash_flow$depreciation

table_cash_flow$income_tax <-  - table_cash_flow$operating_income * input$tax_rate/100 

# Insert row: year 0
table_cash_flow <- rbind(0, table_cash_flow)

# downpayments and salvage values
table_cash_flow$downpayment[1] <- -(input$down_robot1 + input$down_housing)
table_cash_flow$downpayment[(1 + input$robot_years)] <-   -  input$down_robot2 
table_cash_flow$salvage[(1 + input$robot_years)] <-  rv$salvage_robot1
table_cash_flow$salvage[(1 + 2*input$robot_years)] <- rv$salvage_robot2 + rv$salvage_housing

table_cash_flow$after_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$salvage  +
  + table_cash_flow$operating_income + table_cash_flow$income_tax + 
  + table_cash_flow$principal_total + table_cash_flow$add_back_depr

rv$table_cash_flow <- table_cash_flow
rv$table_debt <- table_debt
rv$table_depreciation <- table_depreciation

rate <- WACC()/100


rv$NPV <- npv(rate, table_cash_flow$after_tax_cash_flow[-1]) +
  + table_cash_flow$after_tax_cash_flow[1]
rv$ANPV <- -pmt(rate, rv$housing_years,rv$NPV)
rv$ANPVr <- rv$ANPV * rv$deflator
rv$IRR <- irr(table_cash_flow$after_tax_cash_flow) * 100
if (rv$IRR>200) {
  rv$IRR <- NA
} 
# change the rates for MIRR ?
rv$MIRR <- mirr(table_cash_flow$after_tax_cash_flow, input$interest/100, input$interest/100) * 100

# })

})

