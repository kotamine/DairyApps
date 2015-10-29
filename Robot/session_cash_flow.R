


n_years <- 50;


## ---- Depreciation Table ----

cost_robot_1 <- rv$robot_invest
salvage_robot_1 <- input$robot_salvage 
cost_robot_2 <- rv$robot_invest*(1+input$inflation_robot)^input$robot_year
salvage_robot_2 <- input$robot_salvage*(1+input$inflation_robot)^input$robot_year
yr_AGDS_robot <- 7
yr_SLD_robot <- 10

cost_housing <- rv$cost_housing
salvage_housing <- 0
yr_AGDS_housing <- 10
yr_SLD_housing <- 15

dep_robot <- rep(0,n_years); dep_housing  <- rep(0,n_years)
## if Accelerated depreciation method is used 
dep_robot[1:yr_AGDS_robot] <- vdb(cost_robot_1, salvage_robot_1, yr_AGDS_robot, factor=1.5, sequence=TRUE) 

dep_robot[(1+robot_year):yr_AGDS_robot] <-  vdb(cost_robot_2, salvage_robot_2, yr_AGDS_robot, factor=1.5, sequence=TRUE)

dep_housing [1:yr_AGDS_housing] <- vdb(cost_housing, salvage_housing, yr_AGDS_housing, factor=1.5, sequence=TRUE) 


## if Straight line depreciation method is used 
dep_robot[1:yr_SLD_robot] <- (cost_robot_1 - salvage_robot_1)/yr_SLD_robot
dep_robot[(1+robot_year):yr_SLD_robot] <- (cost_robot_2 - salvage_robot_2)/yr_SLD_robot

dep_housing[1:yr_SLD_housing] <- (cost_housing - salvage_housing)/yr_SLD_housing

table_depreciation <- cbind(c(1:n_years),dep_robot,dep_housing)
colnames(table_depreciation) <- c("year","depreciation_robot","depreciation_housing")
table_depreciation$total <- table_depreciation$depreciation_robot + depreciation_robot$depreciation_housing 




## ---- Debt Table ---
# interest rate info 
loan_robot_1 <- rv$robot_invest
loan_robot_2 <- rv$robot_invest*(1+input$inflation_robot)^input$robot_year
loan_period_robot <- round(input$robot_year*.8)

loan_housing <- rv$cost_housing
loan_period_housing <- round(rv$housing_year*.8)


tbl_robot <- debt_table(loan_robot_1, interest_rate, loan_period_robot, n_years , 1) +
  + debt_table(loan_robot_2, interest_rate, loan_period_robot, n_years, robot_year+1)
colnames(tbl_robot) <- lapply(colnames(tbl_robot), function(x) { paste0("robot_",x)}) %>% unlist()

tbl_barn <- debt_table(loan_housing, interest_rate, loan_period_housing, n_years, 1)
colnames(tbl_barn) <- lapply(colnames(tbl_barn), function(x) { paste0("barn_",x)}) %>% unlist()

table_debt <- cbind(tbl_robot, tbl_barn[,c(-1)])
table_debt$interest_total <- table_debt$robot_interest + table_debt$barn_interest 
table_debt$principal_total <- table_debt$robot_principal + table_debt$barn_principal



## ---- Cash Flow Table ---- 
# downpayment info + salvage value
# tax rate

table_cash_flow <- matrix(c(c(1:n_years), rep(rep(0,n_years),9)),ncol=10,byrow=FALSE)  %>% data.frame()
colnames(table_cash_flow) <- c("year","downpayment", "revenue_over_expense", "interest_total","depreciation","operating_income",
                               "income_tax", "principal_total","add_back_depr","after_tax_cash_flow")

table_cash_flow$interest_total <- table_debt$interest_total
table_cash_flow$principal_total <- table_debt$principal_total
table_cash_flow$depreciation <- table_depreciation$total
table_cash_flow$add_back_depr <-  -table_depreciation$total

table_cash_flow$revenue_over_expense <- lapply(c(1:n_years), function(t) {
  (rv$inc_rev_total - rv$inc_exp_total + rv$inc_exp_capital_recovery)*(1+input$inflation_margin/100)^(t-1) +
    + dec_exp_total * (1+input$inflation_labor)^(t-1)
}) %>% unlist()

table_cash_flow$operating_income <- table_cash_flow$revenue_over_expense + table_cash_flow$interest_total +
  + table_cash_flow$depreciation

table_cash_flow$income_tax <- table_cash_flow$operating_income * input$tax_rate 
table_cash_flow$after_tax_cash_flow <- table_cash_flow$downpayment + table_cash_flow$operating_income +
  + table_cash_flow$income_tax + table_cash_flow$principal_total + table_cash_flow$add_back_depr




