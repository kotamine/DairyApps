

obeserveEvent(input$calculate_cash_flow, {

n_years <- 50;

cost_robot_1 <- rv$robot_invest
robot_salvage_1 <- input$robot_salvage 
cost_robot_2 <- rv$robot_invest*(1+input$inflation_robot)^input$robot_year
robot_salvage_2 <- input$robot_salvage*(1+input$inflation_robot)^input$robot_year

cost_housing <- rv$cost_housing
salvage_housing <- rv$salvage_housing

rv$yr_robot2 <- input$robot_years 
rv$yr_robot3 <- input$robot_years * 2
rv$loan_housing <- cost_housing - input$down_housing
rv$loan_robot1 <- cost_robot_1 - input$down_robot1
rv$loan_robot2 <- cost_robot_2 - input$down_robot2
rv$loan_robot3 <- 0 #rv$robot_invest * ()  - input$down_robot3

rv$copy_robot_salvage1 <- robot_salvage_1 
rv$copy_robot_salvage2 <- robot_salvage_2
rv$copy_robot_salvage3 <- 0 #input$robot_salvage * ()


## ---- Depreciation Table ----


yr_AGDS_robot <- 7
yr_SLD_robot <- 10

yr_AGDS_housing <- 10
yr_SLD_housing <- 15

dep_robot <- rep(0,n_years); dep_housing  <- rep(0,n_years)

## if Accelerated depreciation method is used 
if (input$dep_method=="d1") {
dep_robot[1:yr_AGDS_robot] <- vdb(cost_robot_1, robot_salvage_1,
                                  yr_AGDS_robot, factor=1.5, sequence=TRUE) 

dep_robot[(1+robot_year):yr_AGDS_robot] <-  vdb(cost_robot_2, robot_salvage_2, 
                                                yr_AGDS_robot, factor=1.5, sequence=TRUE)

dep_housing [1:yr_AGDS_housing] <- vdb(cost_housing, salvage_housing,
                                       yr_AGDS_housing, factor=1.5, sequence=TRUE) 
} else {

## if Straight line depreciation method is used 
dep_robot[1:yr_SLD_robot] <- (cost_robot_1 - robot_salvage_1)/yr_SLD_robot
dep_robot[(1+robot_year):yr_SLD_robot] <- (cost_robot_2 - robot_salvage_2)/yr_SLD_robot

dep_housing[1:yr_SLD_housing] <- (cost_housing - salvage_housing)/yr_SLD_housing

table_depreciation <- cbind(c(1:n_years),dep_robot,dep_housing)
colnames(table_depreciation) <- c("year","depreciation_robot","depreciation_housing")
table_depreciation$total <- table_depreciation$depreciation_robot + depreciation_robot$depreciation_housing 
}


## ---- Debt Table ---
loan_robot_1 <- rv$robot_invest
loan_robot_2 <- rv$robot_invest*(1+input$inflation_robot)^input$robot_years
loan_period_robot <- round(input$robot_years*.8)

loan_housing <- rv$cost_housing
loan_period_housing <- round(rv$housing_years*.8)


tbl_robot <- debt_table(loan_robot_1, input$r_robot1, loan_period_robot, n_years, 1) +
  + debt_table(loan_robot_2, input$r_robot2, loan_period_robot, n_years, input$robot_years+1)
colnames(tbl_robot) <- lapply(colnames(tbl_robot), function(x) { paste0("robot_",x)}) %>% unlist()

tbl_barn <- debt_table(loan_housing, input$r_housing, loan_period_housing, n_years, 1)
colnames(tbl_barn) <- lapply(colnames(tbl_barn), function(x) { paste0("barn_",x)}) %>% unlist()

table_debt <- cbind(tbl_robot, tbl_barn[,c(-1)])
table_debt$interest_total <- table_debt$robot_interest + table_debt$barn_interest 
table_debt$principal_total <- table_debt$robot_principal + table_debt$barn_principal



## ---- Cash Flow Table ---- 
# downpayment info + salvage value

table_cash_flow <- matrix(c(c(1:n_years), rep(rep(0,n_years),9)),ncol=10,byrow=FALSE)  %>% data.frame()
colnames(table_cash_flow) <- c("year","downpayment", "revenue_over_expense",
                               "interest_total","depreciation","operating_income",
                               "income_tax", "principal_total","add_back_depr","after_tax_cash_flow")

table_cash_flow$downpayment[1] <- input$down_robot1 + input$down_housing
table_cash_flow$downpayment[input$robot_years] <- input$robot_salvage
table_cash_flow$downpayment[1+input$robot_years] <- input$down_robot2 
table_cash_flow$downpayment[2*input$robot_years] <- input$robot_salvage * 
                              (1+input$inflation_robot)^input$robot_years
table_cash_flow$downpayment[rv$housing_years] <- input$salvage_housing

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

rv$table_cash_flow <- table_cash_flow
rv$table_debt <- table_debt
rv$table_depreciation <- table_depreciation

  rv$weighted_cost_capital <-   ((input$down_housing + input$down_robot1 + input$down_robot2) * input$hurdle_rate +
                                   + rv$loan_housing * input$r_housing + rv$loan_robot1 * input$r_robot1 + rv$loan_robot2 * input$r_robot2)/
                                (cost_housing + cost_robot_1 + cost_robot_2)
  rv$NPV <- npv(rv$weighted_cost_capital/100,table_cash_flow$after_tax_cash_flow[-1]) + table_cash_flow$after_tax_cash_flow[1]
  rv$ANPV <- -pmt(rv$weighted_cost_capital/100, rv$housing_years+1,rv$NPV)
  rv$IRR <- irr(table_cash_flow$after_tax_cash_flow)
  rv$MIRR <- mirr(table_cash_flow$after_tax_cash_flow, input$interest/100, input$interest/100)
})


cash_render_0 <- c("yr_robot2", "yr_robot3", "loan_housing",
                   "loan_robot1","loan_robot2","loan_robot3",
                   "copy_robot_salvage1","copy_robot_salvage2","copy_robot_salvage3")

cash_render_0_right <- c("NPV","ANPV")

cash_render_2_right <- c("IRR","MIRR","weighted_cost_capital")


lapply(cash_render_0, function(x) {
  rv[[x]] <- renderUI({
    rv[[x]] %>% round() %>% helpText()
  })
})

lapply(cash_render_0_right, function(x) {
  rv[[x]] <- renderUI({
    rv[[x]] %>% round() %>% helpText() %>% div(align="right")
  })
})

lapply(cash_render_2_right, function(x) {
  rv[[x]] <- renderUI({
    rv[[x]] %>% round(2) %>% helpText() %>% div(align="right")
  })
})

lapply(c("table_cash_flow","table_debt","table_depreciation"), 
       function(x) {
        output[[x]] <- DT::renderDataTable({
            if (dim(rv[[x]])[1]==0) return()
        
          DT::datatable(rv[[x]],
                        rownames = FALSE,
                        extensions = 'ColVis',
                        # extensions = 'ColReorder',
                        options = list(
                          dom = 'C<"clear">lfrtip',
                          scrollX = TRUE,
                          scrollCollapse = TRUE,
                          scrollY = 500,
                          scrollCollapse = TRUE,
                          # colVis = list(exclude = c(0, 1,1,0),
                          showNone=TRUE, 
                          activate = 'mouseover'))
          
        })

})


