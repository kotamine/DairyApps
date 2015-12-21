

# The following inputs are replaced by profile-specific variables. 
# e.g. input$herd_increaseRobots, input$herd_increaseRetrofit, input$herd_increaseNew
#   c("herd_increase", "repair","insurance_rate","hr_sv_milking", 
#     "anticipated_hours_heat","increase_rc_mgt",
#     "decrease_lab_mgt", "milk_change","scc_change","software",
#     "pellets","cost_pellets","change_turnover","change_electricity",
#     "change_water", "change_chemical",
#     "cost_housing_cow",
#     "down_housing", "down_milking1", "down_milking2",
#     "n_yr_housing", "n_yr_milking1","n_yr_milking2" ,
#     "salvage_housing", "salvage_milking1", 
#     "milking_years", "cost_parlors", "cost_robot", "robot_years", "n_robot")
#     
#       and input$NAI

#  ------ Dashboard Features -----------



#  ------ Dashboard  triggered by any change in; -----------
# input$NAI
# ans[[x]]$net_annual_impact_after_tax
lapply(base_profiles, function(x) {
  
  
  observe({
    
    need(!is.na(ans[[x]]$net_annual_impact_after_tax)) %>% validate()
    
    browser()
    
    input$NAI
    ans[[x]]$net_annual_impact_after_tax
    
    isolate({
      
      tax_factor <- (1-(input$NAI=="after tax")*input$tax_rate/100)
      
      if (input$NAI=="before tax") {
        ans[[paste0(x,"_da")]]$NAI <- ans[[x]]$net_annual_impact_before_tax
      } else {
        ans[[paste0(x,"_da")]]$NAI <- ans[[x]]$net_annual_impact_after_tax
      }
      
      ans[[paste0(x,"_da")]]$IOFC <- (input$milk_cow_day * input$price_milk/100 - ans[[x]]$DMI_day * input$cost_DM )*330 * tax_factor  
      
      ans[[paste0(x,"_da")]]$IOFC2 <- (ans[[x]]$milk_day_cow_alt * input$price_milk/100 + 
                           - ans[[x]]$DMI_projected * input$cost_DM - input[[paste0("pellets",x)]] *
                           input[[paste0("cost_pellets",x)]]/2000)*330 * tax_factor    
      
      ans[[paste0(x,"_da")]]$IOFC_cwt <- ans[[x]]$IOFC /365 /input$milk_cow_day * 330 
      
      ans[[paste0(x,"_da")]]$IOFC2_cwt <- ans[[x]]$IOFC2 /365 /ans[[x]]$milk_day_cow_alt * 330 
      
      ans[[paste0(x,"_da")]]$milk_current <- 
        input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                        +  input$scc_premium/100 * input$scc_average/1000) * tax_factor  
      
      ans[[paste0(x,"_da")]]$milk_robot <- (ans[[x]]$herd_size2 * 330 * ans[[x]]$milk_day_cow_alt *
                                (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*
                                   (1-input[[paste0("scc_change",x)]]/100)/1000)) * tax_factor   
      
      ans[[paste0(x,"_da")]]$labor_current <-  (input$hr_heat_detection + input$hours_milking) * 
        input$labor_rate*365 * tax_factor   
      
      ans[[paste0(x,"_da")]]$labor_robot <- ((input[[paste0("anticipated_hours_heat",x)]] + ans[[x]]$anticipated_hours_milking) *
                                 input$labor_rate*365 +
                                 + (input[[paste0("increase_rc_mgt",x)]] - input[[paste0("decrease_lab_mgt",x)]]) *
                                 input$labor_rate_rc_mgt *365 +
                                 + input$additional_labor * input[[paste0("herd_increase",x)]]) * tax_factor   
      
      ans[[paste0(x,"_da")]]$feed_current <-  ans[[x]]$DMI_day * input$cost_DM * 330 * input$herd_size * tax_factor  
      
      ans[[paste0(x,"_da")]]$feed_robot <- (ans[[x]]$DMI_projected * input$cost_DM + input[[paste0("pellets",x)]] *
                                input[[paste0("cost_pellets",x)]]/2000) * 330 * ans[[x]]$herd_size2 * tax_factor   
      
      ans[[paste0(x,"_da")]]$milk_feed <-  (-(ans[[paste0(x,"_da")]]$feed_robot - ans[[paste0(x,"_da")]]$feed_current) +
                                            +  ans[[paste0(x,"_da")]]$milk_robot -   ans[[paste0(x,"_da")]]$milk_current )
      
      ans[[paste0(x,"_da")]]$labor_repair <- -(ans[[paste0(x,"_da")]]$labor_robot -ans[[paste0(x,"_da")]]$labor_current  + ans[[x]]$inc_exp_repair) 
      
      ans[[paste0(x,"_da")]]$inflation <- - pmt(input$interest/100, ans[[x]]$planning_horizon, 
                                  npv(input$interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
        - (ans[[x]]$inc_rev_total + ans[[x]]$dec_exp_total - ans[[x]]$inc_exp_total) 
      
      ans[[paste0(x,"_da")]]$capital <- -ans[[x]]$capital_cost_total + 
        +(input$NAI=="after tax")*(ans[[x]]$tax_interest + ans[[x]]$tax_depreciation)
      
      ans[[paste0(x,"_da")]]$misc <- ans[[paste0(x,"_da")]]$NAI - (ans[[paste0(x,"_da")]]$milk_feed + 
                            + ans[[paste0(x,"_da")]]$labor_repair + ans[[paste0(x,"_da")]]$capital + ans[[paste0(x,"_da")]]$inflation) 
      
      ans[[paste0(x,"_da")]]$capital_recovery_robot2 <- ans[[x]]$capital_recovery_robot +
        - (input$NAI=="after tax")*ans[[x]]$tax_deduction_milking
      ans[[paste0(x,"_da")]]$capital_recovery_housing2 <- ans[[x]]$capital_recovery_housing +
        - (input$NAI=="after tax")*ans[[x]]$tax_deduction_housing
      
    })
  })
  
})



