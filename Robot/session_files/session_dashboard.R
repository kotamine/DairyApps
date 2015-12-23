

#  ------ Dashboard  -----------
# Triggered by any change in ans[[x]]$net_annual_impact_before_tax 


lapply(base_profiles, function(x) {

  ans[[paste0(x,"_da")]] <- reactive({ 
  
    need(length(ans[[x]]$net_annual_impact_before_tax)>0, "NA") %>% validate()
    
    browser()
    lapply(c("before_tax","after_tax"), function(loc_NAI) { 
    isolate({
      
      tax_factor <- (1-(loc_NAI=="after_tax")*input$tax_rate/100)
      
      if (loc_NAI=="before_tax") {
        ans[[paste0(x,"_da_",loc_NAI)]]$NAI <- ans[[x]]$net_annual_impact_before_tax
      } else {
        ans[[paste0(x,"_da_",loc_NAI)]]$NAI <- ans[[x]]$ANPV 
      }
      
      ans[[paste0(x,"_da_",loc_NAI)]]$IOFC <- (input$milk_cow_day * input$price_milk/100 - 
                                                 ans[[x]]$DMI_day * input$cost_DM )*330 * tax_factor  
      
      ans[[paste0(x,"_da_",loc_NAI)]]$IOFC2 <- (ans[[x]]$milk_day_cow_alt * input$price_milk/100 + 
                           - ans[[x]]$DMI_projected * input$cost_DM - input[[paste0("pellets",x)]] *
                           input[[paste0("cost_pellets",x)]]/2000)*330 * tax_factor    
      
      ans[[paste0(x,"_da_",loc_NAI)]]$IOFC_diff <- ans[[paste0(x,"_da_",loc_NAI)]]$IOFC2 - ans[[paste0(x,"_da_",loc_NAI)]]$IOFC
        
      ans[[paste0(x,"_da_",loc_NAI)]]$IOFC_cwt <- ans[[paste0(x,"_da_",loc_NAI)]]$IOFC /365 /input$milk_cow_day * 330 
      
      ans[[paste0(x,"_da_",loc_NAI)]]$IOFC2_cwt <- ans[[paste0(x,"_da_",loc_NAI)]]$IOFC2 /365 /ans[[x]]$milk_day_cow_alt * 330 
      
      ans[[paste0(x,"_da_",loc_NAI)]]$IOFC_diff_cwt <- ans[[paste0(x,"_da_",loc_NAI)]]$IOFC2_cwt - ans[[paste0(x,"_da_",loc_NAI)]]$IOFC_cwt
      
      ans[[paste0(x,"_da_",loc_NAI)]]$milk_current <- 
        input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                        +  input$scc_premium/100 * input$scc_average/1000) * tax_factor  
      
      ans[[paste0(x,"_da_",loc_NAI)]]$milk_robot <- (ans[[x]]$herd_size2 * 330 * ans[[x]]$milk_day_cow_alt *
                                (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*
                                   (1-input[[paste0("scc_change",x)]]/100)/1000)) * tax_factor   
      
      ans[[paste0(x,"_da_",loc_NAI)]]$labor_current <-  (input$hr_heat_detection + input$hours_milking) * 
        input$labor_rate*365 * tax_factor   
      
      ans[[paste0(x,"_da_",loc_NAI)]]$labor_robot <- ((input[[paste0("anticipated_hours_heat",x)]] + ans[[x]]$anticipated_hours_milking) *
                                 input$labor_rate*365 +
                                 + (input[[paste0("increase_rc_mgt",x)]] - input[[paste0("decrease_lab_mgt",x)]]) *
                                 input$labor_rate_rc_mgt *365 +
                                 + input[[paste0("additional_labor",x)]] * input[[paste0("herd_increase",x)]]) * tax_factor   
      
      ans[[paste0(x,"_da_",loc_NAI)]]$feed_current <-  ans[[x]]$DMI_day * input$cost_DM * 330 * input$herd_size * tax_factor  
      
      ans[[paste0(x,"_da_",loc_NAI)]]$feed_robot <- (ans[[x]]$DMI_projected * input$cost_DM + input[[paste0("pellets",x)]] *
                                input[[paste0("cost_pellets",x)]]/2000) * 330 * ans[[x]]$herd_size2 * tax_factor   
      
      ans[[paste0(x,"_da_",loc_NAI)]]$milk_feed <-  (-(ans[[paste0(x,"_da_",loc_NAI)]]$feed_robot - ans[[paste0(x,"_da_",loc_NAI)]]$feed_current) +
                                            +  ans[[paste0(x,"_da_",loc_NAI)]]$milk_robot -   ans[[paste0(x,"_da_",loc_NAI)]]$milk_current )
      
      ans[[paste0(x,"_da_",loc_NAI)]]$labor_repair <- -(ans[[paste0(x,"_da_",loc_NAI)]]$labor_robot -ans[[paste0(x,"_da_",loc_NAI)]]$labor_current  + ans[[x]]$inc_exp_repair) 
      
      ans[[paste0(x,"_da_",loc_NAI)]]$inflation <- - pmt(ans[[x]]$avg_interest/100, ans[[x]]$planning_horizon, 
                                  npv(ans[[x]]$avg_interest/100, ans[[x]]$table_cash_flow$revenue_minus_expense[-1])) +
        - (ans[[x]]$inc_rev_total + ans[[x]]$dec_exp_total - ans[[x]]$inc_exp_total) 
      
      ans[[paste0(x,"_da_",loc_NAI)]]$capital <- -ans[[x]]$capital_cost_total + 
        +(loc_NAI=="after_tax")*(ans[[x]]$tax_interest + ans[[x]]$tax_depreciation)
      
      ans[[paste0(x,"_da_",loc_NAI)]]$misc <- ans[[paste0(x,"_da_",loc_NAI)]]$NAI - (ans[[paste0(x,"_da_",loc_NAI)]]$milk_feed + 
                            + ans[[paste0(x,"_da_",loc_NAI)]]$labor_repair + ans[[paste0(x,"_da_",loc_NAI)]]$capital + ans[[paste0(x,"_da_",loc_NAI)]]$inflation) 
      
      
      ans[[paste0(x,"_da_",loc_NAI)]]$capital_recovery_robot2 <- ans[[x]]$capital_recovery_milking +
        - (loc_NAI=="after_tax")*ans[[x]]$tax_deduction_milking
      ans[[paste0(x,"_da_",loc_NAI)]]$capital_recovery_housing2 <- ans[[x]]$capital_recovery_housing +
        - (loc_NAI=="after_tax")*ans[[x]]$tax_deduction_housing
      
     })
    })
     list("before tax"=ans[[paste0(x,"_da_before_tax")]], "after tax"=ans[[paste0(x,"_da_after_tax")]])
  }) 

}) 



