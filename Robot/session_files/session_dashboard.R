

#  ------ Dashboard  -----------


  ans[[paste0(X,"_da")]] <- reactive({ 
  
    need(length(ans[[X]]$net_annual_impact_before_tax)>0, "NA") %>% validate()
    
    lapply(c("before_tax","after_tax"), function(loc_NAI) { 
    isolate({
      
      tax_factor <- (1-(loc_NAI=="after_tax")*input$tax_rate/100)
      
      if (loc_NAI=="before_tax") {
        ans[[paste0(X,"_da_",loc_NAI)]]$NAI <- ans[[X]]$net_annual_impact_before_tax
      } else {
        ans[[paste0(X,"_da_",loc_NAI)]]$NAI <- ans[[X]]$ANPV 
      }
      
      ans[[paste0(X,"_da_",loc_NAI)]]$IOFC <- (input$milk_cow_day * input$price_milk/100 - 
                                                 ans[[X]]$DMI_day * input$cost_DM )*330 * tax_factor  
      
      ans[[paste0(X,"_da_",loc_NAI)]]$IOFC2 <- (ans[[X]]$milk_day_cow_alt * input$price_milk/100 + 
                           - ans[[X]]$DMI_projected * input$cost_DM - input[[paste0("pellets",x)]] *
                           input[[paste0("cost_pellets",x)]]/2000)*330 * tax_factor    
      
      ans[[paste0(X,"_da_",loc_NAI)]]$IOFC_diff <- ans[[paste0(X,"_da_",loc_NAI)]]$IOFC2 - ans[[paste0(X,"_da_",loc_NAI)]]$IOFC
        
      ans[[paste0(X,"_da_",loc_NAI)]]$IOFC_cwt <- ans[[paste0(X,"_da_",loc_NAI)]]$IOFC /365 /input$milk_cow_day * 330 
      
      ans[[paste0(X,"_da_",loc_NAI)]]$IOFC2_cwt <- ans[[paste0(X,"_da_",loc_NAI)]]$IOFC2 /365 /ans[[X]]$milk_day_cow_alt * 330 
      
      ans[[paste0(X,"_da_",loc_NAI)]]$IOFC_diff_cwt <- ans[[paste0(X,"_da_",loc_NAI)]]$IOFC2_cwt - ans[[paste0(X,"_da_",loc_NAI)]]$IOFC_cwt
      
      ans[[paste0(X,"_da_",loc_NAI)]]$milk_current <- 
        input$herd_size * 330 * input$milk_cow_day * (input$price_milk/100 + 
                                                        +  input$scc_premium/100 * input$scc_average/1000) * tax_factor  
      
      ans[[paste0(X,"_da_",loc_NAI)]]$milk_robot <- (ans[[X]]$herd_size2 * 330 * ans[[X]]$milk_day_cow_alt *
                                (input$price_milk/100  +  input$scc_premium/100 * input$scc_average*
                                   (1-input[[paste0("scc_change",x)]]/100)/1000)) * tax_factor   
      
      ans[[paste0(X,"_da_",loc_NAI)]]$labor_current <-  (input$hr_heat_detection + input$hours_milking) * 
        input$labor_rate*365 * tax_factor   
      
      ans[[paste0(X,"_da_",loc_NAI)]]$labor_robot <- ((input[[paste0("anticipated_hours_heat",x)]] + ans[[X]]$anticipated_hours_milking) *
                                 input$labor_rate*365 +
                                 + (input[[paste0("increase_rc_mgt",x)]] - input[[paste0("decrease_lab_mgt",x)]]) *
                                 input$labor_rate_rc_mgt *365 +
                                 + input[[paste0("additional_labor",x)]] * input[[paste0("herd_increase",x)]]) * tax_factor   
      
      ans[[paste0(X,"_da_",loc_NAI)]]$feed_current <-  ans[[X]]$DMI_day * input$cost_DM * 330 * input$herd_size * tax_factor  
      
      ans[[paste0(X,"_da_",loc_NAI)]]$feed_robot <- (ans[[X]]$DMI_projected * input$cost_DM + input[[paste0("pellets",x)]] *
                                input[[paste0("cost_pellets",x)]]/2000) * 330 * ans[[X]]$herd_size2 * tax_factor   
      
      ans[[paste0(X,"_da_",loc_NAI)]]$milk_feed <-  (-(ans[[paste0(X,"_da_",loc_NAI)]]$feed_robot - ans[[paste0(X,"_da_",loc_NAI)]]$feed_current) +
                                            +  ans[[paste0(X,"_da_",loc_NAI)]]$milk_robot -   ans[[paste0(X,"_da_",loc_NAI)]]$milk_current )
      
      ans[[paste0(X,"_da_",loc_NAI)]]$labor_repair <- -(ans[[paste0(X,"_da_",loc_NAI)]]$labor_robot -ans[[paste0(X,"_da_",loc_NAI)]]$labor_current  + ans[[X]]$inc_exp_repair) 
      
      ans[[paste0(X,"_da_",loc_NAI)]]$inflation <- - pmt(ans[[X]]$avg_interest/100, ans[[X]]$planning_horizon, 
                                  npv(ans[[X]]$avg_interest/100, ans[[X]]$table_cash_flow$revenue_minus_expense[-1])) +
        - (ans[[X]]$inc_rev_total + ans[[X]]$dec_exp_total - ans[[X]]$inc_exp_total) 
      
      ans[[paste0(X,"_da_",loc_NAI)]]$capital <- -ans[[X]]$capital_cost_total + 
        +(loc_NAI=="after_tax")*(ans[[X]]$tax_interest + ans[[X]]$tax_depreciation)
      
      ans[[paste0(X,"_da_",loc_NAI)]]$misc <- ans[[paste0(X,"_da_",loc_NAI)]]$NAI - (ans[[paste0(X,"_da_",loc_NAI)]]$milk_feed + 
                            + ans[[paste0(X,"_da_",loc_NAI)]]$labor_repair + ans[[paste0(X,"_da_",loc_NAI)]]$capital + ans[[paste0(X,"_da_",loc_NAI)]]$inflation) 
      
      
      ans[[paste0(X,"_da_",loc_NAI)]]$capital_recovery_robot2 <- ans[[X]]$capital_recovery_milking +
        - (loc_NAI=="after_tax")*ans[[X]]$tax_deduction_milking
      ans[[paste0(X,"_da_",loc_NAI)]]$capital_recovery_housing2 <- ans[[X]]$capital_recovery_housing +
        - (loc_NAI=="after_tax")*ans[[X]]$tax_deduction_housing
      
      
      if (grepl("_se", X)) {  # TRUE/FALSE for sensitivity analysis
        
        dashboard_items_all <- c("NAI","IOFC","IOFC2", "IOFC_cwt", "IOFC2_cwt", 
                                 "milk_feed","labor_repair","capital","misc","inflation")
        
        lapply(dashboard_items_all, function(item) {
          # Take a difference between X (sensitivity) and x (baseline)
          ans[[paste0(X,"_da_",loc_NAI)]][[paste0("diff_",item)]] <- 
              ans[[paste0(X,"_da_",loc_NAI)]][[item]] - ans[[paste0(x,"_da_",loc_NAI)]][[item]]
#             print(item)
#             print(ans[[paste0(X,"_da_",loc_NAI)]][[item]])
#             print(ans[[paste0(x,"_da_",loc_NAI)]]) 
#             print(ans[[paste0(x,"_da_",loc_NAI)]][[item]])
        })

        ans[[paste0(X,"_da_",loc_NAI)]]$did_IOFC <-  
          ans[[paste0(X,"_da_",loc_NAI)]]$diff_IOFC2 -  ans[[paste0(X,"_da_",loc_NAI)]]$diff_IOFC
        
        ans[[paste0(X,"_da_",loc_NAI)]]$did_IOFC_cwt <-  
          ans[[paste0(X,"_da_",loc_NAI)]]$diff_IOFC2_cwt -  ans[[paste0(X,"_da_",loc_NAI)]]$diff_IOFC_cwt

      }
     })
    })
     list("before tax"=ans[[paste0(X,"_da_before_tax")]], "after tax"=ans[[paste0(X,"_da_after_tax")]])
  }) 



