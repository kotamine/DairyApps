

#  
# inputvar_format <- function(var, format, unit, ...) {
#   do.call(format, list(input[[var]], ...)) %>% paste(unit)
# }

# 
# gen_sensitivity_vars <- function(i, x, varname, var0, var1, slider_ini) {
#   output[[paste0("sensitivity_vars",i,x)]] <- renderUI({
#     browser()
#     
#     input[[var]]
#     input[[paste0("sensitivity_slider",i)]]
#     
#     div( helpText(varname), 
#          helpText("From:", var0), 
#          helpText("To:", var1), 
#          sliderInput(paste0("sensitivity_slider",i),NULL,min=-50, max=50, step=10, value = slider_ini), 
#          actionButton(paste0("sensitivity_action",i), "Details")
#     )  
#   })
# }

sensitivity_labels <- c("Change in Milk Output","Milking/Chore Labor Wage", "Inflation: Robot/Parlor",
                           "Inflation: IOFC Margin","Inflation: Labor Wage")
sensitivity_format <- c("formatcomma", "formatdollar_2d", "round_pct", "round_pct", "round_pct")
sensitivity_unit <- c("lb", "", "", "", "")
sensitivity_slider_ini <- c(20, 20, 20, -20, 20) 

lapply(base_profiles, function(x) { 
  
sensitivity_vars <- c(paste0("milk_change",x),"labor_rate", "inflation_robot","inflation_margin","inflation_labor")

lapply(seq_along(sensitivity_vars), function(i) {
  output[[paste0("sensitivity_vars",i,x)]] <- renderUI({
    
    browser()
    
    var <- input[[sensitivity_vars[i]]]
    input[[paste0("sensitivity_slider",i)]]
    
    var0 <- do.call(sensitivity_format[i], list(var)) %>% paste(sensitivity_unit[i])
    var1 <- do.call(sensitivity_format[i], list(var * (1 + input[[paste0("sensitivity_slider",i)]]/100))) %>% 
      paste(sensitivity_unit[i])
    if (is.null(input[[paste0("sensitivity_slider",i)]])) {
      value <- sensitivity_slider_ini[i]
    } else {
     value <- input[[paste0("sensitivity_slider",i)]]
    }
    
    div( div(style="background-color:gray; color:white;",
             h5(sensitivity_labels[i], align="center")), 
         helpText("From:", var0, align="right"), 
         helpText("To:", var1, align="right"), 
         sliderInput(paste0("sensitivity_slider",i), "% Change",min=-50, max=50, step=10, value = value), 
         div(actionButton(paste0("sensitivity_action",i), "Details"), align=center) 
    )  
  }) 
})

# var0_list <- Map(inputvar_format, sensitivity_vars, sensitivity_format, sensitivity_unit) %>% unlist()
# var1_list <- Map(inputvar_format, sensitivity_vars, sensitivity_format, sensitivity_unit) %>% unlist()

# Map(gen_sensitivity_vars, seq_along(sensitivity_vars), x, sensitivity_vars, sensitivity_varlabels, 
#     var0_list, var1_list, sensitivity_slider_ini)

})


