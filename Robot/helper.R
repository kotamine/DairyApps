

zeros <- function(r,c) matrix(c(mat.or.vec(r,c)),nrow=r,ncol=c) 
ones <- function(r,c) matrix(c(rep(1,r*c)),nrow=r,ncol=c) 
nulls <- function(r,c) matrix(c(rep(NA,r*c)),nrow=r,ncol=c)

my_add_row <- function(A,a) {
  # A: data.frame
  # a: row vector that is going to be appended to A   
  A <- as.matrix(A)
  A <- rbind(A,a)
  data.frame(A)
}


formatcomma <- function(x) {
  format(x, big.mark=",", scientific=FALSE) 
} 

formatdollar <- function(x,digit=0) {
  #if (is.na(x)) { return(NA) }
  if (length(x)==0) { return(NA) }
  #if (is.null(x)) { return(NA) }
    if (x>=0) {
      paste0("$",x %>% round(digit) %>% formatcomma()) 
    } else {
      paste0("-$",-x %>% round(digit) %>% formatcomma()) 
    }
}

formatdollar2 <- function(x,digit=0) {
  if (is.na(x)) { return(NA) }
  if (is.null(x)) { return(NA) }
  if (x>=0) {
    paste0("+$",x %>% round(digit) %>% formatcomma()) 
  } else {
    paste0("-$",-x %>% round(digit) %>% formatcomma()) 
  }
}

formatdollar2b <- function(x,digit=0) {
  if (is.na(x)) { return(NA) }
  if (is.null(x)) { return(NA) }
  if (x>=0) {
      paste0("( +$",x %>% round(digit) %>% formatcomma()," )") 
    } else {
      paste0("( -$",-x %>% round(digit) %>% formatcomma()," )") 
    }
}


# --- Common Excel Financial Functions  ---
# \url{http://cvs.moodle.org/contrib/patches/question_calculated_extended/calculated/packages/financial/financial_class.php?view=co}
# @author Enrique Garcia M. \email{egarcia@@egm.as}
# @author Karsten W. \email{k.weinert@@gmx.net}

npv <- function(rate, values) sum(values / (1 + rate)^seq_along(values))

irr <- function(x, start=0.1) {
  t <- seq_along(x)-1
  f <- function(i) abs(sum(x/(1+i)^t))
  return(nlm(f,start)$estimate)
}

fv <- function(rate, nper, pmt, pv = 0.0, type = 0) {
  pvif <- (1+rate)^nper # Present value interest factor
  fvifa <- if(rate==0) nper else ((1+rate)^nper - 1) / rate
  return(-((pv * pvif) + pmt * (1.0 + rate * type) * fvifa))
}

pv <- function(rate, nper, pmt, fv = 0.0, type = 0) {
  pvif <- (1+rate)^nper # Present value interest factor
  fvifa <- if(rate==0) nper else ((1+rate)^nper - 1) / rate
  return((-fv - pmt * (1.0 + rate * type) * fvifa) / pvif)
}

pmt <- function(rate, nper, pv, fv=0, type=0) {
  rr <- 1/(1+rate)^nper
  res <- (-pv-fv*rr)*rate/(1-rr)
  return(res/(1+rate*type))
}

# additional functions
mirr <- function(values, finance_rate, reinvest_rate) {
  n <- length(values)
  tmp <- npv(reinvest_rate,values)*(1+reinvest_rate)^n/
    npv(finance_rate,values)*(1+finance_rate)
  return(tmp^(1/(n-1))-1)
}

# annualized net present value
# need: library(dplyr) for %>% operation
anpv <- function(x, r, i, nper) {
  x_seq <- lapply(c(1:nper), function(t) { x*(1+i)^(t) }) %>% unlist()
  -pmt(r, nper, npv(r, x_seq))
}



# http://www.experts-exchange.com/articles/1948/A-Guide-to-the-PMT-FV-IPMT-and-PPMT-Functions.html
impt <- function(rate, per, nper, pv, fv=0, type=0) {
  
  pmt <- pmt(rate, nper, pv, fv, type)
  fv(rate, per-1, pmt, pv, type) * rate
}

# http://www.excel-easy.com/examples/depreciation.html
vdb <- function(cost, salvage, nper, start_per=1, end_per=1, factor=2, switch=TRUE, sequence=FALSE) {
  v <- c(); vsum <- c(); 
  s0 <- (cost-salvage)/nper;  s <- s0
  tmp <- cost * factor/nper
  v[1] <- (tmp>s) * tmp + (tmp<=s) * s 
  vsum <- v[1]; 
  for (t in 2:nper) {
    s <-  (cost - vsum[t-1] - salvage)/(nper - t + 1)
    tmp <-  (cost - vsum[t-1]) * factor/nper
    v[t] <- switch * ((tmp>s) * tmp + (tmp<=s) * s ) + (!switch) * tmp
    vsum[t] <- vsum[t-1] + v[t]
  }
  if (!sequence)  { 
    return(sum(v[start_per:end_per])) 
  } else {
    return(v)
  }
}




debt_table <- function(loan, interest_rate, loan_period,
                       n_period, starting_year=1) {
  
  loan_period <- round(loan_period,0)
  n_period <- round(n_period,0)
  starting_year <- round(starting_year,0)
  
  pmt <- -pmt(interest_rate, loan_period, loan)
  interest <- impt(interest_rate, 1, loan_period, loan)
  principal <- pmt - interest
  
  df <- data.frame(year=c(1:n_period), yr_pmt =rep(0,n_period))
  
  ending_year <- loan_period + starting_year - 1
  df$yr_pmt[starting_year:ending_year] <- c(1:loan_period)
  
  df$interest <-  (df$yr_pmt >0) *
    (-1) * impt(interest_rate, df$yr_pmt, loan_period, loan) 
  
  df$principal <- (df$yr_pmt >0) * (pmt - df$interest) 
  
  return(df)
}



#---


# ----- dashboard features -----

dash_IOFC <- function(IOFC, IOFC2, basis,
                      milk_cow_day, milk_change, cutoffs=NULL,
                      compare=NULL, compare2=NULL) {
  validate( 
    need(!(is.na(IOFC) | is.na(IOFC2)), 
                 "NA")
    ) 
  
  if (basis=="per cow")
  {  if (is.null(cutoffs)) { cutoffs <- c(663,331) } 
    digit <- 0
    IOFC_unit <- "IOFC ($/cow/year)"
  }
  else {
    if (is.null(cutoffs)) { cutoffs <- c(8,4) } 
    digit <- 2
    IOFC_unit <- "IOFC ($/cwt)"
  }
  diff <- IOFC2 - IOFC
  
  if (IOFC > cutoffs[1]) { 
    style <- "background-color: #3EA055; color:white;"
  } 
  else if (IOFC > cutoffs[2]) {
    style <-  "background-color: #FFA62F; color:white;" 
  } else {
    style <-  "background-color: #F70D1A; color:white;" 
  }
  
  if (is.null(compare)) {
    div(class="well", style=style,  align="center",
        diff  %>% formatdollar2(digit) %>% strong() %>% h3(),
        h5(IOFC_unit), h5("under robot"))
  } 
  else {
    diff1 <- compare2 - compare 
    diff2 <-  diff - diff1
    div(class="well", style=style,  align="center",
        diff  %>% formatdollar2(digit) %>% strong() %>% h3(),
        diff2 %>% formatdollar2b(digit) %>% strong() %>% h4())
  }
}



dash_NAI <- function(NAI,cutoff=0, compare=NULL) {

  validate( 
    need(!is.na(NAI), 
         "NA")
  )     
  
  if (NAI>cutoff) { 
    style <- "background-color: #306EFF; color:white;"
  } 
  else {
    style <-  "background-color: #F70D1A; color:white;" 
  }
  
  if (is.null(compare)) {
    div(class="well", style=style, align="center",
        NAI %>% formatdollar2() %>% strong() %>% h3(),
        h5("Net Impact ($/year)"),h5("under robot"))
  } 
  else {
    diff <- NAI - compare
    div(class="well", style=style, align="center",
        NAI %>% formatdollar2() %>% strong() %>% h3(),
        diff %>% formatdollar2b() %>% strong() %>% h4())
  }
}


dash_plot1 <- function(feed_current,feed_robot,milk_current,milk_robot) { 

  validate( 
    need(!(is.na(feed_current) | is.na(feed_robot) | 
             is.na(milk_current) | is.na(milk_robot)), 
         "NA")
  )      
  
  a <- data.frame("vars"=c("feed","feed","milk", "milk"), 
                  "values"=c(feed_current,feed_robot,milk_current,milk_robot)/1000,"type"= c(0,1,0,1)) 
  a$label <- apply(cbind(a$values),2,round,0)
  a$label <- apply(a$label, 2,formatcomma) 
  a$label <- apply(a$label, 2, function(x) { paste0("$", x,"k") })
  
  ggplot(data=a, aes(x=vars, y=values, fill= factor(type))) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    #   ggtitle("Milk vs Feed")+ 
    geom_text(aes(label=label, ymax=max(values)*1.1), 
              vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=5) +
    scale_fill_brewer(palette="Paired", breaks=c(1,0), labels=c("Robots","Current")) +
    theme_minimal() +
    scale_x_discrete(
      limits=c("feed","milk"),   
      labels=c("Feed \n Cost","Milk \n Income")    
    ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(),  #removes y-axis label
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      text=element_text(family="sans", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
      legend.title=element_blank(), 
      legend.position=c(0.85,0.10)
    )
}

dash_plot2 <- function(inc_exp_repair,labor_current,labor_robot) {
  
  validate( 
    need(!(is.na(inc_exp_repair) | is.na(labor_current) | 
              is.na(labor_robot)), 
         "NA")
  )     
  
  a <- data.frame("vars"=c("repair","repair","labor", "labor"), 
                  "values"=c(0,inc_exp_repair,labor_current,labor_robot)/1000,"type"= c(0,1,0,1)) 
  
  a$label <- apply(cbind(a$values),2,round,0)
  a$label <- apply(a$label, 2,formatcomma) 
  a$label <- apply(a$label, 2, function(x) { paste0("$", x,"k") })
  
  ggplot(data=a, aes(x=vars, y=values, fill= factor(type))) + 
    geom_bar(stat="identity", position=position_dodge()) +
    coord_flip() +
    #   ggtitle("Labor vs Repair") + 
    geom_text(aes(label=label, ymax=max(values)*1.1), 
              vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=5) +
    scale_fill_brewer(palette="Reds", breaks=c(1,0), labels=c("Robots","Current")) +
    theme_minimal() +
    scale_x_discrete(
      limits=c( "repair", "labor"),   
      labels=c(" Additional \n Repair \n Cost", "Labor \n Cost")    
    ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(),  #removes y-axis label
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      text=element_text(family="sans", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
      legend.title=element_blank(), 
      legend.position=c(0.85,0.10)
    )
}

dash_plot3 <- function(inc_exp_capital_recovery,capital_recovery_housing,
                       robot_end_PV, input_NAI) { 
  
  validate( 
    need(!(is.na(inc_exp_capital_recovery) | is.na(capital_recovery_housing) | 
             is.na(robot_end_PV)), 
         "NA")
  )
  
  if (input_NAI=="w/o housing") {
    capital_recovery_housing_show <- 0
    robot_end_PV_show <- 0
  } else if (input_NAI=="w/ housing") {
    capital_recovery_housing_show <- capital_recovery_housing
    robot_end_PV_show <- 0
  } else {
    capital_recovery_housing_show <- capital_recovery_housing
    robot_end_PV_show <- robot_end_PV
  }
  a <- data.frame("vars"=c("capital_robot","capital_housing","robot_end_PV"), 
                  "values"=c(inc_exp_capital_recovery,capital_recovery_housing_show,robot_end_PV_show)/1000,
                  "values_shadow"=c(inc_exp_capital_recovery,capital_recovery_housing,robot_end_PV)/1000,
                  "type"= c(1,1,1))  
  
  a$label <- apply(cbind(a$values),2,round,0)
  a$label <- apply(a$label, 2,formatcomma) 
  a$label <- apply(a$label, 2, function(x) { paste0("$", x,"k") })
  
  ggplot(data=a, aes(x=vars, y=values, fill=factor(type))) + 
    geom_bar(stat="identity", position=position_dodge(),width=0.7, fill="seagreen3") +
    coord_flip() +
    # ggtitle("Cost of Capital") + 
    geom_text(aes(label=label,ymax=max(values_shadow)*1.0), 
              vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=5) +
    theme_minimal() + 
    scale_x_discrete(
      limits=c("robot_end_PV", "capital_housing","capital_robot"),   
      labels=c("Robot \n Salvage \n PV", 
               "Housing \n Capital \n Recovery \n Cost","Robot \n Capital\n Recovery \n Cost")    
    ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.title.y=element_blank(),  #removes y-axis label
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      text=element_text(family="sans", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0))  #changes font face and location for graph title
    )
}





