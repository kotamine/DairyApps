

library(ggplot2)
library(scales)


a <- data.frame("vars"=c("v1","v1","v2", "v2"), "values"=c(1.2,2.1,1.4,1.8),"type"= c(0,1,0,1)) 

ggplot(data=a, aes(x=vars, y=values, fill= factor(type))) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() + 
  # ggtitle("Positive Impacts")+ 
  geom_text(aes(label=values,ymax=max(values)*1.05), vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=4) +
  scale_fill_brewer(palette="Paired", breaks=c(1,0), labels=c("Robots","Current")) +
  theme_minimal() +
  scale_x_discrete(
    limits=c("v1","v2"),   
    labels=c("Milk \n income"," Labor \n savings")    
  ) + 
  theme(
    axis.title.x=element_blank(), 
    axis.text.x = element_text(hjust=0.5), 
    axis.title.y=element_blank(),  #removes y-axis label
    text=element_text(family="serif", size=12),                       #changes font on entire graph
    plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
    legend.title=element_blank(), 
    legend.position=c(-0.05,-0.05)
  )



aa <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 

ggplot(data=aa, aes(x=vars, y=values, fill=factor(type))) + 
  geom_bar(stat="identity", position=position_dodge(),width=0.7, fill="seagreen3") +
  coord_flip() +
  geom_text(aes(label=values), vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=4) +
  theme_minimal() + 
  scale_x_discrete(
    limits=c("v1","v2"),   
    labels=c("var \n 1"," var \n 2")    
  ) + 
    theme(
      axis.title.x=element_blank(), 
      axis.text.x = element_text(hjust=0.5), 
      axis.title.y=element_blank(),  #removes y-axis label
      text=element_text(family="serif", size=14),                       #changes font on entire graph
      plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
      legend.title=element_blank(), 
      legend.position=c(-0.05,-0.05)
    )


aa$label <- apply(cbind(aa$values),2,round,1)
aa$label <- apply(aa$label, 2,formatcomma) 
aa$label <- apply(aa$label, 2, function(x) { paste0("$", x) })

ggplot(data=aa, aes(x=vars, y=values, fill=factor(type))) + 
  geom_bar(stat="identity", position=position_dodge(),width=0.7, fill="seagreen3") +
  coord_flip() +
  ggtitle("Cost of capital") + 
  geom_text(aes(label=label,ymax=max(values)*1.05), 
            vjust=0.5, hjust=1.2, color="white", position = position_dodge(0.9), size=4) +
  theme_minimal() + 
  scale_x_discrete(
    limits=c("v1","v2"),   
    labels=c("Housing \n recovery","Robot \n recovery")    
  ) + 
  theme(
    axis.title.x= element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(hjust=0.5), 
    axis.title.y=element_blank(),  #removes y-axis label
    text=element_text(family="sans", size=14),                       #changes font on entire graph
    plot.title=element_text(face="bold",hjust=c(0,0)),  #changes font face and location for graph title
    legend.title=element_blank(), 
    legend.position=c(-0.05,-0.05)
  )



ggplot(data=a, aes(x=vars, y=values, fill=type)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  coord_flip() +
  geom_text(aes(label=values), vjust=.5, hjust=.5, color="white", size=3) +
  scale_fill_brewer(palette="Reds") + theme_minimal()



A <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 
A <- A[FALSE,]
colnames(A)
A
A <- rbind(A,c("v1",2,0))
A

A <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 
A <- as.matrix(A)

A <- nulls(1,3) 
A <- A[FALSE,]
colnames(A)
A
A <- rbind(A,c("v1",2,0))
A
A <- data.frame(A)
A

my_add_row <- function(A,a) {
# A: data.frame
# a: row vector that is going to be appended to A   
  A <- as.matrix(A)
  A <- rbind(A,a)
  data.frame(A)
}



A <- nulls(1,3)
colnames(A) <- c("vars","values","type")
A <- A[FALSE,]
A
A <- rbind(A,c("v1",2,0))


A
A <- data.frame(A)
A

A <- data.frame("vars"=c("v1","v2"), "values"=c(1.2,1.4),"type"= c(1,1)) 
A <- A[FALSE,]
A <- as.matrix(A)
A
A <- rbind(A,c("v1",2,0))


df <- data.frame(Doubles=double(),
                 Ints=integer(),
                 Factors=factor(),
                 Logicals=logical(),
                 Characters=character(),
                 stringsAsFactors=FALSE)

A <- data.frame( vars=character(),
                 values=double(),
                 type=factor())
A <- rbind(A,a)
a <- c("v1",2,0)


A <- data.frame(Column1 = numeric(0), Column2 = numeric(0), Column3=character())
A


A <- data.frame(Column1 = numeric(0))
A
a <- matrix(c(2,0.1113,"B"),nrow=1)
colnames(a) <- c("v1","v2","v3")
A <- rbind(A,a)
A <- rbind(A,a)
A

A <- apply(A[,-3],2,as.numeric)
apply(A,2,round,0)


b <- matrix(c(4,5),nrow=1)
# colnames(b) <- c("v1","v3")
A <- rbind(A,b)
A



A <- data.frame(Row1 = numeric(0))
A
a <- matrix(c(2,0),ncol=1)
rownames(a) <- c("v1","v2")
A <- cbind(A,a)


a <- matrix(c(2,0.113,"abc"),nrow=1)
apply(a,2,round, 2)

apply(a, 2, is.numeric)

apply(a[is.numeric(a)],2,round, 2)



list1 <- c("a","b","c")

A <- list()
lapply(list1, 
       function(x) {
         str <- paste0("A$",x, " <- paste(' ",
                       x, "1')")
         eval(parse(text=str))
       }
)




library(shiny)
runApp(list(
  ui = fluidPage(lapply(1:10, function(i) {
    uiOutput(paste0('x', i))
  })),
  server = function(input, output) {
    lapply(1:10, function(i) {
      output[[paste0('x', i)]] = renderUI(paste('Hi, this is', i))
    })
  }
)) 



x <- c(1:10)

paste("x")


A <- data.frame(Column1 = numeric(0))
A
a <- matrix(c(2,0),nrow=1)
colnames(a) <- c("v1","v2")


loan <- 360000
interest_rate <- 5/100
loan_period <- 12
robot_year <- 15
robot_cycle <- 2

loan2 <- 350000


loan_housing <- 1040000
loan_period_housing <- 24
housing_year <- 30



debt_table <- function(loan, interest_rate, loan_period,
                        n_period, starting_year=1) {
  
  loan_period <- round(loan_period)
  n_period <- round(n_period)
  starting_year <- round(starting_year)
  
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

debt_table(loan, interest_rate, loan_period, 50, 1)
debt_table(loan2, interest_rate, loan_period, 50, robot_year+1)
debt_table(loan_housing, interest_rate, loan_period_housing, 50, 1)


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
  


cost_robot <- 360000
salvage_robot <- 45000
robot_year_AGDS <- 7

vdb(cost_robot, salvage_robot, robot_year_AGDS, factor=1.5, sequence=TRUE)



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




library(googleVis)


df <- data.frame(profile =c("A","B","C","D"),
                 var1 = c(1,2,2,1),
                 var2 = c(4,2,6,3))
gvisColumnChart(df, xvar="profile", yvar=c("var1","var2")) %>% plot()


ls()
f <- function() {
  browser()
  x <- 0
  y <- 0
  g <- function() { 
     x <<- 1
    h <- function() {
       y <<-1
       z <<-1
    }
      h()
      list(x1=x,y1=h())
      }
  g()
  browser()
  print(x)
  print(y)
  print(z)
}
f()
ls()






computeSquares <- function(n,messUpVisibility) {
  # pre-allocate v
  # (doesn't actually help!)
  v <- 1:n
  if(messUpVisibility) {
    vLast <- v
  }
  vv <- 0
  # print details of v
  .Internal(inspect(v))
  for(i in 1:n) {
    v[[i]] <- i^2
    if(messUpVisibility) {
      vLast <- vv
      vLast2 <- v
    }
    # print details of v
    .Internal(inspect(v))
  }
  v
}

computeSquares(5,TRUE)


power <- function(exponent) {
  function(x) {
    x ^ exponent 
    }
}

square <- power(2)
square(3)
cube <- power(3)
cube(3)


new_counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  } 
}
counter_one <- new_counter()
counter_two <- new_counter()

counter_one()
counter_one()
counter_two()






x <- 1:10
funs <- list(
  sum = sum,
  mean = mean,
  median = median
)
lapply(funs, function(f) f(x, na.rm = TRUE))



midpoint <- function(f, a, b) {
  (b - a) * f((a + b) / 2)
}

trapezoid <- function(f, a, b) {
  (b - a) / 2 * (f(a) + f(b))
}

newton_cotes <- function(coef, open = FALSE) {
  n <- length(coef) + open
  function(f, a, b) {
    pos <- function(i) a + i * (b - a) / n
    points <- pos(seq.int(0, length(coef) - 1))
    (b - a) / sum(coef) * sum(f(points) * coef)
  }
}


composite <- function(f, a, b, n = 10, rule) {
  points <- seq(a, b, length = n + 1)
  area <- 0
  for (i in seq_len(n)) {
    area <- area + rule(f, points[i], points[i + 1])
  }
  area
}


boole <- newton_cotes(c(7, 32, 12, 32, 7))
milne <- newton_cotes(c(2, -1, 2), open = TRUE)

composite(sin, 0, pi, n = 10, rule = milne)
rule <- c(midpoint, trapezoid, boole, milne)
lapply(rule, function(loc_rule) composite(sin, 0, pi, n = 10, rule = loc_rule))


# rlt <- data.frame(matrix(NA,ncol=4,nrow=1))
# rlt[] <- lapply(rule, function(loc_rule) composite(sin, 0, pi, n = 10, rule = loc_rule))


where <- function(f, x) {
  vapply(x, f, logical(1))
}


poisson_nll <- function(x) {
  n <- length(x)
  sum_x <- sum(x)
  function(lambda) {
    n * lambda - sum_x * log(lambda) # + terms not involving lambda
  }
}


failwith <- function(default = NULL, f, quiet = FALSE) {
  force(f)
  function(...) {
    out <- default
    try(out <- f(...), silent = quiet)
    out
  }
}

# If any model fails, all models fail to fit:
models <- lapply(datasets, glm, formula = y ~ x1 + x2 * x3)
# If a model fails, it will get a NULL value
models <- lapply(datasets, failwith(NULL, glm),
                 formula = y ~ x1 + x2 * x3)

# remove failed models (NULLs) with compact
ok_models <- compact(models)
# extract the datasets corresponding to failed models
failed_data <- datasets[vapply(models, is.null, logical(1))]



and <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) && f2(...)
  }
}

or <- function(f1, f2) {
  force(f1); force(f2)
  function(...) {
    f1(...) || f2(...)
  }
}

not <- function(f) {
  force(f)
  function(...) {
    !f(...)
  }
}

Filter(or(is.character, is.factor), iris)
Filter(not(is.numeric), iris)



abc <- list()
for (a in c(1:3)) {
  
}


list_1 <- list()
gen_list <- function(list_name) {
  function(var_name, n) {
    list_1[[paste0(list_name,"_",var_name)]] <<- c(1:n)
  }
}

oper_1 <- gen_list("list_1")
oper_2 <- gen_list("list_2")

oper_1("var_a", 5)
oper_1("var_b", 7)
list_1





