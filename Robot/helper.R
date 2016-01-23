

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

round_pct <- function(x, digits=2) { 
  paste0(round(x,digits),"%")
} 


formatcomma <- function(x, digits=NULL, dollar=FALSE) {
  if (length(x)==0) { return(NA) }
  if (is.null(digits)) {
    xFormat <- format(x, big.mark=",", scientific=FALSE) 
  } else {
    xFormat <- format(round(x,digits), big.mark=",", scientific=FALSE) 
  }
  if (dollar) { xFormat <- paste0("$", xFormat) }
  return(xFormat)
} 

formatdollar <- function(x,digit=0) {
  if (length(x)==0) { return(NA) }
  if (x>=0) {
    paste0("$",x %>% round(digit) %>% formatcomma()) 
  } else {
    paste0("-$",-x %>% round(digit) %>% formatcomma()) 
  }
}

gen_formatdollar <- function(digit=0) {
  function(x) {
    if (length(x)==0) { return(NA) }
    if (x>=0) { 
      paste0("$",x %>% round(digit) %>% formatcomma()) 
    }  else {
      paste0("-$",-x %>% round(digit) %>% formatcomma())
    }
  }
}

formatdollar_2d <- gen_formatdollar(2)
# formatdollar_2d(-12.345)


formatdollar2 <- function(x,digit=0) { # add +/- sign
  if (is.na(x)) { return(NA) }
  if (is.null(x)) { return(NA) }
  if (x>=0) {
    paste0("+$",x %>% round(digit) %>% formatcomma()) 
  } else {
    paste0("-$",-x %>% round(digit) %>% formatcomma()) 
  }
}

formatdollar2b <- function(x,digit=0) { # add parenthesis 
  if (is.na(x)) { return(NA) }
  if (is.null(x)) { return(NA) }
  if (x>=0) {
    paste0("(+$",x %>% round(digit) %>% formatcomma(),")") 
  } else {
    paste0("(-$",-x %>% round(digit) %>% formatcomma(),")") 
  }
}


df_null <- function(colnames) {
  dummy <- matrix(rep(NA,length(colnames)),nrow=1)
  colnames(dummy) <- colnames 
  empty_table <- data.frame(Column1 = numeric(0)) 
  empty_table <- rbind(empty_table, dummy)[NULL,] 
  empty_table
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


rate <- function(nper, pmt, pv) {
  irr(c(pv,rep(pmt, nper)))
}


# annualized net present value
# need: library(dplyr) for %>% operation
anpv <- function(x, r, i, nper) {
  x_seq <- lapply(c(1:nper), function(t) { x*(1+i)^(t-1) }) %>% unlist()
  -pmt(r, nper, npv(r, x_seq))
}

# Used to bring anpv() back to the real term basis 
deflator <- function(nper, i) {
  nper/sum((1 + i)^(seq_along(c(1:nper)-1))) 
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
  if (nper<= 0) { 
    return(NA) 
  } else if (nper<=1) {
    v <- s0 
  } else {  
    tmp <- cost * factor/nper
    v[1] <- (tmp>s) * tmp + (tmp<=s) * s 
    vsum <- v[1]; 
    for (t in 2:nper) {
      s <-  (cost - vsum[t-1] - salvage)/(nper - t + 1)
      tmp <-  (cost - vsum[t-1]) * factor/nper
      v[t] <- switch * ((tmp>s) * tmp + (tmp<=s) * s ) + (!switch) * tmp
      vsum[t] <- vsum[t-1] + v[t]
    }
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
  if (interest_rate < 1e-9) interest_rate <- 1e-9
  
  pmt <- -pmt(interest_rate, loan_period, loan)
  interest <- impt(interest_rate, 1, loan_period, loan)
  principal <- pmt - interest
  
  df <- data.frame(year=c(1:n_period), yr_pmt =rep(0,n_period))
  
  ending_year <- loan_period + starting_year - 1
  df$yr_pmt[starting_year:ending_year] <- c(1:loan_period)
  
  df$interest <-  (df$yr_pmt >0) *
    (-1) * impt(interest_rate, df$yr_pmt, loan_period, loan) %>% round(2)
  
  df$principal <- (df$yr_pmt >0) * (pmt - df$interest)  %>% round(2)
  
  return(df)
}



#---


# ----- dashboard features -----

dash_IOFC <- function(IOFC, IOFC2, basis, x, 
                      cutoff=0,
                      difference=NULL) {
  
  if (basis=="per cow") { 
    digit <- 0
    IOFC_unit <- "IOFC ($/cow/year)"
  } else {
    digit <- 2
    IOFC_unit <- "IOFC ($/cwt)"
  }
  
  if (is.na(IOFC) | is.na(IOFC2)) {
    diff <- NA
  } else {
    diff <- IOFC2 - IOFC
  }
  if (is.na(IOFC)) {
    style <- "background-color: #3EA055; color:white;"
  } else {
    if (IOFC > cutoff) { 
      style <- "background-color: #3EA055; color:white;"
      #  style <- "background-color: #726F2D;  color:white;"
    } else {
      style <-  "background-color: #F70D1A; color:white;" 
    }
  }
  if (is.null(difference)) {
    div(class="well", style=style,  align="center",
        diff  %>% formatdollar2(digit) %>% strong() %>% h3(),
        h5(IOFC_unit), h5("under", refProfileName(x)))
  } else {
    div(class="well", style=style,  align="center",
        diff  %>% formatdollar2(digit) %>% strong() %>% h3(),
        h5(IOFC_unit),
        difference %>% formatdollar2b(digit) %>% strong() %>% h4())
  }
}



dash_NAI <- function(NAI, x,cutoff=0, difference=NULL) {
  
  if (is.na(NAI)) {
    style <- "background-color: #306EFF; color:white;"
  } else {
    if (NAI>cutoff) { 
      style <- "background-color: #306EFF; color:white;"
      # style <- "background-color: #0B3D4C; color:white;"
    } else {
      style <- "background-color: #F70D1A; color:white;" 
    }
  }
  
  if (is.null(difference)) {
    div(class="well", style=style, align="center", 
        NAI %>% formatdollar2() %>% strong() %>% h3(),
        h5("Net Impact ($/year)"),
        h5("under", refProfileName(x)))
  } else {
    div(class="well", style=style, align="center",
        NAI %>% formatdollar2() %>% strong() %>% h3(),
        h5("Net Impact ($/year)"),
        difference %>% formatdollar2b() %>% strong() %>% h4())
  }
} 


