

library(dplyr)
library(lubridate)
library(tidyr)
library(RcppRoll)
library(xts)

options(scipen = 15)

dat <- readRDS("Data/Local/data")

# (Volume in lakhs; Value in INR Crores)

dat <- dat %>% 
  mutate(
    Month = ymd(paste0(year(Date),"-",month(Date),"-01")),
    Quarter = paste0(year(Date),"-",quarter(Date))
  ) %>% 
  group_by(Quarter) %>% 
  mutate(Quarter_Date = min(Date)) %>% 
  ungroup() %>% 
  select(-Quarter) %>% 
  rename(Quarter = Quarter_Date)

dat_avg_txn <- dat %>% 
  filter(Vol_or_Val == "Vol") %>% 
  rename(Vol=Value) %>% 
  inner_join(
    dat %>% 
      filter(Vol_or_Val == "Val") %>% 
      rename(Val = Value), 
    by = c("Date", "Payment_or_Cash", "Operator", "PayMode", "Month", "Quarter")
  ) %>% 
  mutate(Value = Val/Vol*100) %>% 
  mutate(Vol_or_Val="AvgVal") %>% 
  .[colnames(dat)] %>% 
  distinct()


dat <- dat %>% 
  bind_rows(dat_avg_txn) %>% 
  rename(
    Var_Type = Vol_or_Val,
    Day = Date
  )

rm(dat_avg_txn)


arg_info <- list(
  Var_Type =  tibble(
    name = c("Volume (in lakhs)","Value (in INR Crores)", "Average Value per Txn"),
    val = c("Vol", "Val", "AvgVal")
  ),
  Time_Var =  tibble(
    name = c("Day","Month","Quarter"),
    val = c("Day","Month","Quarter")
  ),
  Aggregator = tibble(
    name = c("Sum","Mean", "Median", "Max", "Min"),
    val = c("sum","mean", "median", "max", "min")
  ),
  Gr_Var = tibble(
    name = c("Mode of Payment (cash/non-cash)","Payment System Operator", "Payment System"),
    val = c("Payment_or_Cash","Operator", "PayMode")
  ),
  xox_map = tibble(
    name = c("Day","Month","Quarter"),
    vals = list(c("Daily Growth", "Month on Month", "Q-o-Q", "Y-o-Y"),
                c("M-o-M", "Q-o-Q", "Y-o-Y"),
                c("Q-o-Q", "Y-o-Y"))
  )
)

interpret <- function(val,dat){
  dat$val[dat$name == val]
}

get_data <- function(
    Var_Type_in = arg_info$Var_Type$val, 
    Time_Var = arg_info$Time_Var$val,
    Aggregator = arg_info$Aggregator$val,
    Gr_Var = arg_info$Gr_Var$val,
    Gr_Var_Fine
){
  
  Time_Var <- match.arg(Time_Var)
  Aggregator <- match.arg(Aggregator)
  Gr_Var <- match.arg(Gr_Var)
  
  if(missing(Gr_Var_Fine)){
    Gr_Var_Fine <- unique(dat[[Gr_Var]])
  }
  
  # local copy
  dat_l <- dat %>% 
    rename(Time_Var = !!Time_Var, Gr_Var = !!Gr_Var) %>% 
    filter(Var_Type %in% !!Var_Type_in) %>% 
    filter(Gr_Var %in% !!Gr_Var_Fine) %>% 
    distinct(Time_Var, Gr_Var, Var_Type, Value)
  
  
  f_a <- get(Aggregator)
  
  d <- dat_l %>%
    group_by(Time_Var, Gr_Var, Var_Type) %>% 
    summarise(Val = f_a(Value, na.rm = TRUE), .groups = "drop")
  
  
  ma_dat <- function(wd = 3){
    
    if(wd <=1 ) return(d)
    
    d %>% 
      group_by(Gr_Var, Var_Type) %>% 
      arrange(Time_Var) %>% 
      mutate(Val = roll_mean(Val, n = wd, fill = NA)) %>% 
      ungroup()
  }
  
  
  
  flat_dat <- function(Var = Var_Type_in[1], in_ts = FALSE, wd = 1, dat_in){
    
    if(missing(dat_in)){
      dat_in <- ma_dat(wd)
    }
    
    dp <- dat_in %>% 
      filter(Var_Type == !!Var) %>% 
      pivot_wider(id_cols = Time_Var, names_from = Gr_Var, values_from = Val)
    dp <- dp %>% arrange(Time_Var)
    
    if(in_ts){
      dp %>% select(-Time_Var) %>% xts(order.by = dp$Time_Var)
    }else{
      dp
    }
  }
  
  xox_dat <- function(
    Var = Var_Type_in[1], in_ts = FALSE , wd = 1, 
    xox = arg_info$xox_map$vals[ arg_info$xox_map$name == Time_Var][[1]][1]
  ){
    dxox <- ma_dat(wd) %>% 
      filter(Var_Type == !!Var)
    dxox <- dxox %>% 
      arrange(Time_Var) %>% 
      group_by(Gr_Var)
    
    lvls <- arg_info$xox_map$vals[ arg_info$xox_map$name == Time_Var][[1]]
    
    lvlt <- which(lvls == xox)
    
    if(Time_Var == "Day"){
      xox_out <- switch(lvlt,
        # daily
        "1" = dxox %>% mutate(Val0 = 100*(Val/lag(Val, 1)-1)),
        # mom
        "2" = dxox %>% mutate(Val0 = 100*(Val/lag(Val, 30)-1)),
        #qoq
        "3" = dxox %>% mutate(Val0 = 100*(Val/lag(Val, 30*3)-1)),
        #yoy
        dxox %>% mutate(Val0 = 100*(Val/lag(Val, 365)-1))
      )
    }
    
    if(Time_Var == "Month"){
      xox_out <- switch(lvlt,
        # mom
        "1" = dxox %>% mutate(Val0 = 100*(Val/lag(Val, 1)-1)),
        # qoq
        "2" = dxox %>% mutate(Val0 = 100*(Val/lag(Val, 3)-1)),
        # yoy
        dxox %>% mutate(Val0 = 100*(Val/lag(Val, 12)-1))
      )
    }
    
    if(Time_Var == "Quarter"){
      xox_out <- switch(lvlt,
        # qoq
        "1" = dxox %>% mutate(Val0 = 100*(Val/lag(Val, 1)-1)),
        #yoy
        dxox %>% mutate(Val0 = 100*(Val/lag(Val, 4)-1))
      )
    }
    
    xox_out <- xox_out %>% select(-Val) %>% rename(Val = Val0) %>% ungroup()
    
    
    if(in_ts){
      flat_dat(dat_in = xox_out, Var = Var, wd = wd, in_ts = TRUE)
    }else{
      xox_out
    }
    
    
  }
  
  monthly_pie_dat <- function(Var = Var_Type_in[1], wd = 1){
    
    if(Time_Var=="Quarter") stop("not for Quarter!!")
    
    dm <- ma_dat(wd) %>% 
      filter(Var_Type == !!Var) %>%
      arrange(Time_Var) %>% 
      mutate(Month = format(Time_Var, "%Y-%m"), MTag = format(Time_Var, "%b-%Y")) %>% 
      group_by(Month, MTag, Gr_Var) %>% 
      summarise(Val = sum(Val, na.rm = TRUE), .groups = "drop") %>% 
      arrange(Month)
    
    
    dm %>% inner_join(
      dm %>% distinct(Month, MTag) %>% 
        mutate(Month_Fct = factor(Month, labels = MTag)),
      by = c("Month", "MTag")
    ) -> dm
    
    dm
    
  } 
  
  lo <- list(
    data = d, 
    get_ma_dat = ma_dat, 
    get_flat_dat = flat_dat, 
    get_monthly_pie_dat = monthly_pie_dat,
    get_xox_dat = xox_dat
  )
}
