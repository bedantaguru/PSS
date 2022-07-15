
library(dplyr)
library(purrr)
library(openxlsx)
library(stringr)
library(lubridate)
library(zoo)

source("Lib/progress_of_data_read.R")

tce <- asNamespace("tidycells")

xf <- list.files("Data/Cache/", pattern = ".xlsx", full.names = TRUE)

xf_sheets <- tidyxl::xlsx_sheet_names(xf)

progress_of_data_read(3, "Reading Sheets")

xf_dat_lst <- xf_sheets %>% 
  map(~{
    
    progress_of_data_read(
      3+round(25*(which(xf_sheets == .x))/length(xf_sheets)), 
      paste0("Reading Sheets: ", which(xf_sheets == .x))
    )
    
    read.xlsx(xf, sheet = .x, colNames = FALSE, rowNames = FALSE, 
              fillMergedCells = TRUE, detectDates = TRUE)
  })


cdp_quick_tap <- function(dat){
  d <- tce$as_cell_df(dat)
  d <- tce$numeric_values_classifier(d)
  val_range <- d %>%
    filter(type == "value") %>%
    summarise(mr = min(row), mc = min(col), Mr = max(row), Mc =max(col))
  d$type[
    d$row>=val_range$mr &
      d$row<=val_range$Mr &
      d$col>=val_range$mc &
      d$col<=val_range$Mc
  ] <- "value"
  
  d <- d %>% filter(row<=val_range$Mr)
  d %>% 
    tce$analyse_cells() %>% 
    tce$compose_cells(silent = TRUE) %>% 
    tce$collate_columns()
}

progress_of_data_read(30, "Making {tidy}")

Ns <- length(xf_dat_lst)

xf_tidy_dat_lst <- 1:Ns %>% 
  map(~{
    progress_of_data_read(
      30+round(59*.x/Ns), 
      paste0("Making {tidy}, Sheet No: ", .x)
    )
    cdp_quick_tap(xf_dat_lst[[.x]])
  })

# light fix : dependent on structure 
cn_assign <- function(dat){
  if(ncol(dat)==9){
    colnames(dat) <- c("PubTag_0","UnitTag_0","Payment_or_Cash","Operator","PayMode","Vol_or_Val","Date","Tag_0","Value")
  }else{
    colnames(dat) <- c("PubTag_0","UnitTag_0","Payment_or_Cash","Operator","PayMode", "PayMode_Micro_1","Vol_or_Val","Date","Tag_0","Value")
  }
  dat
}


progress_of_data_read(90, "Naming Columns")

full_dat <- xf_tidy_dat_lst %>% map_dfr(cn_assign)


full_dat %>% 
  mutate(
    PayMode_Micro_1  = ifelse(
      is.na(PayMode_Micro_1),
      "",
      PayMode_Micro_1 
    ),
    PayModeGen = ifelse(
      PayMode == PayMode_Micro_1, 
      PayMode, 
      paste0(PayMode, " ", PayMode_Micro_1)
    ) %>% str_trim()
  ) -> full_dat

selected_dat <- full_dat %>% 
  select(Payment_or_Cash, Operator, PayMode = PayModeGen, Vol_or_Val, Date, Value) %>% 
  distinct()

selected_dat <- selected_dat %>% 
  mutate(
    Value = suppressWarnings(
      as.numeric(Value)
    )
  )

selected_dat <- selected_dat %>% 
  filter(Date %>% tolower() %>% str_detect("total") %>% `!`())

progress_of_data_read(95, "Casting Dates")

dts <- selected_dat$Date %>% unique()
dts_out <- dts %>% 
  map_dbl(~{
    if(str_detect(.x,"[a-zA-Z]")){
      mdy(.x)
    }else{
      ymd(.x)
    }
  })

progress_of_data_read(99, "Saving")

dts_out <- as.Date(dts_out)
dts_d <- tibble(Date = dts, DT = dts_out)

selected_dat <- selected_dat %>% inner_join(dts_d, by = "Date")

selected_dat <- selected_dat %>% 
  select(Date = DT, Payment_or_Cash, Operator, PayMode, Vol_or_Val, Value)

saveRDS(selected_dat, "Data/Local/data")
meta <- list(type = "Daily Data", last_update = Sys.Date())
saveRDS(meta, "Data/Local/meta")
