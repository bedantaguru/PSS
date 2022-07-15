

progress_of_data_read <- function(set, desc = ""){
  pf <- file.path("Data/Cache", "progress")
  if(missing(set)){
    if(file.exists(pf)){
      return(readRDS(pf))
    }else{
      list(pct = 0 , desc = "Not Started")
    }
  }else{
    saveRDS(list(pct = set , desc = desc), pf)
  }
}
