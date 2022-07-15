


read_data <- function(fn){
  xf <- list.files("Data/Cache/", pattern = ".xlsx", full.names = TRUE)
  if(length(xf)>0){
    unlink(xf, recursive = TRUE)
    unlink("Data/Cache/progress")
  }
  
  outer <- function(fn){
    source("Lib/progress_of_data_read.R")
    progress_of_data_read(1, "Start")
    file.copy(fn, "Data/Cache", overwrite = TRUE, recursive = TRUE)
    source("Lib/read_data_from_cache.R", local = TRUE)
    progress_of_data_read(100, "Done!")
  }
  
  callr::r_bg(outer, args = list(fn = fn))
  
}
