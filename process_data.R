source("libraries.R")
# remotes::install_git(url = "https://gitlab.com/meter-group-inc/pubpackages/zentracloud")

# IMPORT  ######

source("process_functions.R")

options(readr.show_col_types = FALSE)

Sys.setenv(ZENTRACLOUD_TOKEN = "6f7951c0b288fa94bea984f1f90348b587771e9c")

setZentracloudOptions(
  token = Sys.getenv("ZENTRACLOUD_TOKEN")
  , domain = "default"
)

# getZentracloudOptions()
# clearCache(file_age = 0L)

# Database stuff? add when needed
zl6.db = read_csv(file.path("Microclimate_data_supporting",
                            "zl6_map.csv"))

name.change <- read_excel(file.path("Microclimate_data_supporting",
                                    "Variable_names.xlsx"))

Im.vec = c("NL_GR","NL_MC","NL_UC","NM_GR","NM_MC","NM_UC",
           "RD_GR","RD_MC","RD_UC","SL_GR","SL_MC", 
           "SL_UC","SM_GR","SM_MC","SM_UC")

HOBO.vec=c("RD_GR","NL_GR","NM_GR","SM_GR","SL_GR")
#note to self: RD and NM ground data auto upload to cloud, other ground datas we upload

endDL = as.character(Sys.time())
#Log = data.frame()

Log <- data.frame(MC = NA, Action = NA, Reason = NA)

MC.vec = Im.vec

for(i in MC.vec){
  import.log <- read_csv(file.path("Microclimate_data_supporting",
                                   "zl6_import_log.csv"), show_col_types = F)
  Last.import <- import.log %>% 
    filter(MC.ID == i) %>% 
    pull(Last.import)
  
  if(Last.import <= as.POSIXct(endDL) - days(3)) {
    repeat {
      tryCatch({
        # subtract 5 hours to fix strange gap before data start
        MC <- read_ZC(i, as.character(Last.import - hours(5)), endDL)
        #MC <- read_ZC(i, Start = "2024-06-07 00:00:00", 
                       #End = "2024-06-15 00:00:00")
        break
      }
      , error = function(e) {
        # An error occurred, so print a message and continue with the same i
        cat(paste("Error on iteration", i, ":", conditionMessage(e), "\n"))
      }
      )
    } 
  } else {
    cat("Skipped ", i, ", no import needed", "\n")
    Log1 <- data.frame(MC = i, Action = "Skipped",
                       Reason = "Import log")
    Log <- bind_rows(Log, Log1)
    next
  }
  
  # Format each port's data and combine them together. reduce function is a way to join list elements by a key column
  d <- lapply(seq(1:length(MC)), format_element) %>% 
    lapply(change_column_names, "ZC_R") %>% 
    reduce(left_join, by = "Timestamp")
  
  filename.in.raw <- file.path("Microclimate_data_raw", str_c(i, ".csv"))
  
  if(file.exists(filename.in.raw)){
    raw.data <- read_csv(filename.in.raw, show_col_types = F)
    
    d.out <- bind_rows(raw.data, d) %>% 
      distinct() %>% 
      arrange(Timestamp)
    
    write_csv(d.out, filename.in.raw)
    
  } else{
    write_csv(d, filename.in.raw)
  }
  
  cat("Finished ", i, "\n")
  
  import.log <- import.log %>%
    mutate(Last.import = case_when(
      MC.ID == i ~ max(d$Timestamp, na.rm = T),
      MC.ID != i ~ Last.import))
  
  Log1 <- data.frame(MC = i, Action = "Saved raw data", 
                     Problems = "None")
  Log <- bind_rows(Log, Log1)
  
  write_csv(import.log, file.path("Microclimate_data_supporting",
                                  "zl6_import_log.csv"))
}

# HOBO #######
for(i in HOBO.vec){
  hobo.import.log <- read_csv(file.path("Microclimate_data_supporting",
                                   "hobo_import_log.csv"), show_col_types = F)
  
  Last.import <- hobo.import.log %>% 
    filter(HOBO.ID == i) %>% 
    pull(Last.import)
  
  curhobo=read.csv(str_c("./hobo/",i,"_hobo.csv"),skip=1)
  curhobo=change_column_names(curhobo,"HOBO")
  rid=which(is.na(curhobo$`Air TempC`))
  if(length(rid)>0){
    curhobo=curhobo[-rid,]
  }
  curhobo$Timestamp=sapply(curhobo$Timestamp,as.POSIXct,format="%m/%d/%y %H:%M:%S %p")
  offset=getoffset(i)
  curhobo[,1]=curhobo[,1]-offset
  curhobo[,1]=strftime(curhobo[,1])
  curhobo=curhobo[curhobo$Timestamp>Last.import,]
  addhobo(curhobo,i)
  
  hobo.import.log <- hobo.import.log %>%
    mutate(Last.import = case_when(
      HOBO.ID == i ~ max(curhobo$Timestamp, na.rm = T),
      HOBO.ID != i ~ as.character(Last.import)))
  
  write_csv(hobo.import.log, file.path("Microclimate_data_supporting",
                                  "hobo_import_log.csv"))
  
  print(str_c("Done with ",i))
}

# EM50 ########

# PROCESS ######
files=list.files("./Microclimate_data_raw")

treelist=c("NL","NM","RD","SL","SM")

#---------------------------

for(t in treelist){
  processtree(t)
}

