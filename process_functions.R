source("libraries.R")

# Basic ZC read function. Read data from zentracloud based on our naming convention
# Parameters: MC: MC ID; Start: Date start in y-m-d h:m:s; End: Date end
read_ZC <- function(MC, Start, End){
  ZL <- zl6.db %>% 
    filter(MC.ID == MC) %>% 
    pull(ZL.ID)
  d <- getReadings(
    device_sn = ZL, start_time = Start, end_time = End
  )
  return(d)
}

reducename = function(x,source){
  if(source=="ZC_R"){
    if(endsWith(x,"_wetness_level")){
      return(x)
    }
    if(x=="Timestamp"){
      return(x)
    }
    return(substr(x,7,nchar(x)))
  }
  if(source=="HOBO"){
    return(unlist(strsplit(x,".",fixed=TRUE))[1])
  }
}

change_column_names <- function(element, source){
  name.change2 <- name.change %>% 
    select(Final, Original = all_of(source))
  element2=data.frame(element)
  VarNames = names(element2[1:ncol(element2)])
  VarNamesReduced=as.character(lapply(VarNames,reducename,source))
  newVarNames = name.change$Final[match(VarNamesReduced, name.change2$Original)]
  newColNames = newVarNames
  names(element2) <- newColNames
  element2 <- element2[!is.na(names(element2))]
  return(element2)
}

format_element <- function(element){
  # Add the tz offset so it shows the correct time, but call it "UTC" to match with all the other data
  d <- MC[[element]] %>% 
    select(!contains("error")) %>% 
    rename_with(~str_remove(., ".value")) %>%
    mutate(Timestamp = timestamp_utc + tz_offset,
           Timestamp = as_datetime(Timestamp)) %>% 
    select(Timestamp, everything(), -timestamp_utc, -tz_offset, -datetime)
  
  port.num <- rep(str_c("Port", str_sub(names(MC), -1)[element], "_"),
                  ncol(d) - 1)
  
  d2 <- d %>%
    rename_with(.cols = !starts_with("Time"), .fn = ~str_c(port.num, .))
  return(d2)
}

appcol=c("Timestamp","VPD","Vapor.Pressure","Atmos.Pressure","Air.TempC","Wetness.Level.1","Wetness.Level.2","Solar")

processtree = function(t){
  curfiles=files[str_detect(files,t)]
  df=data.frame(matrix(ncol=10,nrow=0))
  colnames(df)=c(appcol,"Station","Tree")
  for (f in curfiles){
    station=substr(f,4,5)
    curdf=read.csv(sprintf("./Microclimate_data_raw/%s",f),header=TRUE)
    # for(c in appcol){
    #   curcols=colnames(curdf)[str_detect(colnames(curdf),c)]
    #   if(length(curcols)<2){
    #     next
    #   }
    #   curdf=unite(curdf, new, curcols, sep = "", remove = FALSE ,na.rm=TRUE)
    #   colnames(curdf)=replace(colnames(curdf),colnames(curdf)=="new",c)
    # }
    curdf=select(curdf,ends_with(appcol))
    curdf$Station=try(rep(station,length(curdf[,1])))
    curdf$Tree=try(rep(t,length(curdf[,1])))
    df=rbind(df,curdf)
  }
  df=try(df %>% relocate(Station))
  df=try(df %>% relocate(Tree))
  df=try(df %>% relocate(Timestamp))
  write.csv(df,sprintf("./Microclimate_data_L2/%s_L2.csv",t),row.names=FALSE)
}

addhobo = function(d,station){
  filename=str_c("./Microclimate_data_raw/",station,".csv")
  fulldata <- read.csv(filename)
  fulldata$Timestamp=sapply(fulldata$Timestamp,as.POSIXct,format="%Y-%m-%dT%H:%M:%SZ")
  fulldata$Timestamp=sapply(fulldata$Timestamp,strftime)
  
  for(r in 1:nrow(d)){
    currow=d[r,]
    if(as.character(currow$Timestamp[1]) %in% fulldata['Timestamp']){
        fulldata[which(fulldata$Timestamp==currow$Timestamp[1]),'Air.TempC'] = as.numeric(currow$`Air TempC`[1])
      } else {
        fulldata = add_row(fulldata, Timestamp=currow[1,1], Air.TempC=as.numeric(currow[1,2]))
      }
  }
  write_csv(fulldata, filename)
}

isotoposix = function(iso){
  ret=strptime(iso, "%Y-%m-%dT%H:%M")
  if(is.na(ret)){
    return(iso)
  }
  return(ret)
}

getoffset=function(site){
  offsetdb=read.csv("./Microclimate_data_supporting/hobo_offset.csv")
  
  offset <- offsetdb %>% 
    filter(hobo.ID == i) %>% 
    pull(offset)
  
  return(offset)
}
