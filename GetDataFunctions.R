merge_sitl <- function(ina, inb) {
  outdata <- na.omit(read.csv(ina))
  sitl <- read.csv(inb)
  
  # omit first X column
  outdata <- outdata[,-1]
  
  # Rectifying date/times
  sitl$Start = stringr::str_replace_all(sitl$Start, "([T])", " ")
  sitl$End = stringr::str_replace_all(sitl$End, "([T])", " ")
  
  sitl$Start <- lubridate::hour(sitl$Start)*3600 + lubridate::minute(sitl$Start)*60 + lubridate::second(sitl$Start)
  sitl$End <- lubridate::hour(sitl$End)*3600 + lubridate::minute(sitl$End)*60 + lubridate::second(sitl$End)
  outdata$Time <- lubridate::hour(outdata$Time)*3600 + lubridate::minute(outdata$Time)*60 + lubridate::second(outdata$Time)
  outdata[,"Priority"] <- 0
  outdata[,"Selected"] <- 0
  
  # populate Y in outdata
  for(i in 1:nrow(sitl)){
    for(x in 1:nrow(outdata)){
      if(outdata$Time[x] >= sitl$Start[i]){
        if(outdata$Time[x] <= sitl$End[i]){
          outdata$Priority[x] <- sitl$Priority[i]
          outdata$Selected[x] <- 1
        }
      }
    }
  }
  outdata
}

split_point <- function(indata) {
  select = floor(0.75 * nrow(indata))
  set.seed(0) 
  train.select <- sample(seq_len(nrow(indata)), size = select)
}

split_train <- function(indata){
  point = split_point(indata)
  train <- indata[point, ]
  train
}

split_test <- function(indata){
  point = split_point(indata)
  test <- indata[-point, ]
  test
}