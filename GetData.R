knitr::opts_chunk$set(echo = FALSE)

#library(stringr)
#library(lubridate)

data <- na.omit(read.csv("mms_20151016.csv"))
sitl <- read.csv("sitl_20151016.csv")

# omit first X column some columns
data <- data[,-1]

# Rectifying date/times
sitl$Start = stringr::str_replace_all(sitl$Start, "([T])", " ")
sitl$End = stringr::str_replace_all(sitl$End, "([T])", " ")

sitl$Start <- lubridate::hour(sitl$Start)*3600 + lubridate::minute(sitl$Start)*60 + lubridate::second(sitl$Start)
sitl$End <- lubridate::hour(sitl$End)*3600 + lubridate::minute(sitl$End)*60 + lubridate::second(sitl$End)
data$Time <- lubridate::hour(data$Time)*3600 + lubridate::minute(data$Time)*60 + lubridate::second(data$Time)
data[,"Priority"] <- 0
data[,"Selected"] <- 0

# populate Y in data
for(i in 1:nrow(sitl)){
  for(x in 1:nrow(data)){
    if(data$Time[x] >= sitl$Start[i]){
      if(data$Time[x] <= sitl$End[i]){
        data$Priority[x] <- sitl$Priority[i]
        data$Selected[x] <- 1
      }
    }
  }
}

# split data
train.size <- floor(0.75 * nrow(data))
set.seed(0)
train.select <- sample(seq_len(nrow(data)), size = train.size)
train <- data[train.select, ]
test <- data[-train.select, ]

# order values by time
train <- train[order(train$Time),]