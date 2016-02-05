# Installing packages and masking functions --------------------
rm(list=ls())
list.of.packages <- c("dplyr", "stringr","ggmap","RPostgreSQL")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
select <- dplyr::select
rm(list.of.packages,new.packages)
# Defining functions to be used --------------------
getGOA <- function(){
  #Syntax: goa <- getGOA()
  my_db  <- src_postgres(dbname="csci403",host="flowers.mines.edu",user="ceberlco",password=.rs.askForPassword("DB Pass?"))
  return(as.data.frame(tbl(my_db,"goa"),n=-1))
}
goa_cleanDates <- function(data){
  #This is specifically for GOA! It cleans the occ_date, occ_time, and dob columns
  colonifier <- function(s){
    #The occ_time column is unstructed numbers, but can be made into "time-like" characters
    #then turned into POSIX objects, i.e. actual R time data type
    options(warn=-1)
    if(str_length(s)==1){
      paste("00:0",s,sep="")    
    } else if(str_length(s)==2){
        paste("00:",s,sep="")
    } else if(str_length(s)==3){
        paste("0",str_sub(s,1,1),":",str_sub(s,2,3),sep="")
    } else if(str_length(s)==4){
        paste(str_sub(s,1,2),":",str_sub(s,3,4),sep="")
    } else {
        return(NA)
    }
  }

  data$occ_date <- str_sub(data$occ_date,start=1,end=-6)
  data$occ_time <- unlist(lapply(X = data$occ_time, FUN = colonifier))
  temp <- str_join(data$occ_date, data$occ_time,sep=" ")
  data$occ_date <- strptime(temp,format="%m/%d/%Y %H:%M")
  data$occ_date <- as.POSIXct(data$occ_date)
  #This line above is because of the following error: https://github.com/hadley/dplyr/issues/859
  data <- data[,-6]
  data$dob <- str_sub(data$dob,start=1,end=-6)
  data$dob <- as.Date(data$dob,format="%m/%d/%Y")
  return(data)
}
goa_factorize <- function(data){
  data$sex <- as.factor(data$sex)
  data$translation <- as.factor(data$translation)
  data$translation <- str_trim(data$translation,side=c("right"))
  data$ucr_cat <- as.factor(data$ucr_cat)
  #Have to add a bit of manual cleaning here after having looked at the levels of ucr_group: there is one that is misspelt, so it
  #shows up as a fifth category even though it's just missing an 's' at the end
  data$ucr_group[which(data$ucr_group=="Public Disorder Crime")] <- "Public Disorder Crimes"
  data$ucr_group <- as.factor(data$ucr_group)
  data$exp_translation <- as.factor(data$exp_translation)
  return(data)
}
getSCA <- function(){
  #Syntax: sca <- getSCA()
  my_db  <- src_postgres(dbname="csci403",host="flowers.mines.edu",user="ceberlco",password=.rs.askForPassword("DB Pass?"))
  return(as.data.frame(tbl(my_db,"sca"),n=-1))
}
sca_cleanDates <- function(data){
  #This is specifically for SCA! It cleans the occ_date, occ_time, and dob columns. It's being a jerk, 
  #so I have to come up with a different funcitonality. 
  colonifier <- function(s){
    #The occ_time column is unstructed numbers, but can be made into "time-like" characters
    #then turned into POSIX objects, i.e. actual R time data type
    options(warn=-1)
    if(str_length(s)==1){
      paste("00:0",s,sep="")    
    } else if(str_length(s)==2){
      paste("00:",s,sep="")
    } else if(str_length(s)==3){
      paste("0",str_sub(s,1,1),":",str_sub(s,2,3),sep="")
    } else if(str_length(s)==4){
      paste(str_sub(s,1,2),":",str_sub(s,3,4),sep="")
    } else {
      return(NA)
    }
  }
  s <- data$occ_time
  s[which(str_length(s)==1)] <- colonifier(s[which(str_length(s)==1)])
  s[which(str_length(s)==2)] <- colonifier(s[which(str_length(s)==2)])
  s[which(str_length(s)==3)] <- colonifier(s[which(str_length(s)==3)])
  s[which(str_length(s)==4)] <- colonifier(s[which(str_length(s)==4)])
  
  data$occ_date <- str_sub(data$occ_date,start=1,end=-9)
  data$occ_time <- s
  temp <- str_join(data$occ_date, data$occ_time,sep=" ")
  data$occ_date <- strptime(temp,format="%m/%d/%Y %H:%M")
  data$occ_date <- as.POSIXct(data$occ_date)
  #This line above is because of the following error: https://github.com/hadley/dplyr/issues/859
  data <- data[,-4]
  data <- data[,-2]
  data$dob <- str_sub(data$dob,start=1,end=-9)
  data$dob <- as.Date(data$dob,format="%m/%d/%Y")
  return(data)
}
sca_factorize <- function(data){
  data$sex <- as.factor(data$sex)
  data$reason_checked <- as.factor(data$reason_checked)
  data$reason_text <- as.factor(data$reason_text)
  return(data)
}
# Clean goa --------------------
goa <- getGOA()
goa <- goa_cleanDates(goa) 
goa <- goa_factorize(goa)
glimpse(goa)
# Clean sca -------------------- 
sca <- getSCA()
sca <- sca_cleanDates(sca)
sca <- sca_factorize(sca)
glimpse(sca)
