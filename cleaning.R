# Installing packages and masking functions --------------------
rm(list=ls())
list.of.packages <- c("dplyr", "stringr","ggmap","RPostgreSQL","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
select <- dplyr::select
rm(list.of.packages,new.packages)
# Defining functions to be used --------------------
goa.get <- function(){
  #The modified GOA data with better longitude and latitude is saved under crimeTable so will be replacing GOA with crimeTable
  #Syntax: goa <- getGOA()
  my_db  <- src_postgres(dbname="csci403",host="flowers.mines.edu",user="ceberlco",password=.rs.askForPassword("DB Pass?"))
  return(as.data.frame(tbl(my_db,"crimetable"),n=-1))
}
goa.clean <- function(data){
  #This is specifically for GOA! It cleans and formats
  #Fixing the date and time
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
  d <- str_sub(data$occ_date,start=1,end=-6)
  t <- unlist(lapply(X = data$occ_time, FUN = colonifier))
  data$occ_date <- lubridate::mdy_hm(str_join(d,t,sep=" ") ,tz = "MST")
  data <- data %>% select(-c(occ_time))
  #Fixing date of birth
  b <- str_sub(data$dob,start=1,end=-6)
  data$dob <- lubridate::mdy(b,tz="MST")
  #Fixing sex
  data$sex <- as.factor(data$sex)
  #Fixing translation 
  data$translation <- str_trim(as.factor(data$translation),side=c("right"))
  data$translation <- as.factor(data$translation)
  #Fixing ucr_cat
  data$ucr_cat <- as.factor(data$ucr_cat)
  #Fixing ucr_group
  data$ucr_group[which(data$ucr_group=="Public Disorder Crime")] <- "Public Disorder Crimes"
  data$ucr_group <- as.factor(data$ucr_group)
  return(data)
}
sca.get <- function(){
  #Syntax: sca <- getSCA()
  my_db  <- src_postgres(dbname="csci403",host="flowers.mines.edu",user="ceberlco",password=.rs.askForPassword("DB Pass?"))
  return(as.data.frame(tbl(my_db,"sca"),n=-1))
}
sca.clean<- function(data){
  #This is specifically for SCA! It cleans and formats 
  #Fixing date and time
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
  d <- str_sub(data$occ_date,start=1,end=-6)
  data$occ_date <-lubridate::mdy_hm(str_join(d,s,sep=" ") ,tz = "MST")
  data <- data %>% select(-c(rt,occ_time))
  #Fixing date of birth 
  b <- str_sub(data$dob,start=1,end=-6)
  data$dob <- lubridate::mdy(b,tz="MST")
  #Fixing sex 
  data$sex <- as.factor(data$sex)
  #Fixing reason checked and reason checked text 
  data$reason_checked <- as.factor(data$reason_checked)
  data$reason_text <- as.factor(data$reason_text)
  return(data)
}
# Clean goa --------------------
g <- goa.get()
goa <- goa.clean(g)
glimpse(goa)
# Clean sca -------------------- 
s <- sca.get()
sca <- sca.clean(s)
glimpse(sca)


# If the above does not function, grab the csv from Excel and clean --------------------
test <- read.csv(paste(getwd(),"/GeneralOccurrencesAll2.csv",sep=""),header=T,sep=",")
excel_clean <- function(data){
  data$TRANSLATION <- str_trim(data$TRANSLATION,side=c("right"))
  data$TRANSLATION <- as.factor(data$TRANSLATION)
  data$UCR_Cat <- as.factor(data$UCR_Cat)
  #Have to add a bit of manual cleaning here after having looked at the levels of ucr_group: there is one that is misspelt, so it
  #shows up as a fifth category even though it's just missing an 's' at the end
  data$UCR_Group[which(data$UCR_Group=="Public Disorder Crime")] <- "Public Disorder Crimes"
  levels(data$UCR_Group)[4] <- NA
  
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
  
  d <- str_sub(data$OCC_DATE,start=1,end=-6)
  t <- unlist(lapply(X = data$OCC_TIME, FUN = colonifier))
  data$OCC_DATE <- lubridate::mdy_hm(str_join(d,t,sep=" ") ,tz = "MST")
  data <- data %>% select(-c(OCC_TIME))
  colnames(data) <- tolower(colnames(data))
  return(data)
}
goa <- excel_clean(test)
