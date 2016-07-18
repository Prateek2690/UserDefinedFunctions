#*******************************************************
# READING TRACEFILES FROM DATE X TO DATE Y:
# INPUTS : DATE X, DATE Y
# OUTPUTS : Currently returns trace data for the endDate
#*******************************************************

#rm(list=ls())

#imports
library(rlist)


traceDatesFilterAll <- function(datex = "2013-05-01", datey = "2016-05-03" )

if  (year(datey) <= 2016){
  
datex <- "2013-05-01"
datey <- "2016-05-03"
yeardiff <- 1 + year(datey) - year(datex) # one added to count for beginning year


td <- "Z:\\TraceData\\"
setwd(td)

#*************************************************************************************
# STEP: 1 GET me the yearList from dateX to dateY - Assgin a year filter from datex - datey
#**************************************************************************************
yearBegin <- toString(year(datex))
yearList <- list(yearBegin)
for (i  in  1:yeardiff-1){
  yearList<-list.append(yearList, toString(year(datex)+i))
}


#*************************************************************************************
# STEP: 2 GET me the folders from TraceDatafolder for specific dates in yearList
#**************************************************************************************
folders <- list()
for (i in 1:length(yearList)){
folders <- append(folders, list.files(td, pattern = yearList[[i]]))
}
folders <- folders[-grep(".zip", folders)] # remove files with .zip


#*************************************************************************************
# STEP: 3 GET me the list of files from yearFolder for specific dates in yearList
#**************************************************************************************
files <- lapply(folders, function (x) {list.files(path = paste(td, x, sep = ""))})
files_ <-as.data.frame(unlist(files)); colnames(files_) <- "filename"
names(files)<- folders

#local function to calculate length of each element of folders
b <- function(x) {l <- length(x);}
        #files_[1:l,"folder"] <- names(x); 
        #n <- l+1
          #print(l)}

length<- unlist(lapply(files, b)); cumsum(length)
endIndex <- cumsum(length); beginIndex <- 1+endIndex-length

# attach foldernames to the files_ frames
for (i in 1:length(folders)){
  files_[beginIndex[i]:endIndex[i],"folder"] <- names(files[i]);
}
list_files <- as.vector(files_[, 'filename']); 

# string split to get only dates part
 splits <- strsplit(list_files, "cusip-");split_2 <- rapply(splits, function(x) x[2]);
splits_1 <- strsplit(split_2, ".txt");
 
# attach dates to the files_ dataFrame
files_$dates <-splits_1; files_$dates <- as.Date(unlist(files_$dates))




#*****************************************************************
# STEP: 4 Get me the list of the files for the END Date
#****************************************************************
endDate_trace <- unique(filter(files_, dates == datex))
path <- paste(td, endDate_trace[1, 'folder'],  "\\",sep = "")
setwd(path); 

##****************************************************************
# WRAPUP: READ TRACE DATA FRAME
##***************************************************************
#Read files
system.time(data_traceTXT <-read.delim(file = as.character(endDate_trace[1,'filename']), header=T, sep="|",stringsAsFactors = FALSE))




} else {
  
  #*********************************************
  # STEP:1 GET A TRACE DATA FOLDER FOR END DATE
  #*********************************************
  yyyymmdd <- paste(substr(datey,1,4), substr(datey,6,7), substr(datey,9,10), sep ="" )
   td <-"Z:\\TraceData\\History\\"; folder <- traceDatesFilter(yyyymmdd, td)
  
  #****************************************************************
  # STEP:2 set path to the Folder and Search for date yyyymmdd file
  #****************************************************************
  path <- paste(td, folder, "\\", sep = "")
  data_csv <- list.files(path, pattern = paste("data","-" ,yyyymmdd,".csv", sep = ""))
  data_txt <- list.files(path, pattern = paste("tickmapping",".txt", sep = ""))
  
  ##****************************************************************
  # WRAPUP: READ TRACE DATA FRAME
  ##***************************************************************
  path <- paste(td, folder,  "\\",sep = "")
  setwd(path); 
  
  #Read filess
  system.time(data_traceCSV <- as.data.frame(fread(data_csv, header=FALSE, sep=",",stringsAsFactors = FALSE, skip =1)))
  system.time(data_traceTXT <-read.delim(file = as.character(data_txt), header=F, sep="|",stringsAsFactors = FALSE))
  
  # Renaming Colmns
  colnames(data_traceCSV) <- scan(data_csv, nlines = 1, what = character(), sep = ",")
  colnames(data_traceTXT) <- c("Cusip", "Description", "SYMBOL")
  
  
  #************************************************************************************
  # STEP: 5 ONLY FOR DATA AFTER 2016 - PUT a left join (by SYMBOL) for csv and txt data
  #***********************************************************************************
  p <- left_join(data_traceCSV,  data_traceTXT, by = "SYMBOL", copy = F)
  
}



