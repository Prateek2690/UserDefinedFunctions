#***********************************************************
# ReadIn ML FileS after Consolidation
# Inputs : path, dataFileFrame
# Outputs : Frame having :{1_date, 2_date, 3_date....10_date}
#************************************************************

# Call to consolidateFiles to build the dataFileFrame

# Imports 
library(data.table)


singleFrameForADate <- function(path = "Y:\\MLBondData\\", dataFileFrame = dataFileFrame) {

# filePath
path <- "Y:\\MLBondData\\"

# readCreditSights Bond Data - changeDirectory: path
setwd(path); fullyQualifiedFileName <- paste(path,dataFileFrame[, 'filename'], sep = ""); 
#system.time(dataFrame <- as.data.frame(fread(fullyQualifiedFileName, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ","))); Frame <- dataFrame[,-is.na.data.frame(dataFrame)]  #dataFrame <- dataFrame[,-ncol(dataFrame)] # remove "NA" Strings)

#***************************************
# READING ALL DATA FOR A GIVEN DATE AND
# CONVERTING INTO A SINGLE DATAFRAME
#***************************************
dataFrame <- as.data.frame(list())
dataFrame <- do.call(rbind.data.frame, lapply(fullyQualifiedFileName, function(x) {rbind(dataFrame, as.data.frame(fread(x, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ",")))})) 
str(dataFrame)

# ColNames
colnames(dataFrame) <- scan(fullyQualifiedFileName[1], nlines = 1, what = "character", quiet = T, skip = 3, sep = ",")
drops <- c("") # eliminates NA columns
dataFrame <- dataFrame[, !(names(dataFrame) %in% drops)]
 
return(dataFrame) 

}
 
