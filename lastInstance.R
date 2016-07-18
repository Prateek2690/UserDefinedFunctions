


# *********************************************************************************
# READ TRACEDATA FILES FROM ML_BONDDATA :: fileDir
# Inputs  <- get directory Path                    <- "Z:\\TraceData\\History\\"
#         <- get fileDataFrame                     <- {filename, YYYY,  MM,  DD}
# Outputs -> return dataTable Trace                ->  traceDataFrame
#***********************************************************************************

# Read Trace Data
setwd(FINALDF[1, "TracePath"]) # setWd

# dateModification for Trace

# FileReads
filetxt <- "tickmapping.txt" # filename 

# list files in tracePath
filedata <- list.files(FINALDF[1, "TracePath"], pattern = ".csv")[1] #Just uses First File


# using read.delim
system.time(dataReadUsingDelim <-read.delim(filetxt, header=FALSE, sep="|",stringsAsFactors = FALSE))
system.time(dataFrameTrace <- as.data.frame(fread(filedata, header=FALSE, sep=",",stringsAsFactors = FALSE, skip =1)))

# Renaming Colmns
colnames(dataFrameTrace) <- scan(filedata, nlines = 1, what = character(), sep = ",")
colnames(dataReadUsingDelim) <- c("Cusip", "Description", "SYMBOL")

dataT <- dataFrameTrace  

# merger b/w TraceVol and tickMapper data
dataFrameTrace <- merge.data.frame(dataFrameTrace, dataReadUsingDelim, by = "SYMBOL")

rm(dataReadUsingDelim) # remove redundant vars

# OuterJoin with MlData

pmd <- "Y:\\Prateek\\" # Permanent Path Directory 

setwd(pmd) # setting Permanent workingDirectory

# filePath
path <- "Y:\\MLBondData\\"

# readCreditSights Bond Data - changeDirectory: path
setwd(path); fullyQualifiedFileName <- paste(path,FINALDF[1, 'filename'], sep = ""); 
system.time(dataFrame <- as.data.frame(fread(fullyQualifiedFileName, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ","))); Frame <- dataFrame[,-is.na.data.frame(dataFrame)]  #dataFrame <- dataFrame[,-ncol(dataFrame)] # remove "NA" Strings)



#***************************************************************
# CONVERTING DATA TO PROCESSIBLE DATA FRAMES FORMAT
#***************************************************************                

# ColNames
colnames(dataFrame) <- scan(fullyQualifiedFileName, nlines = 1, what = "character", quiet = T, skip = 3, sep = ",")
#colnames(traceVol) <- scan("Z:/TraceData/History/2016 02 23/data-20160212.csv", nlines = 1, what = "character", quiet = T, skip = 0, sep = ",")

dataF <- dataFrame
dataFrame <- merge.data.frame(dataFrame, dataFrameTrace, by ="Cusip")


#***************************
# LEVEL:1 Preliminary Class
#**************************
# Filtering IG's for Top Change in OAS
dataFrame[, "ChangeInOAS"] = dataFrame$OAS - dataFrame$`PrevMend OAS`
#Ig <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'C0A0'")

# Filtering HY's for Top Change in Price
dataFrame[, "ChangeInPrice"] = dataFrame$Price - dataFrame$`PrevMend Price`
#Hy <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'H0A0'")


#***************************
# LEVEL:2 Based on FV
#**************************
# Filtering IG's for threshold Face Value = 1000 
#Ig <- sqldf("SELECT * FROM Ig  WHERE `Face Value LOC` >  1000")

# Filtering Hys's for threshold Face Value = 500
#Hy <- sqldf("SELECT * FROM Hy  WHERE `Face Value LOC` >  500")


#********************************
# LEVEL:3 Based on Trading Volume
#********************************
Ig <- sqldf("SELECT `Index Name`, SYMBOL, CUSIP,  max(`CUM_VOL`) as TRC_VOl, ChangeInOAS as SpreadChange FROM dataFrame WHERE `Index Name` = 'C0A0' AND (`Face Value LOC` >  1000)  GROUP BY `SYMBOL` ORDER BY `ChangeInOAS` DESC")
# Filtering Hys's for minimum Volumes
Hy <- sqldf("SELECT `Index Name`, SYMBOL, CUSIP,  max(`CUM_VOL`) as TRC_VOl, ChangeInPrice as PriceChange FROM dataFrame WHERE `Index Name` = 'H0A0' AND (`Face Value LOC` >  500) GROUP BY `SYMBOL` ORDER BY `ChangeInPrice` DESC")

#*********************
# data NotFound Errors
#*********************

if (nrow(Hy) == 0){
  sprintf("Hy data isn't availaible in %s, looking over to file dir: {4, 6, 6N} ", fullyQualifiedFileName)
}

if (nrow(Ig) == 0){
  sprintf("Ig data isn't availaible in %s", fullyQualifiedFileName)
}




#*********************************
#STEP:3 Top/bOttom 20 Movers table
#*********************************
top20Ig <- Ig[1:20,1:ncol(Ig)]
bottom20Ig <- Ig[(nrow(Ig)-19):nrow(Ig), 1:ncol(Ig)]
top20Hy <- Hy[1:20,1:ncol(Hy)]
bottom20Hy <- Hy[(nrow(Ig)-19):nrow(Hy), 1:ncol(Hy)]






#***************************
#STEP:3 EXPORT DATA TO EXCEL
#***************************
# function(fullyQualifiedWorkingDir, dataToEmbed, filename, sheetName, headerValue) 
fullyQualifiedOutputWorkingDir <- "Y:\\Prateek\\Output"
filename <- paste("IG",FINALDF[1,'filename'], sep ='_')
writeToExcel(fullyQualifiedOutputWorkingDir, top20Ig, bottom20Ig, filename, "Top_Bottom", "Top") #top20Ig

filename <- paste("HY",FINALDF[1,'filename'], sep ='_')
writeToExcel(fullyQualifiedOutputWorkingDir, top20Hy, bottom20Hy, filename, "Top_Bottom", "Top") #top20Ig
