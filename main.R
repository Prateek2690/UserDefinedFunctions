#**************************
# MAIN FILE: TOP TEN MOVERS
#**************************
rm(list=ls())

source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\fileDataFrame.r")
#source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\filterDataIgHy.r") # need to pass a Frame to a func
source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\writeToExcel.r")
source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\traceDatesFilter.r")


#Imports
library(xlsx) # library gdata
library(r2excel) # For Formatiing Output File
library(data.table) # fread def
library(sqldf) # for filtering data

#**********************************************************************************
#STEP:1 Get File DataFrame containing - {filenames, date -{YYYY, MM, DD}, TracePath}
#**********************************************************************************
FINALDF <- fileDataFrame("Y:\\MLBondData", "2016", "Z:\\TraceData\\History\\")


#***********************************************************************************
#STEP:2 Get IG & HY DataFrame for OutPuts - iterating over each File in the dataFrame
#***********************************************************************************
iterIg4 <-  grep(c("4_"), FINALDF$filename) # Number of Elements in having 4
iterIg4N <-  grep(c("4N_"), FINALDF$filename) # Number of Elements in having 4N

iterHy6 <-  grep(c("6_"), FINALDF$filename) # Number of Elements in having 4
iterHy6N <-  grep(c("6N_"), FINALDF$filename) # Number of Elements in having 4N


#iterIg <- 1:nrow(FINALDF) # ONLY qualified dataFiles from TraceVol #For IG
for(i in iterIg4)
{
  
  
  
  # *********************************************************************************
  # READ TRACEDATA FILES FROM ML_BONDDATA :: fileDir
  # Inputs  <- get directory Path                    <- "Z:\\TraceData\\History\\"
  #         <- get fileDataFrame                     <- {filename, YYYY,  MM,  DD}
  # Outputs -> return dataTable Trace                ->  traceDataFrame
  #***********************************************************************************
  
  # Read Trace Data
  setwd(FINALDF[i, "TracePath"]) # setWd
  
  # dateModification for Trace
  
  # FileReads
  filetxt <- "tickmapping.txt" # filename 
  
  # list files in tracePath
  filedata <- list.files(FINALDF[i, "TracePath"], pattern = ".csv")[1] #Just uses First File
  
  
  # using read.delim
  system.time(dataReadUsingDelim <-read.delim(filetxt, header=FALSE, sep="|",stringsAsFactors = FALSE))
  system.time(dataFrameTrace <- as.data.frame(fread(filedata, header=FALSE, sep=",",stringsAsFactors = FALSE, skip =1)))
  
  # Renaming Colmns
  colnames(dataFrameTrace) <- scan(filedata, nlines = 1, what = character(), sep = ",")
  colnames(dataReadUsingDelim) <- c("Cusip", "Description", "SYMBOL")
  
  # merger b/w TraceVol and tickMapper data
  # dataFrameTrace <- sql
  dataFrameTrace <- merge.data.frame(x = dataFrameTrace, y = dataReadUsingDelim, by.x = "SYMBOL", by.y = "SYMBOL", all.x = T)
  rm(dataReadUsingDelim) # remove redundant vars
  
  
  
  pmd <- "Y:\\Prateek\\" # Permanent Path Directory 
  setwd(pmd) # setting Permanent workingDirectory
  
  # filePath
  path <- "Y:\\MLBondData\\"
  
  # readCreditSights Bond Data - changeDirectory: path
  setwd(path); fullyQualifiedFileName <- paste(path,FINALDF[i, 'filename'], sep = "");  fullyQualifiedFileName1D <- paste(path,FINALDF[i, 'filename1D'], sep = "");
  system.time(dataFrame <- as.data.frame(fread(fullyQualifiedFileName, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ","))); Frame <- dataFrame[,-is.na.data.frame(dataFrame)]  #dataFrame <- dataFrame[,-ncol(dataFrame)] # remove "NA" Strings)
  
  
  #***************************************************************
  # CONVERTING DATA TO PROCESSIBLE DATA FRAMES FORMAT
  #***************************************************************                
  
  # ColNames
  colnames(dataFrame) <- scan(fullyQualifiedFileName, nlines = 1, what = "character", quiet = T, skip = 3, sep = ",")
  dataFrame['-D OAS'] <- as.data.frame(fread(fullyQualifiedFileName1D, stringsAsFactors =  FALSE, header = T, skip = 3, sep = ","))["OAS"]
  dataFrame <- merge.data.frame(x = dataFrame, y = dataFrameTrace, by.x ="Cusip", by.y = "Cusip", all.x = T)
  
  
  #***************************
  # LEVEL:1 Preliminary Class
  #**************************
  # Filtering IG's for Top Change in OAS
  dataFrame[, "ChangeInOAS"] = dataFrame$OAS - dataFrame$`-D OAS`
  #Ig <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'C0A0'")
  
  #***************************
  # LEVEL:2 Based on FV
  #**************************
  # Filtering IG's for threshold Face Value = 1000 
  #Ig <- sqldf("SELECT * FROM Ig  WHERE `Face Value LOC` >  1000")
  
  #********************************
  # LEVEL:3 Based on Trading Volume
  #********************************
  dataFrame$CUM_VOL <- dataFrame$CUM_VOL/10^3
  Ig <- sqldf("SELECT `Index Name`, CUSIP,  max(`CUM_VOL`) as TRC_VOl_inBillions, ChangeInOAS as SpreadChange, Price,`PrevMend Price` as LastPrice,  `Face Value LOC`, OAS , Ticker, Rating, `Par Wtd Coupon`, `Maturity Date`, `PrevMend OAS` as Lastspread FROM dataFrame WHERE `Index Name` = 'C0A0' AND (`Face Value LOC` >  1000 AND abs(ChangeInOAS) < 500)  GROUP BY `Cusip` ORDER BY `ChangeInOAS` DESC")
  
  #*********************
  # data NotFound Errors
  #*******************
  
  if (nrow(Ig) == 0){
    sprintf("Ig data isn't availaible in %s", fullyQualifiedFileName)
  }
  
  
  
  
  #*********************************
  #STEP:3 Top/bOttom 20 Movers table
  #*********************************
  top20Ig <- Ig[1:20,1:ncol(Ig)]
  bottom20Ig <- Ig[(nrow(Ig)-19):nrow(Ig), 1:ncol(Ig)]
  
  
  
  
  #***************************
  #STEP:3 EXPORT DATA TO EXCEL
  #***************************
  # function(fullyQualifiedWorkingDir, dataToEmbed, filename, sheetName, headerValue) 
  fullyQualifiedOutputWorkingDir <- "Y:\\Prateek\\Output"
  filename <- paste("IG",FINALDF[i, 'filename'], sep ='_')
  writeToExcel(fullyQualifiedOutputWorkingDir, top20Ig, bottom20Ig, filename, "Top_Bottom", "Top") #top20Ig
  
   }

for(i in iterIg4N)
{
  
  
  
  
  # *********************************************************************************
  # READ TRACEDATA FILES FROM ML_BONDDATA :: fileDir
  # Inputs  <- get directory Path                    <- "Z:\\TraceData\\History\\"
  #         <- get fileDataFrame                     <- {filename, YYYY,  MM,  DD}
  # Outputs -> return dataTable Trace                ->  traceDataFrame
  #***********************************************************************************
  
  # Read Trace Data
  setwd(FINALDF[i, "TracePath"]) # setWd
  
  # dateModification for Trace
  
  # FileReads
  filetxt <- "tickmapping.txt" # filename 
  
  # list files in tracePath
  filedata <- list.files(FINALDF[i, "TracePath"], pattern = ".csv")[1] #Just uses First File
  
  
  # using read.delim
  system.time(dataReadUsingDelim <-read.delim(filetxt, header=FALSE, sep="|",stringsAsFactors = FALSE))
  system.time(dataFrameTrace <- as.data.frame(fread(filedata, header=FALSE, sep=",",stringsAsFactors = FALSE, skip =1)))
  
  # Renaming Colmns
  colnames(dataFrameTrace) <- scan(filedata, nlines = 1, what = character(), sep = ",")
  colnames(dataReadUsingDelim) <- c("Cusip", "Description", "SYMBOL")
  
  # merger b/w TraceVol and tickMapper data
  # dataFrameTrace <- sql
  dataFrameTrace <- merge.data.frame(x = dataFrameTrace, y = dataReadUsingDelim, by.x = "SYMBOL", by.y = "SYMBOL", all.x = T)
  rm(dataReadUsingDelim) # remove redundant vars
  
  
  
  pmd <- "Y:\\Prateek\\" # Permanent Path Directory 
  setwd(pmd) # setting Permanent workingDirectory
  
  # filePath
  path <- "Y:\\MLBondData\\"
  
  # readCreditSights Bond Data - changeDirectory: path
  setwd(path); fullyQualifiedFileName <- paste(path,FINALDF[i, 'filename'], sep = "");  fullyQualifiedFileName1D <- paste(path,FINALDF[i, 'filename1D'], sep = "");
  system.time(dataFrame <- as.data.frame(fread(fullyQualifiedFileName, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ","))); Frame <- dataFrame[,-is.na.data.frame(dataFrame)]  #dataFrame <- dataFrame[,-ncol(dataFrame)] # remove "NA" Strings)
  
  
  #***************************************************************
  # CONVERTING DATA TO PROCESSIBLE DATA FRAMES FORMAT
  #***************************************************************                
  
  # ColNames
  colnames(dataFrame) <- scan(fullyQualifiedFileName, nlines = 1, what = "character", quiet = T, skip = 3, sep = ",")
  dataFrame['-D OAS'] <- as.data.frame(fread(fullyQualifiedFileName1D, stringsAsFactors =  FALSE, header = T, skip = 3, sep = ","))["OAS"]
  dataFrame <- merge.data.frame(x = dataFrame, y = dataFrameTrace, by.x ="Cusip", by.y = "Cusip", all.x = T)
  
  
  #***************************
  # LEVEL:1 Preliminary Class
  #**************************
  # Filtering IG's for Top Change in OAS
  dataFrame[, "ChangeInOAS"] = dataFrame$OAS - dataFrame$`-D OAS`
  #Ig <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'C0A0'")
  
  #***************************
  # LEVEL:2 Based on FV
  #**************************
  # Filtering IG's for threshold Face Value = 1000 
  #Ig <- sqldf("SELECT * FROM Ig  WHERE `Face Value LOC` >  1000")
  
  #********************************
  # LEVEL:3 Based on Trading Volume
  #********************************
  dataFrame$CUM_VOL <- dataFrame$CUM_VOL/10^3
  Ig <- sqldf("SELECT `Index Name`, CUSIP,  max(`CUM_VOL`) as TRC_VOl_inBillions, ChangeInOAS as SpreadChange, Price,`PrevMend Price` as LastPrice,  `Face Value LOC`, OAS , Ticker, Rating, `Par Wtd Coupon`, `Maturity Date`, `PrevMend OAS` as Lastspread FROM dataFrame WHERE `Index Name` = 'C0A0' AND (`Face Value LOC` >  1000 AND abs(ChangeInOAS) < 500)  GROUP BY `Cusip` ORDER BY `ChangeInOAS` DESC")
  
  #*********************
  # data NotFound Errors
  #*******************
  
  if (nrow(Ig) == 0){
    sprintf("Ig data isn't availaible in %s", fullyQualifiedFileName)
  }
  
  
  
  
  #*********************************
  #STEP:3 Top/bOttom 20 Movers table
  #*********************************
  top20Ig <- Ig[1:20,1:ncol(Ig)]
  bottom20Ig <- Ig[(nrow(Ig)-19):nrow(Ig), 1:ncol(Ig)]
  
  
  
  
  #***************************
  #STEP:3 EXPORT DATA TO EXCEL
  #***************************
  # function(fullyQualifiedWorkingDir, dataToEmbed, filename, sheetName, headerValue) 
  fullyQualifiedOutputWorkingDir <- "Y:\\Prateek\\Output"
  filename <- paste("IG",FINALDF[i, 'filename'], sep ='_')
  writeToExcel(fullyQualifiedOutputWorkingDir, top20Ig, bottom20Ig, filename, "Top_Bottom", "Top") #top20Ig
  
}

for(i in iterHy6)
{
  
  
  
  # *********************************************************************************
  # READ TRACEDATA FILES FROM ML_BONDDATA :: fileDir
  # Inputs  <- get directory Path                    <- "Z:\\TraceData\\History\\"
  #         <- get fileDataFrame                     <- {filename, YYYY,  MM,  DD}
  # Outputs -> return dataTable Trace                ->  traceDataFrame
  #***********************************************************************************
  
  # Read Trace Data
  setwd(FINALDF[i, "TracePath"]) # setWd
  
  # dateModification for Trace
  
  # FileReads
  filetxt <- "tickmapping.txt" # filename 
  
  # list files in tracePath
  filedata <- list.files(FINALDF[i, "TracePath"], pattern = ".csv")[1] #Just uses First File
  
  
  # using read.delim
  system.time(dataReadUsingDelim <-read.delim(filetxt, header=FALSE, sep="|",stringsAsFactors = FALSE))
  system.time(dataFrameTrace <- as.data.frame(fread(filedata, header=FALSE, sep=",",stringsAsFactors = FALSE, skip =1)))
  
  # Renaming Colmns
  colnames(dataFrameTrace) <- scan(filedata, nlines = 1, what = character(), sep = ",")
  colnames(dataReadUsingDelim) <- c("Cusip", "Description", "SYMBOL")
  
  dataT <- dataFrameTrace  
  
  # merger b/w TraceVol and tickMapper data
  dataFrameTrace <- merge.data.frame(x = dataFrameTrace, y = dataReadUsingDelim, by.x = "SYMBOL", by.y = "SYMBOL", all.x = T)
  
  
  rm(dataReadUsingDelim) # remove redundant vars
  
  # OuterJoin with MlData
  
  pmd <- "Y:\\Prateek\\" # Permanent Path Directory 
  
  setwd(pmd) # setting Permanent workingDirectory
  
  # filePath
  path <- "Y:\\MLBondData\\"
  
  # readCreditSights Bond Data - changeDirectory: path
  setwd(path); fullyQualifiedFileName <- paste(path,FINALDF[i, 'filename'], sep = ""); 
  system.time(dataFrame <- as.data.frame(fread(fullyQualifiedFileName, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ","))); Frame <- dataFrame[,-is.na.data.frame(dataFrame)]  #dataFrame <- dataFrame[,-ncol(dataFrame)] # remove "NA" Strings)
  
  
  
  #***************************************************************
  # CONVERTING DATA TO PROCESSIBLE DATA FRAMES FORMAT
  #***************************************************************                
  
  # ColNames
  colnames(dataFrame) <- scan(fullyQualifiedFileName, nlines = 1, what = "character", quiet = T, skip = 3, sep = ",")
  #colnames(traceVol) <- scan("Z:/TraceData/History/2016 02 23/data-20160212.csv", nlines = 1, what = "character", quiet = T, skip = 0, sep = ",")
  
  dataF <- dataFrame
  dataFrame <- merge.data.frame(x = dataFrame, y = dataFrameTrace, by.x ="Cusip", by.y = "Cusip", all.x = T)
  
  
  
  #***************************
  # LEVEL:1 Preliminary Class
  #**************************
  dataFrame[, "ChangeInOAS"] = dataFrame$OAS - dataFrame$`PrevMend OAS`
  dataFrame[, "ChangeInPrice"] = dataFrame$Price - dataFrame$`PrevMend Price`
  #Hy <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'H0A0'")
  
  
  #***************************
  # LEVEL:2 Based on FV
  #**************************
  # Filtering Hys's for threshold Face Value = 500
  #Hy <- sqldf("SELECT * FROM Hy  WHERE `Face Value LOC` >  500")
  
  
  #********************************
  # LEVEL:3 Based on Trading Volume
  #********************************
  # Filtering Hys's for minimum Volumes
  dataFrame$CUM_VOL <- dataFrame$CUM_VOL/10^3
  Hy <- sqldf("SELECT `Index Name`, CUSIP,  max(`CUM_VOL`) as TRC_VOl_inBillions, ChangeInPrice as PriceChange, Price, `PrevMend Price` as LastPrice, `Face Value LOC`, OAS,  Ticker, Rating, `Par Wtd Coupon`, `Maturity Date`, `PrevMend OAS` as Lastspread FROM dataFrame WHERE `Index Name` = 'H0A0' AND (`Face Value LOC` >  500 AND abs(ChangeInOAS) < 500)  GROUP BY `SYMBOL` ORDER BY `ChangeInPrice` DESC" )
  
  #*********************
  # data NotFound Errors
  #*********************
  
  if (nrow(Hy) == 0){
    sprintf("Hy data isn't availaible in %s, looking over to file dir: {4, 6, 6N} ", fullyQualifiedFileName)
  }
  
  
  
  #*********************************
  #STEP:3 Top/bOttom 20 Movers table
  #*********************************
  top20Hy <- Hy[1:20,1:ncol(Hy)]
  bottom20Hy <- Hy[(nrow(Hy)-19):nrow(Hy), 1:ncol(Hy)]
  
  
  
  
  
  #***************************
  #STEP:3 EXPORT DATA TO EXCEL
  #***************************
  # function(fullyQualifiedWorkingDir, dataToEmbed, filename, sheetName, headerValue) 
  fullyQualifiedOutputWorkingDir <- "Y:\\Prateek\\Output"
  filename <- paste("HY",FINALDF[i,'filename'], sep ='_')
  writeToExcel(fullyQualifiedOutputWorkingDir, top20Hy, bottom20Hy, filename, "Top_Bottom", "Top") #top20Ig
  
}

for(i in iterHy6N)
{
  
  
  
  # *********************************************************************************
  # READ TRACEDATA FILES FROM ML_BONDDATA :: fileDir
  # Inputs  <- get directory Path                    <- "Z:\\TraceData\\History\\"
  #         <- get fileDataFrame                     <- {filename, YYYY,  MM,  DD}
  # Outputs -> return dataTable Trace                ->  traceDataFrame
  #***********************************************************************************
  
  # Read Trace Data
  setwd(FINALDF[i, "TracePath"]) # setWd
  
  # dateModification for Trace
  
  # FileReads
  filetxt <- "tickmapping.txt" # filename 
  
  # list files in tracePath
  filedata <- list.files(FINALDF[i, "TracePath"], pattern = ".csv")[1] #Just uses First File
  
  
  # using read.delim
  system.time(dataReadUsingDelim <-read.delim(filetxt, header=FALSE, sep="|",stringsAsFactors = FALSE))
  system.time(dataFrameTrace <- as.data.frame(fread(filedata, header=FALSE, sep=",",stringsAsFactors = FALSE, skip =1)))
  
  # Renaming Colmns
  colnames(dataFrameTrace) <- scan(filedata, nlines = 1, what = character(), sep = ",")
  colnames(dataReadUsingDelim) <- c("Cusip", "Description", "SYMBOL")
  
  dataT <- dataFrameTrace  
  
  # merger b/w TraceVol and tickMapper data
  dataFrameTrace <- merge.data.frame(x = dataFrameTrace, y = dataReadUsingDelim, by.x = "SYMBOL", by.y = "SYMBOL", all.x = T)
  
  
  rm(dataReadUsingDelim) # remove redundant vars
  
  # OuterJoin with MlData
  
  pmd <- "Y:\\Prateek\\" # Permanent Path Directory 
  
  setwd(pmd) # setting Permanent workingDirectory
  
  # filePath
  path <- "Y:\\MLBondData\\"
  
  # readCreditSights Bond Data - changeDirectory: path
  setwd(path); fullyQualifiedFileName <- paste(path,FINALDF[i, 'filename'], sep = ""); 
  system.time(dataFrame <- as.data.frame(fread(fullyQualifiedFileName, stringsAsFactors =  FALSE, header = F, skip = 4, sep = ","))); Frame <- dataFrame[,-is.na.data.frame(dataFrame)]  #dataFrame <- dataFrame[,-ncol(dataFrame)] # remove "NA" Strings)
  
  
  
  #***************************************************************
  # CONVERTING DATA TO PROCESSIBLE DATA FRAMES FORMAT
  #***************************************************************                
  
  # ColNames
  colnames(dataFrame) <- scan(fullyQualifiedFileName, nlines = 1, what = "character", quiet = T, skip = 3, sep = ",")
  #colnames(traceVol) <- scan("Z:/TraceData/History/2016 02 23/data-20160212.csv", nlines = 1, what = "character", quiet = T, skip = 0, sep = ",")
  
  dataF <- dataFrame
  dataFrame <- merge.data.frame(x = dataFrame, y = dataFrameTrace, by.x ="Cusip", by.y = "Cusip", all.x = T)
  
  
  #***************************
  # LEVEL:1 Preliminary Class
  #**************************
  dataFrame[, "ChangeInOAS"] = dataFrame$OAS - dataFrame$`PrevMend OAS`
  dataFrame[, "ChangeInPrice"] = dataFrame$Price - dataFrame$`PrevMend Price`
  #Hy <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'H0A0'")
  
  
  #***************************
  # LEVEL:2 Based on FV
  #**************************
  # Filtering Hys's for threshold Face Value = 500
  #Hy <- sqldf("SELECT * FROM Hy  WHERE `Face Value LOC` >  500")
  
  
  #********************************
  # LEVEL:3 Based on Trading Volume
  #********************************
  # Filtering Hys's for minimum Volumes
  dataFrame$CUM_VOL <- dataFrame$CUM_VOL/10^3
  Hy <- sqldf("SELECT `Index Name`, CUSIP,  max(`CUM_VOL`) as TRC_VOl_inBillions, ChangeInPrice as PriceChange, Price, `PrevMend Price` as LastPrice, `Face Value LOC`, OAS,  Ticker, Rating, `Par Wtd Coupon`, `Maturity Date`, `PrevMend OAS` as Lastspread FROM dataFrame WHERE `Index Name` = 'H0A0' AND (`Face Value LOC` >  500 AND abs(ChangeInOAS) < 500)  GROUP BY `SYMBOL` ORDER BY `ChangeInPrice` DESC" )
  
  #*********************
  # data NotFound Errors
  #*********************
  
  if (nrow(Hy) == 0){
    sprintf("Hy data isn't availaible in %s, looking over to file dir: {4, 6, 6N} ", fullyQualifiedFileName)
  }
  
  
  
  #*********************************
  #STEP:3 Top/bOttom 20 Movers table
  #*********************************
  top20Hy <- Hy[1:20,1:ncol(Hy)]
  bottom20Hy <- Hy[(nrow(Hy)-19):nrow(Hy), 1:ncol(Hy)]
  
  
  
  
  
  #***************************
  #STEP:3 EXPORT DATA TO EXCEL
  #***************************
  # function(fullyQualifiedWorkingDir, dataToEmbed, filename, sheetName, headerValue) 
  fullyQualifiedOutputWorkingDir <- "Y:\\Prateek\\Output"
  filename <- paste("HY",FINALDF[i,'filename'], sep ='_')
  writeToExcel(fullyQualifiedOutputWorkingDir, top20Hy, bottom20Hy, filename, "Top_Bottom", "Top") #top20Ig
  
}



#**********************************
# CHARTS: 3D Plots
#***********************************
# 3D Scatterplot
library(scatterplot3d)
attach(Hy)
scatterplot3d(TRC_VOl,Price,PriceChange, main="3D Scatterplot")