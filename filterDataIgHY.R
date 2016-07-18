# *********************************************************************************
# FILTER DATA FILES FROM BondData File - "CreditSights ... .csv"
# Inputs  <- get data table                        <- dataFrame
# Outputs -> set Investment grade datatable        <- Ig
#         -> set high Yield       datatable        <- Hy
#         -> return {Ig, Hy}  
#***********************************************************************************

filterDataIgHy <- function(FINALDF){
 
   iter <- 1:nrow(FINALDF)
    

    if(FALSE){ #*************** FOR DEBUG***************************************
    
    for(i in iter)
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
    dataFrameTrace <- merge.data.frame(dataFrameTrace, dataReadUsingDelim, by = "SYMBOL")
    
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
    dataFrame <- merge.data.frame(dataFrame, dataFrameTrace, by ="Cusip")
    
    
    #***************************
    # LEVEL:1 Preliminary Class
    #**************************
    # Filtering IG's for Top Change in OAS
    dataFrame[, "ChangeInOAS"] = dataFrame$OAS - dataFrame$`PrevMend OAS`
    #Ig <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'C0A0'ORDER BY `ChangeInOAS` DESC")
    
    # Filtering HY's for Top Change in Price
    dataFrame[, "ChangeInPrice"] = dataFrame$Price - dataFrame$`PrevMend Price`
    #Hy <- sqldf("SELECT * FROM dataFrame WHERE `Index Name` = 'H0A0'ORDER BY `ChangeInPrice` DESC")
    
    
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
    Ig <- sqldf("SELECT SYMBOL, CUSIP,  max(`CUM_VOL`) as TRC_VOl, ChangeInOAS as SpreadChange FROM dataFrame WHERE `Face Value LOC` >  1000 GROUP BY `SYMBOL` ORDER BY `ChangeInOAS` DESC")
    # Filtering Hys's for minimum Volumes
    Hy <- sqldf("SELECT SYMBOL, CUSIP,  max(`CUM_VOL`) as TRC_VOl, ChangeInPrice as PriceChange FROM dataFrame WHERE `Face Value LOC` >  500 GROUP BY `SYMBOL` ORDER BY `ChangeInPrice` DESC")
    
    #*********************
    # data NotFound Errors
    #*********************
    
    if (nrow(Hy) == 0){
      sprintf("Hy data isn't availaible in %s, looking over to file dir: {4, 6, 6N} ", fullyQualifiedFileName)
    }
    
    if (nrow(Ig) == 0){
      sprintf("Ig data isn't availaible in %s", fullyQualifiedFileName)
    }
    
  }
  

  return(Ig, Hy)
  
    }
    
    return(nrow(iter))#********************END DEBUG*****************************
}