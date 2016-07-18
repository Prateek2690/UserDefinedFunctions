# *********************************************************************************
# READ DATA FILES FROM ML_BONDDATA :: fileDataFrame
# Inputs  <- get directory Path                    <- Y:\\MLBondData
#         <- get year                              <- ""YYYY MM DD""
#         -> get traceDir  (trace data directory)        <- "Z:\\TraceData\\History\\"
# Outputs -> set pmd (Permanent Working Directory) <- "Y:\\Prateek\\"
#         -> return dataFileFrame : {filename, YYYY, MM, DD, TraceDataPath}
#         $ DATE FILTER NOT USED HOWEVER, WILL BE AVAILABLE ASAP$
#***********************************************************************************

# fileDataFrame

fileDataFrame <- function(path = "Y:\\MLBondData\\", year = "2016", traceDir = "Z:\\TraceData\\History\\"){

  # File Beginning
  Begin <- "CreditSights Bond"
  
  # For Year 
  #year <- "2016";
  pattern  <- paste(Begin, list("4", "4N", "6", "6N")); pattern <- paste(pattern, "_" , year, sep = "" )
  
  
  # Currently, ONLY FOR year Data:
  fileList <- list.files(path,pattern = pattern[1]) #files with extention: 4
  fileList <- append(fileList, list.files(path,pattern =pattern[2])) #files with extention: 4N
  fileList <- append(fileList, list.files(path,pattern = pattern[3])) #files with extention: 6
  fileList <- append(fileList, list.files(path,pattern = pattern[4]))  #files with extention: 6N
  
  #************************
  # GENERATE DATAFILE FRAME
  #************************
  dataFileFrame <- as.data.frame(fileList)
  colnames(dataFileFrame) <- c("filename")
  
  # Also Returns Trace Inputs - YYYY, MM, DD
  # For 4N and 6N
  pat_4N = grep("4N",dataFileFrame[,"filename"])
  pat_6N = grep("6N",dataFileFrame[,"filename"])
  
  dataFileFrame["YYYY"] <- substr(dataFileFrame[,'filename'], 21, 24)
  dataFileFrame["MM"] <- substr(dataFileFrame[,'filename'], 25, 26)
  dataFileFrame["DD"] <- substr(dataFileFrame[,'filename'], 27, 28)
  dataFileFrame[c(pat_4N,pat_6N), "YYYY" ] <- substr(dataFileFrame[c(pat_4N,pat_6N) ,'filename'], 22, 25)
  dataFileFrame[c(pat_4N,pat_6N), "MM" ] <- substr(dataFileFrame[c(pat_4N,pat_6N) ,'filename'], 26, 27)
  dataFileFrame[c(pat_4N,pat_6N), "DD" ] <- substr(dataFileFrame[c(pat_4N,pat_6N) ,'filename'], 28, 29)
  
  yyyy_1 <- substr(as.Date(paste(dataFileFrame[,'YYYY'], dataFileFrame[,'MM'], dataFileFrame[,'DD'], sep = "-" )) - 1, 1, 4)
  mm_1   <- substr(as.Date(paste(dataFileFrame[,'YYYY'], dataFileFrame[,'MM'], dataFileFrame[,'DD'], sep = "-" )) - 1, 6, 7)
  dd_1 <- substr(as.Date(paste(dataFileFrame[,'YYYY'], dataFileFrame[,'MM'], dataFileFrame[,'DD'], sep = "-" )) - 1, 9, 10)
  
  
  
  
  # YYYYMMDD formatraceDirate in filedataframe
  dataFileFrame["YYYYMMDD"] <- paste(dataFileFrame[,'YYYY'], dataFileFrame[,'MM'], dataFileFrame[,'DD'], sep = "" )
  dataFileFrame["YYYYMMDD_1"] <- paste(yyyy_1, mm_1, dd_1, sep = "") # Prior day
   dataFileFrame[,'filename1D'] <- paste(substr(dataFileFrame[,'filename'], 1, 20), dataFileFrame[, 'YYYYMMDD_1'], ".csv", sep ="")
   dataFileFrame[c(pat_4N,pat_6N),'filename1D'] <- paste(substr(dataFileFrame[c(pat_4N,pat_6N),'filename'], 1, 21), dataFileFrame[c(pat_4N,pat_6N), 'YYYYMMDD_1'], ".csv", sep ="")
   
  # Trace Directory
  #traceDir <- "Z:\\TraceData\\History\\"
  trc_folder <- list()
    
  for (i in dataFileFrame[, 'YYYYMMDD']) {
  trc_folder <- append(trc_folder, traceDatesFilter(i, traceDir))
  }
  
  #return(trc_folder)
  
  #if(FALSE) { # DEBUG -----------------------------------
  
  # Read Trace Data
  # Modified Trace Folder Path
  dataFileFrame$TracePath <- paste(traceDir, trc_folder, sep ="")
  
  # TraceFilter For For Loop
  filterTraceVol <- paste(traceDir,list.files(traceDir), sep ="")
  idx <- match(dataFileFrame$TracePath,filterTraceVol); idx <- filterTraceVol[match(dataFileFrame$TracePath,filterTraceVol) ] 
  dataFileFrame$idx <- idx; FINALDF <- sqldf("select * from dataFileFrame where `idx` != 'NA'")
  

return(FINALDF)

  #} # END DEBUG---------------------------------------------
}