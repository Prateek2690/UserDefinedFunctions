#*********************************************************************
# CONSOLIDATE ML FILES FOR GIVEN DATE:
# Inputs <- path, date, input for files with Rebalanced constituents
# Outputs <- return frame{1_date, 2_date ......10_date}
#*********************************************************************


# For a given date list files in the directory
#rm(list = ls())

dataFileFrame_ <- function(path = "Y:\\MLBondData\\",  date_1 = "20160530",  input_filesWithRebalancedConstituents = 0) {
  files <- list.files(path, pattern =  date_1) # all files for a particular Date
  files_withRebalanced <- list.files(path, pattern =  paste("N_",date_1, sep="")) # all files with Rebalanced constituents for a given date
  files_OldConstituents <- files[lapply(files,function(x) length(grep("N_",x,value=FALSE))) == 0] # all files with Old Constituents for a given date
  
  
  #************************
  # GENERATE DATAFILE FRAME
  #************************
  dataFileFrame <- as.data.frame(files_OldConstituents)
  if (input_filesWithRebalancedConstituents == 1){
    dataFileFrame <- as.data.frame(files_withRebalanced)
  } # to work only with Rebalanced Constituents
  
  colnames(dataFileFrame) <- c("filename")
  
  # get me a pattern for all file having "N_"
  patN_ = grep("N_",dataFileFrame[,"filename"]); pat10_ <- grep("10_",dataFileFrame[,"filename"]); pat_0 <- grep(" 0_",dataFileFrame[,"filename"]);
  
  #IF DEGUG----------------------------------------------------------------------------------------------------------------------
  dataFileFrame["YYYY"] <- substr(dataFileFrame[,'filename'], 21, 24)
  dataFileFrame["MM"] <- substr(dataFileFrame[,'filename'], 25, 26)
  dataFileFrame["DD"] <- substr(dataFileFrame[,'filename'], 27, 28)
  dataFileFrame[c(patN_, pat10_, pat_0), "YYYY" ] <- substr(dataFileFrame[c(patN_, pat10_, pat_0) ,'filename'], 22, 25)
  dataFileFrame[c(patN_, pat10_, pat_0), "MM" ] <- substr(dataFileFrame[c(patN_, pat10_, pat_0) ,'filename'], 26, 27)
  dataFileFrame[c(patN_, pat10_, pat_0), "DD" ] <- substr(dataFileFrame[c(patN_, pat10_, pat_0) ,'filename'], 28, 29)
  
  
  #remove all entries of Index Dataframe with pat_0
  dataFileFrame <- dataFileFrame[-pat_0,]
  #------------------------------------------------------------------------------------------------------------------

  return (dataFileFrame)
  
  }
