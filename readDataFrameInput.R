
readDataFrameInput <- function(date_1) {

# Data file filter Step
dataFileFrame <- dataFileFrame_(path="Y:\\MLBondData\\", date_1 = date_1) # requires date in 'YYYYMMMDD' -


# Data Aggregation Step
dataFrame <- singleFrameForADate(path = path, dataFileFrame = dataFileFrame)

return(dataFrame)
}
