rm(list=ls())

#install.packages('sqlutils')
#install.packages('sqldf')
#install.packages('RSQLite')


library(sqlutils)
library(sqldf)
library("RSQLite")
library(DBI)


# Imports
source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\readDataFrameInput.r")
source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\ConsolidateMlFilesForAGivenDate.R")
source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\makeLargeSingleFrameForAGivenDate.R")
source("Y:\\Prateek\\TopTenMovers_FinalizedFunctionFiles\\classifierAll.R")


#*******************************************
# Extracting Dates
#*******************************************
path <- 'Y:\\MLBondData\\'; setwd(path)
filedates <- list.files(path = path)
filedates <- strsplit(filedates,"_")
filedates <- rapply(filedates, function(x) {x[2]})
filedates <- strsplit(filedates, ".csv"); filedates <-  unique(rapply(filedates, function(x) {x[1]}))


## BEGIN ----------------------------------------

#*****************************************
# setup  a connection  to sqlLite database
#*****************************************
# connect to the sqlite file
setwd('C:\\Program Files\\R\\R-3.3.0\\library\\sqlutils\\db\\')
conn = dbConnect(SQLite(),dbname="students.db")
# get a list of all tables
alltables = dbListTables(conn)
conn <- dbConnect(SQLite(),"mlBondataIndex.sqlite3")
#conn = dbConnect(RSQLite::SQLite(), dbname="mlBond.sqlite")
# get a list of all tables
alltables = dbListTables(conn)


#sqlutils::sqlPaths()

## function Beginning-----------------------------------------
getDataIntoDataBase <- function(date){

#***********************************
# setUp file Structure in mlData
#***********************************
namePattern <- 'Creditsights'

## DEBUG----------------------------------------
#date <-'20160504'
fullName <- paste(namePattern, date, sep = "_")

## EXTRA---------------------------------------------------
# get the populationtable as a data.frame
#p1 = dbGetQuery( conn,'select * from dataFrame' )
# count the areas in the SQLite table
#p2 = dbGetQuery( conn,'select count(*) from dataFrame' )
# find entries of the DB from the last week
#p3 = dbGetQuery( conn, "SELECT population WHERE DATE(timeStamp) < DATE('now', 'weekday 0', '-7 days')")
#Clear the results of the last query
#dbClearResult(p3)
#Select population with managerial type of job
#p4 = dbGetQuery( conn, "select * from dataFrame where `Index Name` = 'C0A0'")


## DataFrame------------------------------------
frame <- readDataFrameInput(date_1 = date)

## WRITEDATA-------------------------------------------------
#******************************************
# WriteTable :
#******************************************
# write dataFrame to dataBase sql
dbWriteTable(conn, fullName, frame)



#******************************************
# removeTable 
#******************************************
#dbRemoveTable(conn, fullName)



## READING----------------------------------------

#******************************************
# readTable :
#******************************************
#dbReadTable(conn, fullName)

} ## function End------------------------------------------------





#*****************************************
# disconnect sqlLite database
#*****************************************
dbDisconnect(conn)


lapply(filedates[1:6], getDataIntoDataBase)

# Data file filter Step
dataFileFrame <- dataFileFrame_(path="Y:\\MLBondData\\", date_1 = filedates[1]) # requires date in 'YYYYMMMDD' -

getDataIntoDataBase(filedates[1])


