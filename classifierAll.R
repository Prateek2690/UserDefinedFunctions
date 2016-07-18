#*****************************************************************
# Classifier:
# INPUTS: INDEX:
#     
#     {
#     1:BASED ON INDEX
#     2': BASED ON ISSUERS
#     2''': BASED ON LEVEL4
#     2'''': BASED ON MATURITY BUCKETS
#     2''''': BASED ON RATINGS BUCKETS
#     3: BASED ON SECTORS
#     }
#*****************************************************************

#imports
library(dplyr)
library(sqldf)



classifier <- function(index = "C0A0", output = "sector level 4", sector = "Banking") {

#INPUTS
#index <- "C0A0" # index input
#category #input
#sector #input
#maturity input

#******************************************
# STEP:1 BASED ON INDEX
#*****************************************   
data_index <- filter(dataFrame, `Index Name` == index)

#******************************************************
# STEP:2 BASED ON CATEGORY - 
#******************************************************

#level indicator based on levels
level4_cat <- unique(data_index['ML Industry Lvl 4'])
level3_cat <- unique(data_index['ML Industry Lvl 3'])
level2_cat <- unique(data_index['ML Industry Lvl 2'])
level1_cat <- unique(data_index['ML Industry Lvl 1'])


switch (output,
 
        "sector level 4" = {
          
          levelInd <- "ML Industry Lvl 4"
          level_cat <- level4_cat
          
          #******************************************************
          # STEP:3 BASED ON SECTOR - LEVEL 3
          #******************************************************
          sectorInd <- sector; sectorInd <- paste("'", sectorInd, "'", sep = "")
          sqlref <- paste("select * from data_index", " where ","`" ,levelInd, "`",  " = ", sectorInd, sep = "")
          data_sec <- sqldf(sqlref)
          
          # Reassign data_sec to index
          data_index <- data_sec
          
        },
        
        "sector level 3" = {
          levelInd <- "ML Industry Lvl 3"
          level_cat <- level3_cat
          
          
          #******************************************************
          # STEP:3 BASED ON SECTOR - LEVEL 3
          #******************************************************
          sectorInd <- sector; sectorInd <- paste("'", sectorInd, "'", sep = "")
          sqlref <- paste("select * from data_index", " where ","`" ,levelInd, "`",  " = ", sectorInd, sep = "")
          data_sec <- sqldf(sqlref)
          
          # Reassign data_sec to index
          data_index <- data_sec
          
        },
        
        "sector level 2" = {
          levelInd <- "ML Industry Lvl 2"
          level_cat <- level2_cat
          
          
          #******************************************************
          # STEP:3 BASED ON SECTOR - LEVEL 3
          #******************************************************
          sectorInd <- sector; sectorInd <- paste("'", sectorInd, "'", sep = "")
          sqlref <- paste("select * from data_index", " where ","`" ,levelInd, "`",  " = ", sectorInd, sep = "")
          data_sec <- sqldf(sqlref)
          
          # Reassign data_sec to index
          data_index <- data_sec
          
          
        },
        
        "sector level 1" = {
          levelInd <- "ML Industry Lvl 1"
          level_cat <- level1_cat
          
          
          
          #******************************************************
          # STEP:3 BASED ON SECTOR - LEVEL 3
          #******************************************************
          sectorInd <- sector; sectorInd <- paste("'", sectorInd, "'", sep = "")
          sqlref <- paste("select * from data_index", " where ","`" ,levelInd, "`",  " = ", sectorInd, sep = "")
          data_sec <- sqldf(sqlref)
          
          # Reassign data_sec to index
          data_index <- data_sec
          
          
        },
        
        "Maturity Buckets" = {
          
          # maturity indicator : Maturity - todays date
          dateFormated <- paste(substr(data_index$`Maturity Date`, 7, 10), "-",substr(data_index$`Maturity Date`, 1, 2), "-", substr(data_index$`Maturity Date`, 4, 5), sep = "") 
          data_index$dateFormated <- dateFormated
          data_index$matForBucket <- year(data_index$dateFormated) - year(Sys.Date())
          
          # DEBUG ------------------------------------
          maturityInd <-"1-3"
          if (maturityInd == "1-3"){
            # 1-3 := < 3
            data_mat <- sqldf("select * from data_index where (matForBucket < 3)")
            
          } else if (maturityInd == "3-5"){
            # 3-5 := 3 <= mat < 5
            data_mat <-  sqldf("select * from data_index where  matForBucket == 3 OR (matForBucket > 3 AND matForBucket < 5)")
            
          } else if (maturityInd == "5-7"){
            # 5-7 := 5 <= mat < 7
            data_mat <-  sqldf("select * from data_index where  matForBucket == 5 OR (matForBucket > 5 AND matForBucket < 7)")
            
          } else if (maturityInd == "7-10"){
            # 5-7 := 5 <= mat < 7
            data_mat <-  sqldf("select * from data_index where  matForBucket == 7 OR (matForBucket > 7 AND matForBucket < 10)")
            
          } else if (maturityInd == "10+"){
            # +10yrs := mat >= 10
            data_mat <-  sqldf("select * from data_index where  matForBucket == 10 OR (matForBucket > 10)")
          }
          #--------------------------------------------
          
          # Reassign data_sec to index
          data_index <- data_mat
          
          
          
        }
        
     
)


    
    
    
    #dur indicator based on duration
    #dur_5 <- data_index$`Mod Dur To Worst` < 5;
    #dur_5_10 <- 5 < data_index$`Mod Dur To Worst` < 10;
    #dur_10_15 <- 10 < data_index$`Mod Dur To Worst` < 15;
    #dur_15_20 <- 15 < data_index$`Mod Dur To Worst` < 20;
    
    
    
    # Rating indicator : Based on rating
    #rating_cat <- unique(data_index$Rating)
    
   
          
          
          
    return(data_index)
                    
}
          