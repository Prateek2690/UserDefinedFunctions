install.packages('xts')
install.packages('zoo')
install.packages('lubridate')

require(graphics)



## TS OBJECT
library(zoo)
library(lubridate)
library(xts)


#*****************************************************
# Date Time Series REquire: start,  end date
#*****************************************************

from ="2013-01-01"
to = "2014-01-01"


datesN0 <- function(from = from, to = to, datediff_ = 'days') {

  return(
    
    switch (datediff_,
            'days' = {
              
              return(as.numeric(as.Date(to) - as.Date(from))) # number of days
            },
            'weeks' = {
              
              return(difftime(strptime(to, format = "%Y-%m-%d"),
                              strptime(from, format = "%Y-%m-%d"),units="weeks")
              )
              
            },
            'months' = {
              
              return((as.yearmon(strptime(to, format = "%Y-%m-%d"))-
                        as.yearmon(strptime(from, format = "%Y-%m-%d")))*12)
              
            },
            'years' = {
              
              return(year(strptime(to, format = "%Y-%m-%d"))-
                       year(strptime(from, format = "%Y-%m-%d"))
              )
              
            },
            'quarters' = {
              
              return((as.yearqtr(strptime(to, format = "%Y-%m-%d"))-
                        as.yearqtr(strptime(from, format = "%Y-%m-%d")))*4)
              
            }
            
            
    )
    
  )
    
}


asDDMM <- function(dateOrg = '2013-04-02'){
  
  return(paste(paste(substr(dateOrg, 9,10), " ",month(as.Date('2013-04-02'), label = TRUE),sep ="")))
  
}

# seq of dates

seqOFDates<-function(from = from, to = to, datediff_ = 'day') {

  switch (datediff_,
    'day' = {
  
      
        # Daily
        datesAll <- seq.Date(from = as.Date(from), to = as.Date(to), by = 'day')
        
        datesChart  <- lapply(datesAll, asDDMM)
        
        #strip year part -  redundant and repeating
        return(
          list(datesAll, unlist(datesChart))
          )
      
          
    },
    'month' = {

      datesAll <- seq.Date(from = as.Date(from), to = as.Date(to), by = 'month')
      
      datesChart <- as.yearmon(datesAll)
      
      
      return(
        list(datesAll, unlist(datesChart))
      )
      
      
    },
    'year' = {
      
      datesAll <- seq.Date(from = as.Date(from), to = as.Date(to), by = 'year')
      
      datesChart <- as.yearmon(datesAll)
      
      
      return(
        list(datesAll, unlist(datesChart))
      )
      
    },
    'week' = {
      datesAll <- seq.Date(from = as.Date(from), to = as.Date(to), by = 'week')
      
      #datesChart <- as.yearmon(datesAll)
      datesChart  <- lapply(datesAll, asDDMM)
      
      
      
      return(
        list(datesAll, unlist(datesChart))
      )
      
    }
    
    
  )
  
  
}


seqOFDates(from = from, to = to, datediff_ = 'day')







## Using July 1954 as start date:
#gnp <- ts(cumsum(1 + round(rnorm(100), 2)),
#          start = c(1954, 7), frequency = 24)
#plot(gnp) # using 'plot.ts' for time-series plot


## TS TO DF
data.frame(Y=as.matrix(gnp), date=as.Date(as.yearmon(time(gnp))))
