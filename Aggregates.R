#******************************************************
# AGGREGATES : BY SECTOR, BY RATING - GROUP BY MATURITY
#******************************************************

Aggregates <- function( bySector = TRUE, byRating = FALSE, object_){
  
  if(bySector){
    
    switch (object_,
      'US HY' = {
        
        #************************************************
        # Fields: `Index Name` and `Sector Representation`
        #************************************************
        
        
        
        #  US HY sector frame
        US_HY <- as.data.frame(list());
        
        nameList <- c('mlh0a0',
                      'mlh0a1',
                      'mlh0a2',
                      'mlh0a3',
                      'mlhwus',
                      'mlh0pu',
                      'mlh0br',
                      'mlh0pa',
                      'mlh0ra',
                      'mlh0re',
                      'mlh0en',
                      'mlh0ch',
                      'mlh0bl',
                      'mlh0in',
                      'mlh0ct',
                      'mlh0sh',
                      'mlh0co',
                      'mlh0hb',
                      'mlh0ca',
                      'mlh0cv',
                      'mlh0se',
                      'mlh0ah',
                      'mlh0fo',
                      'mlh0au',
                      'mlh0ai',
                      'mlh0ag',
                      'mlh0ba',
                      'mlh0el',
                      'mlh0dm',
                      'mlh0tc',
                      'mlh0ae',
                      'mlh0ty',
                      'mlh0ev',
                      'mlh0fi',
                      'mlh0le',
                      'mlh0et',
                      'mlh0hl',
                      'mlh0fr',
                      'mlh0st',
                      'mlh0me',
                      'mlh0sr',
                      'mlhcme',
                      'mlh0af'
        )
        
        sectorList <- c('HY Master',
                        'BB', 
                        'B', 
                        'CCC',
                        'Global HY USD',
                        'Publishing/ Printing',
                        'Broadcast',
                        'Paper',
                        'Rails',
                        'Restaurants',
                        'Energy',
                        'Chemicals',
                        'Building Materials',
                        'Insurance',
                        'Containers',
                        'Transp.ex-Air/Rail',
                        'Cons. Products',
                        'Homeblds/ RE',
                        'Capital Goods',
                        'Cable/ Satellite',
                        'Services',
                        'Hotels',
                        'Food/Bev/Tobacco',
                        'Autos/Parts',
                        'Air Transport',
                        'Gaming',
                        'Banks&Thrifts',
                        'Utilities',
                        'Diversified Media',
                        'Telecom',
                        'Aerospace',
                        'Tech', 
                        'Environmental',
                        'Fin. Services',
                        'Leisure',
                        'Ent/Film',
                        'Healthcare',
                        'Food/Drug Retail',
                        'Steel',
                        'Metals/Mining',
                        'Super Retail',
                        'Coal',
                        'Floating Rate HY'
                        
        )
        
        
        US_HY$`Index Name` <- lapply(nameList, function(x) {toupper(strsplit(x, 'ml')[[1]][2])});
        
        US_HY$`Sector Rep` <- sectorList
        
        return(US_HY)
        
        }
    )
    
    
    
    
    
    
    
    
## BY SECTOR ENDS--------------------------------------------------------  

    
      } else {
    
    switch (object_,
      'US HY' = {
        
        #************************************************
        # Fields: `Index Name` and `Maturity Type`
        #************************************************
        
        
        
        #  US HY sector frame
        US_HY <- as.data.frame(list());
        
        
        nameList <-           c('mlh0a0',
                                'mlj0a0',
                                'mlj1a0',
                                'mlj2a0',
                                'mlj3a0',
                                'mlj4a0',
                                'mlj7a0',
                                'mlj8a0'
                                
        )
        
        US_HY$`Index Name` <- lapply(nameList, function(x) {toupper(strsplit(x, 'ml')[[1]][2])});
        
        maturityList <-   c(   'US HY Master',
                               'HY cash pay',
                               'HY 1-3 yr*',
                               'HY 3-5 yr*',
                               'HY 5-7 yr*',
                               'HY 7-10 yr*',
                               'HY 10-15 yr*',
                               'HY 15+ yr'
                               
        )      
        
        US_HY$`Maturity Type` <- maturityList
        
      },
      
      'US BB' ={
        
        #************************************************
        # Fields: `Index Name` and `Maturity Type`
        #************************************************
        
        
        
        #  US HY sector frame
        US_BB <- as.data.frame(list());
        
        
        nameList <-           c('mlj0a1',
                                'mlj1a1',
                                'mlj2a1',
                                'mlj3a1',
                                'mlj4a1',
                                'mlj9a1'
                                
        )
        
        US_BB$`Index Name` <- lapply(nameList, function(x) {toupper(strsplit(x, 'ml')[[1]][2])});
        
        maturityList <-   c(   'US HY BB cash',
                               'HY 1-3 yr*',
                               'HY 3-5 yr*',
                               'HY 5-7 yr*',
                               'HY 7-10 yr*',
                               '10+ yr*'
                               
        )      
        US_BB$`Maturity Type` <- maturityList
        
      },
      
      'US B' = {
        
        #************************************************
        # Fields: `Index Name` and `Maturity Type`
        #************************************************
        
        
        
        #  US HY sector frame
        US_B <- as.data.frame(list());
        
        
        nameList <-           c('mlj0a2',
                                'mlj1a2',
                                'mlj2a2',
                                'mlj3a2',
                                'mlj4a2',
                                'mlj9a2'
                                
        )
        
        US_B$`Index Name` <- lapply(nameList, function(x) {toupper(strsplit(x, 'ml')[[1]][2])});
        
        maturityList <-   c(  'US HY B cash', 
                              'US 1-3 B', 
                              'B 3-5', 
                              'B 5-7',
                              'B 7-10',
                              'B 10+'
                              
        )      
        US_B$`Maturity Type` <- maturityList
        
      }
    
        'US CCC' = {
          
          #************************************************
          # Fields: `Index Name` and `Maturity Type`
          #************************************************
          
          
          
          #  US HY sector frame
          US_CCC <- as.data.frame(list());
          
          
          nameList <-           c('mlj0a3',
                                  'mlj1a3',
                                  'mlj2a3',
                                  'mlj3a3',
                                  'mlj4a3',
                                  'mlj9a3'
                                  
          )
          
          US_CCC$`Index Name` <- lapply(nameList, function(x) {toupper(strsplit(x, 'ml')[[1]][2])});
          
          maturityList <-   c(   'US HY CCC cash', 
                                 'US 1-3 CC',
                                 'CCC 3-5',
                                 'CCC 5-7',
                                 'CCC 7-10',
                                 'CCC 10+'
                                
          )      
          US_CCC$`Maturity Type` <- maturityList
          
        },
        

        'Euro' = {
          
          #************************************************
          # Fields: `Index Name` and `Maturity Type`
          #************************************************
          
          
          
          #  US HY sector frame
          Euro <- as.data.frame(list());
          
          
          nameList <-           c('mlhe0b',
                                  'mlhe0d'
                                  
                                  
          )
          
          Euro$`Index Name` <- lapply(nameList, function(x) {toupper(strsplit(x, 'ml')[[1]][2])});
          
          maturityList <-   c( '2-4 year', 
                               '4 - 6 year'
                               
          )      
          Euro$`Maturity Type` <- maturityList
          
        }
        
        
      
      
      
      )
    
    
    
  }
  
  
  
}