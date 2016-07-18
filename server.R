library(shiny)

#****************************
# SETTING UP SERVER INTERFACE
#****************************

shinyServer(
  
  function(input, output){
  
    # reactive Function will take the inputs fron UI.R and use them for read.table() to read the data from the file
    # file$datapath -> gives the path of the file
    data <- reactive({
      file1 <- input$file
      if(is.null(file1)){return()}
      read.table(file = file1$datapath, 
                 sep = input$sep,
                 header = input$header, 
                 stringsAsFactors = input$stringsAsFactors)
      
      
    })
    
    
    # This reactive output contains the summary of the dataset
    
    
    
    # This reactive ouput contains the dataset and display the dataset in table format
    output$table <- renderTable({
      
      if(is.null(data())){return()}
      data()      
      
      
    })
    
    # The following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded
    output$tb <- renderUI({

            tabsetPanel((tabPanel("About file", tableOutput(('fileff')), tabPanel("Data", tableOutput("table")), tabPanel("Summary"))))
      
    })
    
    
    
      
    # Output Goes Here
    output$myname <- renderText(input$name)
    output$myage <- renderText(input$age)
    output$mygender <- renderText(input$gender)
    
    # input goes here from UI
    # input$name
    # input$age
    
    
  }
  
  
)