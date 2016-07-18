library(shiny)

#**************************
# SETTING UP USER INTERFACE
#**************************
shinyUI(fluidPage(
  
  
  # titlePanel
  titlePanel(title = h4("Iris Dataset", align = "center")),
  sidebarLayout(
   sidebarPanel(
     fileInput("file", "Upload the file"), #fileInput() is used to get the file upload
      helpText("Default max. file size is 5Mb."),
      tags$hr(),
      h5(helpText("Select the read.table is below")),
      checkboxInput(inputId = 'header', label ='Header', value =FALSE),
      checkboxInput(inputId = 'stringsAsFactors', "stringAsFactors", FALSE),
      br(),
      radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma = ',', Semicolon = ';', Tab = '\t', Space = ''), selected = ',')
    ), # Inputs from the Users
   
   mainPanel(("Output Windows"),
          uiOutput("tb")
   
            
  ) 
  
) # SidebarLayout END

) # fluidPage END

) # ShinyUI END