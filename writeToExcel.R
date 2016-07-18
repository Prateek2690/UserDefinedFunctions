# writeToExcel Custom Function
# Inputs: fullyQualifiedWorkingDir,  dataToEmbed, filename, sheetName, headerValue

writeToExcel <- function(fullyQualifiedWorkingDir, toptable, bottomtable, filename, sheetName, headerValue)          
{
  library(r2excel) # custom import
  
  # Setwd
  setwd(fullyQualifiedWorkingDir)
  
  # create workbook and sheet
  wb <- createWorkbook(type="xlsx")
  sheet <- createSheet(wb, sheetName = sheetName)
  
  # add iris data using default settings
  #data(toptable)
  xlsx.addHeader(wb, sheet, 
                 value="Top 20")
  xlsx.addLineBreak(sheet, 1)
  xlsx.addTable(wb, sheet, data= toptable,
                fontColor="darkblue", fontSize=08,
                rowFill=c("white", "lightblue"))
  xlsx.addLineBreak(sheet, 2)
  
  # Customized table
  xlsx.addHeader(wb, sheet, value="Bottom 20")
  xlsx.addLineBreak(sheet, 1)
  xlsx.addTable(wb, sheet, data= bottomtable,
                fontColor="darkblue", fontSize=08,
                rowFill=c("white", "lightblue")
  )
  xlsx.addLineBreak(sheet, 2)
  
  # save the workbook to an Excel file
  saveWorkbook(wb, filename)
  #xlsx.openFile("examples_add_table.xlsx")# view the file
}
