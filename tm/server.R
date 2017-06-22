#install.packages("shiny")
#install.packages("plotly")
#install.packages("ggplot2")
#install.packages("DT")

library(shiny)
library(plotly)
library(ggplot2)
library(DT)

tryCatch({
  source("tm/sophia.r")
}, warning = function(e) {
  source("sophia.r")
})

# 
# desc : get import type
# 
getImportType <- function(opt) {
  dataImport <- "text"
  if(as.numeric(opt) == 1) {
    dataImport <- "text"
  } else if (as.numeric(opt) == 2) {
    dataImport <- "url"
  }
  return(dataImport)
}

#
# desc : split the string line
#
splitStrByComma <- function(stringTerms) {
  return(as.vector(strsplit(stringTerms, ',')[[1]]))
}

# shiny server main function
shinyServer(function(input, output, session) {
  
  #################
  # run every session
  #################
  
  # data import type
  output$mostFreqTermsTable <- renderDataTable({
    
    # data import type
    dataImport <- getImportType(input$dataImport)
    
    # data / web content
    dataImportField <- input$dataImportField
    
    # data language
    dataLang <- input$dataLang
    
    # data transformation
    # might be NA
    dataTrans <- splitStrByComma(input$dataTrans)
    
    # data filter
    # might be NA
    dataFilter <- splitStrByComma(input$dataFilter)
    
    # word length in tdm
    wordLength <- input$wordLength
    
    # weighting method
    weightMethod <- c(
      as.numeric(input$weightMethod),
      input$smartTerm,
      input$smartDoc,
      input$smartNormalization
    )
    
    # data analysis
    findFreqTerms <- input$findFreqTerms
    
    # get all terms
    findTermsByFreq(dataImport, dataImportField, dataLang, dataTrans, dataFilter, wordLength, weightMethod, findFreqTerms, "//p")
  })
  
  output$assocTermsTable <- renderDataTable({
    
    # data import type
    dataImport <- getImportType(input$dataImport)
    
    # data / web content
    dataImportField <- input$dataImportField
    
    # data language
    dataLang <- input$dataLang
    
    # data transformation
    # might be NA
    dataTrans <- splitStrByComma(input$dataTrans)
    
    # data filter
    # might be NA
    dataFilter <- splitStrByComma(input$dataFilter)
    
    # word length in tdm
    wordLength <- input$wordLength
    
    # weighting method
    weightMethod <- c(
      as.numeric(input$weightMethod),
      input$smartTerm,
      input$smartDoc,
      input$smartNormalization
    )
    
    # association terms
    termAssoc <- splitStrByComma(input$termAssoc)
    
    # association ratio
    findAssocsRatio <- as.numeric(input$findAssocsRatio)
    
    # get association terms
    findAssocsByTerms(dataImport, dataImportField, dataLang, dataTrans, dataFilter, wordLength, weightMethod, "//p", termAssoc, findAssocsRatio)
    
  })
  
  output$dictListing <- renderDataTable({
    
    # data import type
    dataImport <- getImportType(input$dataImport)
    
    # data / web content
    dataImportField <- input$dataImportField
    
    # data language
    dataLang <- input$dataLang
    
    # data transformation
    # might be NA
    dataTrans <- splitStrByComma(input$dataTrans)
    
    # data filter
    # might be NA
    dataFilter <- splitStrByComma(input$dataFilter)
    
    # word length in tdm
    wordLength <- input$wordLength
    
    # terms for listing dictionaries
    dictList <- splitStrByComma(input$dictList)
    
    # get association terms
    listDictByTerms(dataImport, dataImportField, dataLang, dataTrans, dataFilter, wordLength, "//p", dictList)
    
  })
})








