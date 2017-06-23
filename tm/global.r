#
# desc : anything necessary librarie or package should be loaded on global.r, not in server.r or ui.r
#

#install.packages("shiny")
#install.packages("plotly")
#install.packages("ggplot2")
#install.packages("DT")
#install.packages("RCurl")

library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(RCurl)

tryCatch({
  source("tm/sophia.r")
}, warning = function(e) {
  source("sophia.r")
})


# plain-text example
processFile = function(filepath) {
  allData <- ""
  con = file(filepath, "r")
  
  while ( TRUE ) {
    line = readLines(con, n = 1, encoding="UTF-8")
    if ( length(line) == 0 ) {
      break
    }
    allData <- paste(allData, line, sep="\n")
  }
  
  close(con)
  return(allData)
}

plainDataImportEN <- processFile("data/exampleData/english.txt")
plainDataImportCN <- processFile("data/exampleData/chinese.txt")

webUrlImportEN <- "http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12"
webUrlImportCNOri <- c(
  "https://udn.com/news/story/6897/2528524",
  "http://www.ithome.com.tw/news/114828",
  "http://technews.tw/2017/06/19/ai-doctor/"
)
webUrlImportCN <- webUrlImportCNOri[3]





