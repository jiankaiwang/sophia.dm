#
# auth : JianKai Wang (https://welcome-jiankaiwang.rhcloud.com)
# proj : text mining (tm) | sophia.dm (https://github.com/jiankaiwang/sophia.dm)
#

# 
# desc : dependencies
#
#install.packages("tm")
#install.packages("RCurl")
#install.packages("rvest")
#install.packages("jiebaR")

library("tm")
library("RCurl")
library("rvest")
library("jiebaR")

#
# desc : read file in UTF-8
#
readFilebyLine <- function(filepath, sepbyLine) {
  allData <- ""
  con = file(filepath, "r")
  
  while ( TRUE ) {
    line = readLines(con, n = 1, encoding="UTF-8")
    if ( length(line) == 0 ) {
      break
    }
    allData <- paste(allData, line, sep=sepbyLine)
  }
  
  close(con)
  return(allData)
}

#
# desc : stopword libraries
#
stopWordLibs <- function(getLang) {
  getWords <- c()
  switch(
    getLang,
    "zho" = { 
      getWords <- c(
        strsplit(readFilebyLine("data/stopwords/chinese.txt",""), ',')[[1]]
        ,strsplit(readFilebyLine(jiebaR::STOPPATH,","), ',')[[1]]
      )
      getWords <- unique(getWords)
    },
    "eng" = {
      getWords <- c(stopwords(getLang), stopwords("SMART"))
    },
    "deu" = {
      getWords <- c(stopwords(getLang), stopwords("SMART"))
    },
    "fra" = {
      getWords <- c(stopwords(getLang), stopwords("SMART"))
    },
    "ita" = {
      getWords <- c(stopwords(getLang), stopwords("SMART"))
    }
  )
  return(getWords);
}

#
# desc : fetch web content
#
fetchWebContent <- function(getUrl) {
  # method.1
  #doc.html <- htmlParse(getURL(getUrl, ssl.verifypeer=FALSE), encoding="utf-8")
  
  # method.2
  doc.html <- read_html(getUrl, encoding = "utf8")
  return(doc.html)
}

#
# desc : extract html text
# 
extractHtmlContent <- function(getData) {
  return(html_text(getData))
}

#
# desc : paragraph the content
#
paragraphContent <- function(getType, getData, paragraphCharacter) {

  doc.text <- c()
  
  switch(
    getType,
    "text" = {
      doc.text <- unlist(strsplit(getData, paragraphCharacter))
    },
    "url" = {
      # method.1 : might encounter ssl problems
      #doc.text = unlist(xpathApply(getData, paragraphCharacter, xmlValue, "utf-8"))   
      
      # method.2
      doc.text <- unlist(
        lapply(
          html_nodes(getData, xpath = paragraphCharacter), 
          extractHtmlContent
        )
      )
    }
  )
  
  allRemoveList <- c()
  for(i in 1:length(doc.text)) {
    if(nchar(doc.text[i]) < 1) {
      allRemoveList <- c(allRemoveList, i)
    }
  }
  # only execute when the paragraph with empty content exists
  if(length(allRemoveList) > 0) {
    doc.text <- doc.text[- allRemoveList]
  }
  
  return(doc.text)
}

#
# desc : parse data content
#
parseDataContent <- function(dataImport, dataImportField, extractTag) {
  # get the raw data
  switch(
    dataImport,
    "text" = {
      rawData <- dataImportField
      preData <- paragraphContent("text", rawData, "\n")
    },
    "url" = {
      rawData <- fetchWebContent(dataImportField)
      preData <- paragraphContent("url", rawData, extractTag)
      preData <- gsub('[\r\n\t]', '', preData)
    }
  )
  
  return(preData)
}

#
# desc : detail with the content
# getData : a unlist-based object
#
detailWithContent <- function(getData, removeCharacterList) {
  doc.text <- getData
  for(i in 1:length(removeCharacterList)) {
    doc.text <- gsub(removeCharacterList[i], '', doc.text)
  }
  return(doc.text)
}

#
# desc : transform the corpus
#
transformCorpus <- function(getCorpus, getOperation) {
  d.corpus <- getCorpus
  if(getOperation == "removeNumbers") {
    d.corpus <- tm_map(d.corpus, content_transformer(removeNumbers))
    d.corpus <- tm_map(d.corpus, content_transformer(function(word) {
      gsub("[0-9]", "", word)
    }))
  } else if (getOperation == "removePunctuation") {
    d.corpus <- tm_map(d.corpus, content_transformer(removePunctuation))
  } else if (getOperation == "stripWhitespace") {
    d.corpus <- tm_map(d.corpus, content_transformer(stripWhitespace))
  } else if (getOperation == "PlainTextDocument") {
    d.corpus <- tm_map(d.corpus, content_transformer(PlainTextDocument))
  }
  return(d.corpus)
}

#
# desc : generare the corpus and transform the corpus
#
generateCorpus <- function(preData, dataLang, dataTrans) {
  # corpus preparation
  d.corpus <- VCorpus(
    VectorSource(preData), 
    list(
      reader = readPlain,
      language = dataLang
    )
  )
  
  # data transformation
  if(length(dataTrans) > 0) {
    for(i in 1:length(dataTrans)) {
      switch(
        as.numeric(dataTrans[i]),
        { d.corpus <- transformCorpus(d.corpus, "removePunctuation") },
        { d.corpus <- transformCorpus(d.corpus, "removeNumbers") },
        { d.corpus <- transformCorpus(d.corpus, "stripWhitespace") }
      )
    }
  }
  d.corpus <- transformCorpus(d.corpus, "PlainTextDocument")
  
  return(d.corpus)
}

#
# desc : chinese cutter
#
cutChineseStr <- function(getCorpus) {
  
  # jiebaR package
  cutter <- jiebaR::worker(bylines = TRUE, symbol = FALSE)
  cutfunc <- function(wordInStr){
    allSeg <- segment(wordInStr, cutter)[[1]]
    allSeg <- allSeg[which(nchar(allSeg) > 1)]
    return(as.character(paste(allSeg, collapse = " ")))
  }
  d.corpus <- tm_map(getCorpus, content_transformer(cutfunc))
  
  # total stop words
  rmWords <- stopWordLibs("zho")
  removeStopWordList <- c()
  knownAvoidList <- c("(",")","?","+","\\","[","]","*","*LRB*","*RRB*","!")
  rmWords <- rmWords[-which(rmWords %in% knownAvoidList)]
  
  # try to filter words not good for gsub
  for(i in 1:length(rmWords)) { 
    if(rmWords[i] %in% knownAvoidList) {
      removeStopWordList <- c(removeStopWordList, i)
      next
    }
    tryCatch({
      tm_map(d.corpus,content_transformer(removeWords),rmWords[i])
    }, warning = function(w) {
      removeStopWordList <- c(removeStopWordList, i)
    }, error = function(e) {
      removeStopWordList <- c(removeStopWordList, i)
    })
  }
  
  if(length(removeStopWordList) > 0) {
    rmWords <- rmWords[-removeStopWordList]
  }
  
  # filter all stop words
  d.corpus <- tm_map(d.corpus,content_transformer(removeWords),rmWords)
  
  # make sure chinese string is in cutting mode
  d.corpus <- tm_map(d.corpus, content_transformer(cutfunc))
  
  return(d.corpus)
}

#
# desc : filter the corpus by filters
#
addFilterCorpus <- function(dataLang, dataFilter) {
  myStopWords <- stopWordLibs(dataLang)
  
  if(length(dataFilter) > 0) {
    # additional filter defined by user
    for(i in 1:length(dataFilter)) {
      myStopWords <- c(myStopWords, dataFilter[i])
    }
  }
  
  return(myStopWords)
}

#
# desc : filter the term by filters
#
filterTerms <- function(getAllTerms, dataFilter) {
  allTerms <- getAllTerms
  
  if(length(dataFilter) > 0 && length(allTerms)) {
    # additional filter defined by user
    removeIndex <- c()
    for(i in 1:length(allTerms)) {
      if(allTerms[i] %in% dataFilter) {
        removeIndex <- c(removeIndex, i)
      }
    }
    
    # remove the index
    if(length(removeIndex) > 0) {
      allTerms <- allTerms[-removeIndex]
    }
  }
  
  return(allTerms)
}

#
# desc : weighting function
#
weightingFunc <- function(weightMethod) {
  switch(
    as.character(weightMethod[1]),
    "1" = { return(FALSE) },
    "2" = { return(function(x) { weightTf(x) }) },
    "3" = { return(function(x) { weightTfIdf(x, normalize = TRUE) }) },
    "4" = { return(function(x) { weightBin(x) }) },
    "5" = { return(function(x) { weightSMART(x, spec = paste(weightMethod[-1], collapse = "")) }) }
  )
}

#
# desc : return results of function findTermsByFreq 
#
retFreqTerms <- function(allTerms) {
  if(length(allTerms) < 1) {
    return(matrix(
      c("No term."),
      nrow = 1,
      ncol = 1,
      dimnames = list(c(1), c("Desc"))
    ))
  }
  else {
    return(
      matrix(
        allTerms,
        nrow = length(allTerms),
        ncol = 1,
        dimnames = list(1:length(allTerms), c("Term"))
      )
    )
  }
}

# 
# ---------------------------------------------------------------------------------------
# desc : find terms by frequencies
# call : by function
# inpt :
# |- dataImport : {x|text,url}
# |- dataImportField : {y|text in string, website url}
# |- dataLang : {x|zho,eng, ...}
# |- dataTrans : c(1,2,3)
# |- dataFilter : c("eg1","eg2","eg3")
# |- wordLength : c(2,5)
# |- weightMethod : e.g. c(1,"n","n","n"), the last three options are only available for SMART method
# |- findFreqTermsNum : {x|N, and > 0}, e.g. 1,2,3,4 ...
# |- extractTag : {x|//p, //a}, only availabe when dataImport is url 
# e.g. :
# |- findTermsByFreq("url","http://www.ithome.com.tw/news/114828","zho",c(),c(),c(2,5),c(1,"n","n","n"),1,"//p")
#
findTermsByFreq <- 
  function(dataImport, dataImportField, dataLang, dataTrans, dataFilter, wordLength, weightMethod, findFreqTermsNum, extractTag) 
{
  
  # get the raw data
  preData <- parseDataContent(dataImport, dataImportField, extractTag)
  
  # detailed with content
  preData <- detailWithContent(preData, c('[\r\n\t]'))
  
  # term-document matrix
  tdm <- c()
  
  if(dataLang == "zho") {
    # Chinese
    
    # filter english and number
    preData <- detailWithContent(preData, c('[A-Za-z]'))
    preData <- preData[nchar(preData) > 0]

    # corpus preparation and data transformation
    d.corpus <- generateCorpus(preData, dataLang, dataTrans)
    
    # chinese string cutter
    d.corpus <- cutChineseStr(d.corpus)
    
    # data filter
    d.corpus <- tm_map(d.corpus, content_transformer(removeWords), dataFilter)
    
    # bug : must add while parsing chinese, or '\n' might exist
    Sys.setlocale(locale="English")
    
    tdm <- TermDocumentMatrix(
      d.corpus, 
      control = list(
        wordLengths = c(wordLength[1], Inf),
        removePunctuation = TRUE,
        stopwords = FALSE,
        weighting = weightingFunc(weightMethod)
      )
    )
    
    Sys.setlocale(locale="cht")
    
  } else {
    # English, German, French, Italian
    
    # corpus preparation and data transformation
    d.corpus <- generateCorpus(preData, dataLang, dataTrans)
    
    # data filter
    myStopWords <- addFilterCorpus(dataLang, dataFilter)
    d.corpus <- tm_map(d.corpus, content_transformer(removeWords), myStopWords)
  
    # tdm generation
    tdm <- TermDocumentMatrix(
      d.corpus, 
      control = list(
        wordLengths = c(wordLength[1], Inf),
        removePunctuation = TRUE,
        stopwords = TRUE,
        #weighting = function(x)weightTfIdf(x,normalize = TRUE)
        weighting = weightingFunc(weightMethod)
      )
    )
  }
  
  # Find terms by frequencies
  allTerms <- findFreqTerms(tdm, findFreqTermsNum)
  return(retFreqTerms(allTerms))
}
# ---------------------------------------------------------------------------------------
#

#
# desc : get association terms
#
getAllAssocTerms <- function(tdm, getTerm, assocRatio) {
  assoc <- findAssocs(tdm, getTerm, 0)
  rawAssocRes <- as.matrix(assoc[[getTerm]])
  
  totalAssocTerms <- which(rawAssocRes > assocRatio)
  countTtlAssocTerms <- length(totalAssocTerms)

  if(countTtlAssocTerms > 0) {
    assocRes <- matrix(
      c(
        rep(getTerm, countTtlAssocTerms),
        rownames(rawAssocRes)[totalAssocTerms], 
        rawAssocRes[totalAssocTerms]
      ),
      nrow = countTtlAssocTerms, ncol = 3, 
      dimnames = list(1:countTtlAssocTerms, c("Terms","Association Terms", "Association Ratio"))
    )
  } else {
    assocRes <- matrix(
      c(getTerm, "No association term.", 0),
      nrow = 1,
      ncol = 3,
      dimnames = list(c(1), c("Terms","Association Terms", "Association Ratio"))
    )
  }
  
  return(assocRes)
}

#
# desc : return results of function findAssocsByTerms 
#
retAssocTerms <- function(allResult) {
  if(length(allResult) > 0) {
    return(allResult)
  }
  else {
    return(
      matrix(
        c("NA", "No association term.", 0),
        nrow = 1,
        ncol = 3,
        dimnames = list(c(1), c("Terms","Association Terms", "Association Ratio"))
      )
    )
  }
}

# 
# ---------------------------------------------------------------------------------------
# desc : find terms by association
# call : by function
# inpt :
# |- dataImport : {x|text,url}
# |- dataImportField : {y|text in string, website url}
# |- dataLang : {x|zho,eng, ...}
# |- dataTrans : c(1,2,3)
# |- dataFilter : c("eg1","eg2","eg3")
# |- wordLength : c(2,5)
# |- weightMethod : e.g. c(1,"n","n","n"), the last three options are only available for SMART method
# |- extractTag : {x|//p, //a}, only availabe when dataImport is url 
# |- termAssoc : c("term1","term2")
# |- findAssocsRatio : {x|N, and 0 <= N and 1 >= N}
# e.g. :
# |- findAssocsByTerms("url","http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12","eng",c(),c(),c(2,5),c(1,"n","n","n"),"//p",c("trump"), 0.1)
#
findAssocsByTerms <- 
  function(dataImport, dataImportField, dataLang, dataTrans, dataFilter, wordLength, weightMethod, extractTag, termAssoc, findAssocsRatio) 
{
    
  if(length(termAssoc) < 1) {
    
    return(retAssocTerms(c()))
    
  } else {
    
    # get the raw data
    preData <- parseDataContent(dataImport, dataImportField, extractTag)
    
    # detailed with content
    preData <- detailWithContent(preData, c('[\r\n\t]'))
    
    if(dataLang == "zho") {
      # Chinese
      
      # filter english and number
      preData <- detailWithContent(preData, c('[A-Za-z]'))
      preData <- preData[nchar(preData) > 0]
      
      # corpus preparation and data transformation
      d.corpus <- generateCorpus(preData, dataLang, dataTrans)
      
      # chinese string cutter
      d.corpus <- cutChineseStr(d.corpus)
      
      # data filter
      d.corpus <- tm_map(d.corpus, content_transformer(removeWords), dataFilter)
      
      # bug : must add while parsing chinese, or '\n' might exist
      Sys.setlocale(locale="English")
      
      tdm <- TermDocumentMatrix(
        d.corpus, 
        control = list(
          wordLengths = c(wordLength[1], Inf),
          removePunctuation = TRUE,
          stopwords = FALSE,
          weighting = weightingFunc(weightMethod)
        )
      )
      
      Sys.setlocale(locale="cht")
      
      } else {
        # English, German, French, Italian
      
        # corpus preparation and data transformation
        d.corpus <- generateCorpus(preData, dataLang, dataTrans)
        
        # data filter
        myStopWords <- addFilterCorpus(dataLang, dataFilter)
        d.corpus <- tm_map(d.corpus, content_transformer(removeWords), myStopWords)
        
        # tdm generation
        tdm <- TermDocumentMatrix(
          d.corpus, 
          control = list(
            wordLengths = c(wordLength[1], Inf),
            removePunctuation = TRUE,
            stopwords = myStopWords,
            weighting = weightingFunc(weightMethod)
          )
        )
      }
    
    # get all association terms
    allAssoc <- c()
    for(i in 1:length(termAssoc)) {
      # Find terms by association
      allAssocRes <- getAllAssocTerms(tdm, termAssoc[i], as.numeric(findAssocsRatio))
      if(i == 1) {
        allAssoc <- allAssocRes
      } else {
        allAssoc <- rbind(allAssoc, allAssocRes)
      }
    }
    return(retAssocTerms(allAssoc))
    
  }

}
# ---------------------------------------------------------------------------------------
#

#
# desc : trim raw doc content
#
trimText <- function(getMsg) {
  limitLen <- 20
  if(!is.na(getMsg) && nchar(getMsg) > limitLen) {
    allWord <- strsplit(getMsg,"")[[1]]
    return(
      paste(paste(c(allWord[1:limitLen]), collapse = ""),"...", sep = "")
    )
  } else {
    return(getMsg)
  }
}

#
# desc : prepare the text content for result
#
prepareDocContent <- function(getAllRes, rawDoc) {
  allResult <- getAllRes
  allRawDoc <- rawDoc[as.numeric(rownames(allResult))]
  
  simpleData <- sapply(allRawDoc, trimText)
  rownames(allResult) <- simpleData
  
  return(allResult)
}

#
# desc : return results of function listDictByTerms 
#
listDictResult <- function(allResult) {
  if(length(allResult) > 0) {
    return(allResult)
  }
  else {
    return(
      matrix(
        c("NA", "No related Docs."),
        nrow = 1,
        ncol = 2,
        dimnames = list(c(1), c("Terms","Documents"))
      )
    )
  }
}

#
# ---------------------------------------------------------------------------------------
# desc : list dictionary of doc by terms
# call : by function
# inpt :
# |- dataImport : {x|text,url}
# |- dataImportField : {y|text in string, website url}
# |- dataLang : {x|zho,eng, ...}
# |- dataTrans : c(1,2,3)
# |- dataFilter : c("eg1","eg2","eg3")
# |- wordLength : c(2,5)
# |- extractTag : {x|//p, //a}, only availabe when dataImport is url 
# |- dictList : c("term1","term2")
# e.g. :
# |- listDictByTerms("url","http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12","eng",c(),c(),c(2,5), "//p", c("trump"))
#
listDictByTerms <- 
  function(dataImport, dataImportField, dataLang, dataTrans, dataFilter, wordLength, extractTag, dictList)
{
  if(length(dictList) < 1) {
    
    return(listDictResult(c()))
    
  } else {
    
    # get the raw data
    preData <- parseDataContent(dataImport, dataImportField, extractTag)
    
    # detailed with content
    preData <- detailWithContent(preData, c('[\r\n\t]'))
    
    d.corpus <- c()
    
    if(dataLang == "zho") {
      # Chinese
      
      # filter english and number
      preData <- detailWithContent(preData, c('[A-Za-z]'))
      preData <- preData[nchar(preData) > 0]
      
      # corpus preparation and data transformation
      d.corpus <- generateCorpus(preData, dataLang, dataTrans)
      
      # chinese string cutter
      d.corpus <- cutChineseStr(d.corpus)
      
      # data filter
      d.corpus <- tm_map(d.corpus, content_transformer(removeWords), dataFilter)
      
    } else {
      # English, German, French, Italian
      
      # corpus preparation and data transformation
      d.corpus <- generateCorpus(preData, dataLang, dataTrans)
      
      # data filter
      myStopWords <- addFilterCorpus(dataLang, dataFilter)
      d.corpus <- tm_map(d.corpus, content_transformer(removeWords), myStopWords)
      
    }
    
    dictListObj <- as.matrix(
      DocumentTermMatrix(
        d.corpus, 
        list(dictionary = dictList)
      )
    )
    
    dictListObj <- prepareDocContent(dictListObj, preData)
    
    # get all dictionary listing
    return(listDictResult(dictListObj))
    
  }
}
#
# ---------------------------------------------------------------------------------------
#

#
# desc : parse input parameter from terminal
#
parseInputParas <- function(options) {
  if(length(options) == 1 && options[1] == "NA") {
    return(c())
  } else {
    return(as.vector(strsplit(options, ',')[[1]]))
  }
}

#
# desc : prepare findfreqterm function
#
prepareFindfreqtermFunc <- function(args) {
  checkErrorFlag <- 1
  
  tryCatch({
    # service type
    serviceType <- args[1]
    
    # parse the input
    dataImport <- args[2]
    dataImportField <- args[3]
    dataLang <- args[4]
    dataTrans <- parseInputParas(args[5])
    dataFilter <- parseInputParas(args[6])
    wordLength <- parseInputParas(args[7])
    weightMethod <- parseInputParas(args[8])
    findFreqTermsNum <- as.numeric(args[9])
    extractTag <- args[10]
    
    checkErrorFlag <- 0
  }, error = function(e) {
    return("One of input parameter types is error.")
  })
  
  if(checkErrorFlag == 0) {
    return(
      findTermsByFreq(
        dataImport, 
        dataImportField, 
        dataLang, 
        dataTrans, 
        dataFilter, 
        wordLength, 
        weightMethod, 
        findFreqTermsNum, 
        extractTag
      )
    )
  }
}

#
# desc : prepare findAssocsByTerms function
# 
prepareFindAssocsByTermsFunc <- function(args) {
  checkErrorFlag <- 1
  
  tryCatch({
    # service type
    serviceType <- args[1]
    
    # parse the input
    dataImport <- args[2]
    dataImportField <- args[3]
    dataLang <- args[4]
    dataTrans <- parseInputParas(args[5])
    dataFilter <- parseInputParas(args[6])
    wordLength <- parseInputParas(args[7])
    weightMethod <- parseInputParas(args[8])
    extractTag <- args[9]
    termAssoc <- parseInputParas(args[10])
    findAssocsRatio <- as.numeric(args[11])
    
    checkErrorFlag <- 0
  }, error = function(e) {
    return("One of input parameter types is error.")
  })
  
  if(checkErrorFlag == 0) {
    return(
      findAssocsByTerms(
        dataImport, 
        dataImportField, 
        dataLang, 
        dataTrans, 
        dataFilter, 
        wordLength, 
        weightMethod,
        extractTag, 
        termAssoc,
        findAssocsRatio
      )
    )
  }
}

#
# desc : prepare ListDictByTerms Function
#
prepareListDictByTermsFunc <- function(args) {
  checkErrorFlag <- 1
  
  tryCatch({
    # service type
    serviceType <- args[1]
    
    # parse the input
    dataImport <- args[2]
    dataImportField <- args[3]
    dataLang <- args[4]
    dataTrans <- parseInputParas(args[5])
    dataFilter <- parseInputParas(args[6])
    wordLength <- parseInputParas(args[7])
    extractTag <- args[8]
    dictList <- args[9]
    
    checkErrorFlag <- 0
  }, error = function(e) {
    return("One of input parameter types is error.")
  })
  
  if(checkErrorFlag == 0) {
    return(
      listDictByTerms(
        dataImport, 
        dataImportField, 
        dataLang, 
        dataTrans, 
        dataFilter, 
        wordLength, 
        extractTag, 
        dictList
      )
    )
  }  
}

#
# ---------------------------------------------------------------------------------------
# desc : invoke the tm by command
# call : by batch/bash
#
args <- commandArgs(trailingOnly = TRUE)

if(length(args) > 0) {
  
  serviceType <- args[1]
  
  if(length(args) == 9) {
    switch(
      serviceType,
      "listDictByTerms" = {
        print(prepareListDictByTermsFunc(args))
      },
      { print("No such service.") }
    )
  } else if(length(args) == 10) {
    switch(
      serviceType,
      "findfreqterm" = {
        print(prepareFindfreqtermFunc(args))
      },
      { print("No such service.") }
    )
    
  } else if (length(args) == 11) {
    switch(serviceType,
      "findAssocsByTerms" = {
        print(prepareFindAssocsByTermsFunc(args))
      },
      { print("No such service.") }
    )
  } else {
    print("Error on input parameters.")
  }
} else {
  showMsg <- c(
    "Usage :"
    ,"E.g.1 : Rscript sophia.r findfreqterm      dataImport dataImportField dataLang dataTrans dataFilter wordLength weightMethod findFreqTerms extractTag"
    ,"E.g.2 : Rscript sophia.r findAssocsByTerms dataImport dataImportField dataLang dataTrans dataFilter wordLength weightMethod extractTag    termAssoc  findAssocsRatio"
    ,"E.g.3 : Rscript sophia.r listDictByTerms   dataImport dataImportField dataLang dataTrans dataFilter wordLength extractTag   dictList"
    ,""
    ,"Input :"
    ,"|- dataImport : {x|text,url}"
    ,"|- dataImportField : {y|text in string, website url}"
    ,"|- dataLang : {x|zho,eng, ...}"
    ,"|- dataTrans : c(1,2,3)"
    ,"|- dataFilter : c('eg1','eg2','eg3)"
    ,"|- wordLength : c(2,5))"
    ,"|- weightMethod : e.g. c(1,'n','n','n'), the last three options are only available for SMART method"
    ,"|- findFreqTermsNum : {x|N, and > 0}, e.g. 1,2,3,4 ..."
    ,"|- extractTag : {x|//a,//p, ...}"
    ,"|- termAssoc : c('term1', 'term2')"
    ,"|- findAssocsRatio : {x|N, and 0 <= N and 1 >= N}"
    ,"|- dictList : c('term1','term2')"
    ,""
    ,"Examp :"
    ,'Rscript sophia.r findfreqterm url http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12 eng 1 NA "2,10" "1,n,n,n" 1 //p'
    ,'Rscript sophia.r findAssocsByTerms url http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12 eng 1 NA "2,10" "1,n,n,n" //p "trump" 0.1'
    ,'Rscript sophia.r listDictByTerms url http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12 eng 1 NA "2,10" //p "trump"'
  )
  for(i in 1:length(showMsg)) {
    print(showMsg[i])
  }
}
# ---------------------------------------------------------------------------------------
#











