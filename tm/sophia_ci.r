#
# auth : JianKai Wang (http://jiankaiwang.no-ip.biz/)
# desc : ci for sophia.tm
# call : checkAllFuncs()
#

setwd("D:/code/shiny/github/sophia.dm/tm")

# main test
source("sophia.r")

# parameters
threshold <- 0.9
totalFuncTestPassRatio <- 0.9
currentCIName <- "sophia.tm"

# clear all environment settings
ClearEnv <- function() {
  rm(list=ls())
}

#
# desc : prepare check point matrix
#
prepareCPMatrix <- function(getCP) {
  checkPoint <- matrix(
    getCP, 
    nrow = nrow(getCP),
    ncol = ncol(getCP),
    dimnames = list(
      1:length(getCP[,1]),
      c("CheckPoint", "Test Name", "Result")
    )
  )

  return(checkPoint)
}

#
# desc : test function
#
serviceAvailable <- function(getFuncName, getMatrix) {
  allCheckStatus <- getMatrix[,"Result"]
  passIndex <- which(allCheckStatus == "TRUE")
  if((length(passIndex) / length(allCheckStatus)) >= threshold) {
    return(c(getFuncName,TRUE))
  } else {
    return(c(getFuncName,FALSE))
  }
}

#
# desc : main function 1
#
checkFindTermsByFreq <- function() {
  ClearEnv()

  freqTerms <- findTermsByFreq(
    "url",
    "http://www.ithome.com.tw/news/114828",
    "zho",
    c(),
    c(),
    c(2,5),
    c(0,"n","n","n"),
    1,
    "//p"
  )

  # check point 1 : total terms
  if(length(freqTerms[,1]) < 100) {
    checkPoint <- c(1,"Total Terms",FALSE)
  } else {
    checkPoint <- c(1,"Total Terms",TRUE)
  }

  # check point 2 : specific terms
  if(which(freqTerms == "´I°ê»È¦æ") > -1) {
    checkPoint <- rbind(checkPoint, c(2,"Specific Terms",TRUE))
  } else {
    checkPoint <- rbind(checkPoint, c(2,"Specific Terms",FALSE))
  }  

  checkPoint <- prepareCPMatrix(checkPoint)

  return(serviceAvailable("checkFindTermsByFreq(func)", checkPoint))
}

#
# desc : main function 2
#
checkGetAllAssocTerms <- function() {
  ClearEnv()

  findAssocTerms <- findAssocsByTerms(
    "url",
    "http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12",
    "eng",
    c(),
    c(),
    c(2,5), 
    c(1,"n","n","n"),
    "//p",
    c("trump"), 
    0.1
  )

  # check point 1 : total terms
  if(length(findAssocTerms[,1]) < 100) {
    checkPoint <- c(1,"Total Association Terms",FALSE)
  } else {
    checkPoint <- c(1,"Total Association Terms",TRUE)
  }

  # check point 2 : specific terms
  if(which(findAssocTerms[,2] == "overcome") > -1) {
    checkPoint <- rbind(checkPoint, c(2,"Specific Association Terms",TRUE))
  } else {
    checkPoint <- rbind(checkPoint, c(2,"Specific Association Terms",FALSE))
  }  

  checkPoint <- prepareCPMatrix(checkPoint)

  return(serviceAvailable("checkGetAllAssocTerms(func)", checkPoint))
}

#
# desc : main function 3
#
checkListDictByTerms <- function() {
  ClearEnv()

  listDictTerms <- listDictByTerms(
    "url",
    "http://www.bbc.co.uk/news/resources/idt-d60acebe-2076-4bab-90b4-0e9a5f62ab12",
    "eng",
    c(),
    c(),
    c(2,5), 
    "//p", 
    c("trump")
  )

  # check point 1 : total docs
  if(length(listDictTerms[,1]) < 50) {
    checkPoint <- c(1,"Total Documents",FALSE)
  } else {
    checkPoint <- c(1,"Total Documents",TRUE)
  }

  # check point 2 : specific docs
  if(length(which(listDictTerms[,1] == 1)) > 5) {
    checkPoint <- rbind(checkPoint, c(2,"Specific Documents",TRUE))
  } else {
    checkPoint <- rbind(checkPoint, c(2,"Specific Documents",FALSE))
  }  

  checkPoint <- prepareCPMatrix(checkPoint)

  return(serviceAvailable("checkListDictByTerms(func)", checkPoint))
}


#
# desc : main call
#
checkAllFuncs <- function() {
  funcBuildResMatrix <- checkFindTermsByFreq()
  funcBuildResMatrix <- rbind(funcBuildResMatrix, checkGetAllAssocTerms())
  funcBuildResMatrix <- rbind(funcBuildResMatrix, checkListDictByTerms())

  print(
    matrix(
      funcBuildResMatrix,
      nrow = nrow(funcBuildResMatrix),
      ncol = ncol(funcBuildResMatrix),
      dimnames = list(
        1:nrow(funcBuildResMatrix),
        c("Test Item","Result")
      )
    )
  )
  
  allCheckStatus <- funcBuildResMatrix [,2]
  passIndex <- which(allCheckStatus == "TRUE")
  if((length(passIndex) / length(allCheckStatus)) >= totalFuncTestPassRatio) {
    return(c(currentCIName,TRUE))
  } else {
    return(c(currentCIName,FALSE))
  }
}


