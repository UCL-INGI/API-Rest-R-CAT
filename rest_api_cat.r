# Loading package
library(plumber)
library(rjson)
library(ltm)
library(hash)
library(catR)


matrixItemBank <- hash()

get2PLModel <- function(data){
  irt.mtf = ltm(data ~ z1)
  return(coef(irt.mtf))
}

# if two questions have the same difficulty, add 0.000001 to one of them to avoid randomness
removeSameDif <- function(itemBank)
{
  i = 1
  while(i < nrow(itemBank)){
    j = i + 1
    while(j  < nrow(itemBank)){
      if(itemBank[j,2] == itemBank[i,2]){
        itemBank[j,2] = itemBank[j,2] + 0.000001
        i = 1
        j = 1
      }
      j = j + 1
    }
    i = i + 1
  }
  return(itemBank)
}

#* add a new itemBank in matrixItemBank and compute his parameters
#* @param data an itemBank where each value is separate by a semicolon ';'
#* @param index
#* @post /newExam
newExam <- function(data,index)
{
  matrixItemBank[index] <<- list(NULL)
  questionArrayList <- fromJSON(data)
  if(length(questionArrayList) == 0){
      return(-1)
  }
  questionArrayList <- rapply(questionArrayList,function(x) ifelse(x==-1,NA,x), how = "replace")
  questionArrayMatrix <- t(do.call("cbind",questionArrayList))

  model <- get2PLModel(questionArrayMatrix)
  itemBank.vector <- c()
  
  for(row in 1:nrow(model)) {
    itemBank.vector <- c(itemBank.vector,model[row,2],model[row,1],0,1)
  }
  itemBank <- matrix(itemBank.vector,ncol=4,byrow=TRUE)
  itemBank <- removeSameDif(itemBank)
  matrixItemBank[[index]] <<- itemBank
  return(index)
}

#* @param itemBank: an itemBank (matrix m * 4)
#* @param idList: a list of unique ids
#* return: a troncated item bank where each row is the row at itemBank[id,]
itemBankIDsToItemBank <- function(itemBank,idList){
  itemBankResult <- matrix(nrow=length(idList),ncol = ncol(itemBank))
  index <- 1
  for(id in idList){
    itemBankResult[index,] <- itemBank[id,]
    index <- index + 1
  }
  return(itemBankResult)
}

#* return: TRUE if the itemBank is 1PL (no discrimination)
#*         FALSE otherwise
isOnePL <- function(itemBank){
  for(i in 1:nrow(itemBank)){
    if(itemBank[i,1] != 1){
      return(FALSE)
    }
  }
  return(TRUE)
}

#* @param theta: The evaluated ability
#* @param semTheta: Confident Interval
#* @param nextQuestion: the id of a question
#* @param itemBank: an itemBank (matrix m * 4)
#* return TRUE if the nextQuestion is usefull. A usefull question is consider as a question inside the confident interval between the evaluated ability (+/- maxGap)
#*        FALSE otherwide
notUsefull <- function(theta,semTheta,nextQuestion,itemBank){
  if(!isOnePL(itemBank)) return(FALSE)

  maxGap = 0.4
  nextQuestionDif = itemBank[nextQuestion,2]
  borneInf = theta - semTheta*1.96 - maxGap
  borneSup = theta + semTheta*1.96 + maxGap
  if(borneInf <= nextQuestionDif  && nextQuestionDif <= borneSup) return(FALSE) #if nextQuestion difficulty is in the confidence interval +/- maxGap
  else return(TRUE)
}

#* @param semTheta: Confident Interval
#* @param itemBank: an itemBank (matrix m * 4)
#* return TRUE if the test should stop because of the precision
#*        FALSE otherwise
shouldEnd <- function(semTheta,itemBank){
  if(isOnePL(itemBank) & semTheta < 0.45) return(TRUE)
  else if(semTheta < 0.35) return(TRUE)
  return(FALSE)
}

#* get the id of the next question to ask or -1 if should end and the current score of the student.
#* @param itemBankID the id of the itemBank
#* @param alreadyAnswered the list of id of already answered questions (-1 or void if it's the begining of the test)
#* @param responseList the list of correctness of alreadyAnswered questions. The values must be between 0 and 1 (-1 or void if it's the begining of the test)
#* @post /nextQuestion
nextQuestion <- function(itemBankID,alreadyAnswered="-1",responseList="-1"){
  
  itemBank <- matrixItemBank[[itemBankID]]
  
  if(is.null(itemBank)){
    return(list(index = -1,score=-1))
  }
  
  if(length(alreadyAnswered) == 0){
    index <- catR::startItems(itemBank)$items
    return(list(index = index,score=-1))
  }
  
  itemBankTronc <- itemBankIDsToItemBank(itemBank,alreadyAnswered)
  responses.vector <- responseList
  responses <- matrix(responses.vector,ncol=1)
  alreadyAnswered.vector <- alreadyAnswered
  
  theta <- catR::thetaEst(itemBankTronc,responses)
  semTheta <- catR::semTheta(theta,itemBankTronc,responses)
  
  if( length(alreadyAnswered) >= nrow(itemBank)){
    return(list(index=-1,score=pnorm(theta)*100))
  }
  
  if(shouldEnd(semTheta,itemBank)){
    return(list(index=-1,score=pnorm(theta)*100))
  }

  nextQuestion <- catR::nextItem(itemBank,model=NULL,theta,alreadyAnswered.vector,criterion="MFI")
  if(notUsefull(theta,semTheta,nextQuestion$item,itemBank)){
    return(list(index=-1,score=pnorm(theta)*100))
  }

  return(list(index=nextQuestion$item,score=pnorm(theta)*100))
}
