#This file is part of Survey Prospector

#Survey Prospector is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.

#Survey Prospector is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with Survey Prospector.  If not, see <http://www.gnu.org/licenses/>.


# utility function to turn missing variables into zeros for intialization
denull <- function(x,y=0){
  if (is.null(x)) return(y)
  return(x)
}

hash <- function( keys ) {
  result <- new.env( hash = TRUE, parent = emptyenv(), size = length( keys ) )
  for( key in keys ) {
    result[[ key ]] <- NA
  }
  return( result )
}

fillhash <- function(keys,vals,hash=NULL){
  if (is.null(hash)) {
      result <- new.env( hash = TRUE, parent = emptyenv(), size = length( keys ) )
  } else {
      result <- hash
  }
  for(i in 1:length(keys))
  {
    result[[ keys[[i]] ]] <- vals[[i]]
  }
  return( result )
}


computeAUC <- function(tbl,o){
  #calculate the AUC for a contingiency table, given the permutation order
  
  if (sum(tbl[,2])==0 || sum(tbl[,1]) ==0 ){
    return(NULL) # drop this from the list of predictors
  }
  
  yval <- cumsum(tbl[,2][o])/sum(tbl[,2])
  xval <- cumsum(tbl[,1][o])/sum(tbl[,1])
  
  mysum <- 0
  oldx <- 0
  oldy <-0
  for (j in 1:length(xval)) {
    x <- unname(xval[j])
    y <- unname(yval[j])
    mysum <- mysum + x*oldy - y*oldx
    oldx <- x
    oldy <- y
  }
  t <- (mysum + oldy)/2

  return(t) # t may be less than .5 if the order is bad
}


# encode strings and factors into the most frequent and assign numerical values.
code_nominals <-function(df,var, n){
  # df is a dataframe
  # var is a string that indexes the dataframe via df[[var]]
  # n is the number of categories to produce
  # na is true or false, depending on whether or not NAs are included

  # create a frequency table sorted biggest to smallest
  tempTable <- sort(table(df[[var]]),decreasing = TRUE)
  
  # grab the top n names
  topNames <- rownames(head(tempTable,n))
  
  #remove NA if it's in the list
  topNames <- topNames[!topNames == ""]  
  
  # populate a temp file with the code for 'other'
  t <- rep(length(topNames)+1,nrow(df))
    
  # build t by iterating through each name
  for (j in c(1:length(topNames))){
    t[df[[var]] == topNames[j]] <- j
  }
  
  # put the NAs back
  t[is.na(df[[var]])] <- NA
  
  # add the 'other' category
  topNames <- c(topNames,"Other")
  
  return(list(data=t,names=topNames))
}

ntile <- function(df,var,n){
  # create a function for finding the percentile of a value within the column
  Fn <- ecdf(df[[var]])
  
  # find distribution values
  dist <- sapply(df[[var]],Fn)
                 
  # these may be very skewed, so divide min, max into n segments
  mindist <- min(dist,na.rm=TRUE)
  lendist <- max(dist,na.rm=TRUE) - mindist
  
  # apply it to the column and quantize to 1:n
  vals <- floor((dist - mindist)/lendist*.999*n+1)
    
  return(vals)
}

ROCvector <- function(y,p,intervals){
  # y is a column of {0,1} values to be predicted
  # p is the column of fitted values, interpreted as probabilities
  # intervals is the number of intervals, so there will be intervals+1 points in the orbit
  
  TPR = c()
  FPR = c()
  
  saveTP <- c()
  saveFP <- c()
  saveTN <- c()
  saveFN <- c()
  
  n <-length(y)
  totalP <- sum(y)
  totalN <- n- totalP
  AUC <- 0
  oldTPR <- 1 # for calculating AUC
  oldFPR <- 1
      
  q <- seq(0,1,1/intervals)
  for (t in q){
    TP <- sum(subset(y,p>t)) # for some reason, it's predicting the out-class
    FP <- sum(subset(1-y,p>t))
    FN <- sum(subset(y,p<=t))
    TN <- sum(subset(1-y,p<=t))

    #ROC and AUC calculations
    newTPR <- TP/totalP
    newFPR <- FP/totalN
    TPR <- c(TPR,newTPR)
    FPR <- c(FPR,newFPR)
    AUC <- AUC + (oldFPR*newTPR-oldTPR*newFPR)
    oldTPR <- newTPR
    oldFPR <- newFPR
    
    saveTP <- c(saveTP,TP)
    saveFP <- c(saveFP,FP)
    saveTN <- c(saveTN,TN)
    saveFN <- c(saveFN,FN)

  }
  newTPR <- 0
  newFPR <- 0
  AUC <- AUC + (oldFPR*newTPR-oldTPR*newFPR)
  AUC <- AUC/2 + .5
  
  return(list(TPR=rev(TPR),FPR=rev(FPR),AUC=AUC,TP=rev(saveTP),FP=rev(saveFP),TN=rev(saveTN),FN=rev(saveFN)))
}
