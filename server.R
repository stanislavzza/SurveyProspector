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

options(shiny.maxRequestSize = 200*1024^2) # allow large files
#outputOptions(output, "downloadLog", suspendWhenHidden=FALSE)
#https://groups.google.com/forum/#!topic/shiny-discuss/TWikVyknHYA

source("helpers.R")

############ GLOBALS
global <- list(dataUploadDone = FALSE,dataFiles = 0,logit = NULL, fitted <- NULL)

#keep track of the files loaded
dataFileInfo <- data.frame(Filename = "*ALL*",Index = "",Rows = 0,Columns=0,Status="",stringsAsFactors = FALSE)

#On/Off switch for vars
varInclude <- NULL
varIncludeOrder <- NULL
varOrder <- NULL
rangeCount <- 0


df <- data.frame(NoData=NA)
#dfVarStats <- data.frame()
dataFiltered <- FALSE
indexHash <- hash(c(" "))
AUClog <- 0 # save the AUC for the log

# this function needs the global index
get_description <- function(x) {
  if(is.null(x$var1)) return(NULL)
  paste0(indexHash[[x$var1]])
  #paste0(str(varName))
}

#######################################################################################
#                                    SERVER CODE                                      #
#######################################################################################

shinyServer(function(input, output, session) {
  
    #REACTIVE VALUES-------------------------------------------------------------------
    rvals <-reactiveValues(dataDoneButton=FALSE,dataLoaded=FALSE,refreshData=FALSE,
                           updateDataInfo = 0,interestCB=0,logitModel=0,newVarExists=0,
                           graphUpdate=c(0,0),graphAUC=c(0,0),graphReady=0)
  
    ############################### FILE UPLOADS #####################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
       
    #INPUT------------- Select index file for upload 

    output$indexFileUploader <- renderUI({
      if(rvals$dataLoaded) return("")
  
        fileInput('indexFile', 'Choose optional index (CSV) File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    })
     
    #OBSERVER---------- Process index data after upload
    loadIndex <- observe({
    
      inFile <- input$indexFile
      if (is.null(inFile)) return("")
      
      tempdf <-read.csv(inFile$datapath,header=FALSE)
      
      #read in the index file. The format is VARNAME<comma>Description, which may contain commas
      if (ncol(tempdf) > 2){ # in case there are already commas there...
        indexHash <<- fillhash(as.character(tempdf[,1]),apply(tempdf[,2:ncol(tempdf)],1,paste,collapse=" "),indexHash)
      } else {
        indexHash <<- fillhash(as.character(tempdf[,1]),tempdf[,2],indexHash)  
      }
    })
  
  
    #INPUT------------- Select data file for upload -----------------------
    output$dataFileUploader <- renderUI({
      if(rvals$dataLoaded) return("")
      
      fileInput('dataFile', 'Choose data File (CSV)',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
    })
    
    
    #INPUT------------- Select merge type for upload -----------------------
    output$mergeType <- renderUI({
      if(rvals$dataLoaded) return("")
      if (rvals$updateDataInfo < 1) return("")
      radioButtons("mergeType", "", c("Full join" = "full",  "Left join" = "left",
                                      "Inner Join" = "inner"),inline=TRUE)
      
    })
    
    #OBSERVER----------- Process data -------------------------------------
    loadDataFile <- observe({ 
      inFile <- input$dataFile
      mergeType <- isolate(input$mergeType) # what sort of join to use
      if (is.null(input$dataFile)){
        h4("")
      } else {
        tdf <- read.csv(inFile$datapath)
        
        # increment the number of files we've uploaded
        global$dataFiles <<- global$dataFiles + 1
        
        # load or merge data
        if (global$dataFiles == 1) {
            df <<- tdf
            status = "Loaded Okay"
        } else { #merge the data files, provided there is at least one common column
            if (length(intersect(names(df),names(tdf)))==0){
              status = "No common column to merge on!"
            } else {
              if(mergeType == "full") {
                 df <<- merge(df,tdf,all=TRUE,suffixes=c(global$datafiles-1,global$datafiles))
              } else if (mergeType == "left") {
                 df <<- df %>% left_join(tdf)
              } else { # inner
                 df <<- df %>% inner_join(tdf)
              }
              status = "Merged Okay"
              }
        }
        
        dataFileInfo <<- rbind(dataFileInfo,list(input$dataFile$name,denull(isolate(input$indexFile$name),""),nrow(tdf),ncol(tdf),status))
        dataFileInfo$Rows[1] <<- nrow(df)
        dataFileInfo$Columns[1] <<- ncol(df)
        rm(tdf)
        
        # turn on the 'finished' button and update the data tab
        isolate(rvals$dataDoneButton <- TRUE)
        isolate(rvals$updateDataInfo <- rvals$updateDataInfo + 1)
        
      }
    })
    
    #INPUT---------------------Slider for quantization---------------------
    output$numCatSlider <- renderUI({
      rvals$dataDoneButton # create a reactive dependency
      if(rvals$dataLoaded ||  !global$dataFiles) return("")
      sliderInput("quantCats", label = "Quantization",
                  min = 2, max = 20, value = c(5,10), step = 1,ticks=TRUE)
    })
    
    #INPUT---------------------Slider for setting NA tolerance for processing ----------
    output$NAtolerance <- renderUI({
      rvals$dataDoneButton # create a reactive dependency
      if(rvals$dataLoaded ||  !global$dataFiles) return("")
      sliderInput("NAtolerance", label = "% NAs acceptable",
                  min = 0, max = 99, value = 75, step = 1, ticks=TRUE)
    })
    
    #INPUT-----------------Button for finished loading data---------------
    output$dataDoneButton <- renderUI({
       rvals$dataDoneButton # create a reactive dependency
      if(rvals$dataLoaded || !global$dataFiles) return("")
      actionButton("dataDoneUploading","Done Uploading")
    })
    
    #OBSERVER--------------Quantize and categorize the data, hide the uploaders
    processData <- observe({
      if (!denull(input$dataDoneUploading[1]))
        return()
      
      numCats <- isolate(input$quantCats)
      
      # normalize data
      for (name in colnames(df)){
        
        # make sure we're not working on a variable we defined here, all of them end in underscore
        if(grepl("_$",name)){
          next
        }
        
        # first check for all NAs or other cases of a single value and delete if so, for containing no information
        ulist <- unique(unlist(df[[name]], use.names = TRUE))
        uniqueVals <- sum(!is.na(ulist)) # count the non-NA values
        
        # find the percentage of NAs in the column
        numNA <- sum(is.na(df[[name]]))
        
        # delete the column if it's too impoverished
        if(uniqueVals <= 1 || numNA/nrow(df) > isolate(input$NAtolerance) ){
          df[[name]] <<- NULL
        }
        
        # now check for factors and characters to convert to categories
        if ( is.factor(df[[name]]) || is.character(df[[name]])){
          if (uniqueVals > numCats[2]){  # if the number of categories is too big, chop it down
            t <- code_nominals(df,name,numCats[1])
          } else { # keep them all 
            t <- code_nominals(df,name,numCats[2])
          }
          index <- paste(t$names,sep="=", c(1:length(t$names)), collapse=" ")
          
          # add a new variable 
          newname <- paste0(name,"_cat_")
          indexHash[[newname]] <<- paste(indexHash[[name]],index)
          df[[newname]] <<- t$data
          
          # change the name of the old one to include two underscores
          invisiname <- paste0(name,"__")
          names(df)[names(df) == name] <<- invisiname
          indexHash[[invisiname]] <<- indexHash[[name]]
          
        } else {
          # finally, check for numerical columns with a large number of unique values to quantize
          if (uniqueVals>numCats[2]){
            newname <- paste0(name,"_quant_")
            df[[newname]] <<- ntile(df,name,numCats[1])
            indexHash[[newname]] <<- paste(indexHash[[name]],"(Quantized)")
            
            # change the name of the old one to include two underscores
            invisiname <- paste0(name,"__")
            names(df)[names(df) == name] <<- invisiname
            indexHash[[invisiname]] <<- indexHash[[name]]
          }
        }
        
      }
      
      # set the filter to all trues
      df$filter__ <<- TRUE
      
      # create some random columns for reality checks
      range <- numCats[1]
      
      df$R_A_N_D_1_ <<- sample(c(1:range),nrow(df),replace=TRUE)
      df$R_A_N_D_2_ <<- sample(c(1:range),nrow(df),replace=TRUE)
      df$R_A_N_D_3_ <<- sample(c(1:range),nrow(df),replace=TRUE)
      df$R_A_N_D_4_ <<- sample(c(1:range),nrow(df),replace=TRUE)
      df$R_A_N_D_5_ <<- sample(c(1:range),nrow(df),replace=TRUE)
      
      indexHash$R_A_N_D_1_ <<- "System random data. Predicts nothing."
      indexHash$R_A_N_D_2_ <<- "System random data. Predicts nothing."
      indexHash$R_A_N_D_3_ <<- "System random data. Predicts nothing."
      indexHash$R_A_N_D_4_ <<- "System random data. Predicts nothing."
      indexHash$R_A_N_D_5_ <<- "System random data. Predicts nothing."
      
      
      isolate(rvals$dataLoaded <<- TRUE)
      
      # updateTabsetPanel(session, "Main", selected = "Predictor")
    })
    
    #--------------------------------------------------------------------------------#
    ############################### END FILE UPLOADS #################################
  
    ############################### DATA PANEL  ######################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    
  
    #OUTPUT---------------------------- Describe File Uploads 
    output$dataPanelHeader <- renderTable({
      rvals$updateDataInfo # reactive dependency
      dataFileInfo
    })
    
    #OUTPUT-------------------  Create and show dataframe of variable characteristics
    output$varStats <- renderDataTable({
      rvals$updateDataInfo # dependency for intermediate uploads
      rvals$dataLoaded # dependency for final processing
      
      dftype <- c()
      dfN <- c()
      dfSummary <- c()
      dfNA <- c()
      dfUnique <- c()
      dfvar <- c()
      
      
      dfnames <- as.character(sort(names(df)))
      for (t in dfnames) {
          dftype <- c(dftype,class(df[[t]]))
          if (1) { #class(df[[t]]) == "numeric"){
              s <- paste(as.numeric(summary(df[[t]][!is.na(df[[t]])])),collapse=", ")
              v <- round(var(df[[t]],na.rm=TRUE),2)
          } else {
              s <- "NA"
              v <- "NA"
          }
      
          dfSummary <- c(dfSummary,s)
          dfvar <- c(dfvar,v)
          dfUnique <- c(dfUnique, length(unique(df[[t]][!is.null(df[[t]])])) )
          dfN <- c(dfN, sum(!is.na(df[[t]])) )
      }
      dfVarStats <<- data.frame(Variable = dfnames, Type = dftype, N = dfN, Unique = dfUnique, Min.1Q.Med.Mean.3Q.Max = dfSummary, Variance = dfvar)
      dfVarStats
    })
    
  
    #OUTPUT--------------------------- Provide a link to get the dataframe
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('SP-data', Sys.Date(), '.csv', sep="")
      },
      content = function(file) {
        write.csv(df, file = file, col.names=TRUE, quote=TRUE,na="",row.names=FALSE)
      },
      contentType = 'text/plain'
    )
    
    ############################### ANALYSIS CONTROLS ################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    
    #INPUT-------------------- Select an optional filter------------------------------
    output$filter1Select <- renderUI({ 
      
      if (!rvals$dataLoaded) return("")
      namelist <- as.list(sort(names(df)))
      mask <- grepl("__$",namelist)
      
      selectInput("filter1Var", "Optional Filter 1",c("No Filter",namelist[!mask]), multiple = FALSE )      
    })
    
    #INPUT-------------------- Select a filter range-----------------------------------
    output$filter1RangeSelect <- renderUI({ 
      if ( !denull(rvals$dataLoaded) ) return("")
      
      # force a reset to the target variable select, because predictors will have to be recalculated
      updateSelectInput(session, "targetVarRangeIn",selected="")
      updateSelectInput(session, "targetVarRangeOut",selected="")
      
      if (denull(input$filter1Var,"No Filter") == "No Filter") return("")
      
      namelist <- as.list(names(table(df[[input$filter1Var]])))
      selectInput("filter1Range", "Keep which values?", namelist, multiple = TRUE )
      
    })
    
    #OUTPUT------------------- Show the filter 1 histogram -------------------------------
    output$filter1Plot <- renderPlot({
      if ( !denull(rvals$dataLoaded) || denull(input$filter1Var,"No Filter") == "No Filter" ) return("")
      
      t <- table(df[[input$filter1Var]])
      
      if (is.null(input$filter1Range)) { # nothing to color
        barplot(t,col=c("skyblue"))
      }
      else { #color the selected bars
        colors <- rep("grey",length(t))
        colors[names(t) %in% input$filter1Range] <- "skyblue"
        barplot(t,col=colors)
      }
    })
    
    #OUTPUT------------------- Rows remaining for filter 1 -------------------------------
    output$filter1Cases <- renderUI({
      if ( !denull(rvals$dataLoaded) || denull(input$filter1Var,"No Filter") == "No Filter" | denull(input$filter1Range) == 0) return("")
      tags$b(paste("Number of rows selected ", sum(df[[input$filter1Var]] %in% input$filter1Range )))
    })
    
    #OUTPUT------------------- Description of  filter 1 -------------------------------
    output$filter1Desc <- renderText({
      if ( !denull(rvals$dataLoaded) || denull(input$filter1Var,"No Filter") == "No Filter" ) return("")
      paste(indexHash[[input$filter1Var]])
    })    
    
    #INPUT-------------------- Select filter logic------------------------------
    output$filterLogic <- renderUI({ 
      radioButtons("filterLogic", "", c("AND" = "and",  "OR" = "or"),inline=TRUE)
    })
    
    #OUTPUT-------------------- Report the samples remaining and set filter ------------------------------
    output$filterN <- renderUI({
      if (denull(input$filter1Var,"No Filter") =="No Filter"){
        filter1 <- rep(TRUE,nrow(df))
      } else {
        filter1 <- df[[input$filter1Var]] %in% input$filter1Range
      }
      
      if (denull(input$filter2Var,"No Filter") =="No Filter"){
        filter2 <- rep(TRUE,nrow(df))
      } else {
        filter2 <- df[[input$filter2Var]] %in% input$filter2Range
      }
      
      if (denull(input$filterLogic,"") == "or") {
        N  <- sum(filter1 | filter2, na.rm=TRUE)
        df$filter__ <<- filter1 | filter2
      } else {
        N  <- sum(filter1 & filter2,na.rm=TRUE)
        df$filter__ <<- filter1 & filter2
      }
  
      tags$h3(paste("Rows selected: ",N))
    })
    
    #INPUT-------------------- Select an optional filter 2------------------------------
    output$filter2Select <- renderUI({ 
      
      if (!denull(rvals$dataLoaded)) return("")
      namelist <- as.list(sort(names(df)))
      mask <- grepl("__$",namelist)
      selectInput("filter2Var", "Optional Filter 2",c("No Filter",namelist[!mask]), multiple = FALSE )      
    })
    
    #INPUT-------------------- Select a filter 2 range-----------------------------------
    output$filter2RangeSelect <- renderUI({ 
      if ( !denull(rvals$dataLoaded) ) return("")
      
      updateSelectInput(session, "targetVarRangeIn",selected="")
      updateSelectInput(session, "targetVarRangeOut",selected="")
      
      if( denull(input$filter2Var,"No Filter") == "No Filter" ) return("")
      
      namelist <- as.list(names(table(df[[input$filter2Var]])))
      selectInput("filter2Range", "Keep which values?", namelist, multiple = TRUE )
      
    })
    
    #OUTPUT------------------- Show the filter 2 histogram -------------------------------
    output$filter2Plot <- renderPlot({
      if ( !denull(rvals$dataLoaded) || denull(input$filter2Var,"No Filter") == "No Filter" ) return("")
      t <- table(df[[input$filter2Var]])
      
      if (is.null(input$filter2Range)) { # nothing to color
        barplot(t,col=c("skyblue"))
      }
      else { #color the selected bars
        colors <- rep("grey",length(t))
        colors[names(t) %in% input$filter2Range] <- "skyblue"
        barplot(t,col=colors)
      }
    })
    
    #OUTPUT------------------- Rows remaining for filter 2 -------------------------------
    output$filter2Cases <- renderUI({
      if ( !denull(rvals$dataLoaded) || denull(input$filter2Var,"No Filter") == "No Filter" || is.null(input$filter2Range)) return("")
      tags$b(paste("Number of rows selected ", sum(df[[input$filter2Var]] %in% input$filter2Range )))
    })
    
    #OUTPUT------------------- Description of  filter 2 -------------------------------
    output$filter2Desc <- renderText({
      if ( !denull(rvals$dataLoaded) || denull(input$filter2Var,"No Filter") == "No Filter" || denull(input$filter2Range) == 0) return("")
      paste(indexHash[[input$filter2Var]])
    })       
    
################################ END of FILTER Tab ###################################

    #INPUT--------------------- Select the Target Var to predict-----------------------
    output$targetVar <- renderUI({ 
      # turning off the newvars update for now--it's quite annoying
      #rvals$newVarExists # update the list of names if a new var is created
      if (!rvals$dataLoaded) return("")
      namelist <- as.list(sort(names(df)))
      mask <- grepl("__$",namelist)
      selectInput("targetVar", "Which variable to predict?",  namelist[!mask] )
    })
    
    #INPUT -------------------- Select the target inclass range--------------------------------
    output$targetRangeIn <- renderUI({ 
      if (!rvals$dataLoaded || is.null(input$targetVar)) return("")
        namelist <- as.list(names(table(df[[input$targetVar]])))
        selectInput("targetVarRangeIn", "Select in-class", namelist, multiple = TRUE )
    })

    #INPUT -------------------- Select the target outclass range--------------------------------
    output$targetRangeOut <- renderUI({ 
      if (!rvals$dataLoaded || denull(input$targetVarRangeIn,"")=="") return("")
      namelist <- setdiff(as.list(names(table(df[[input$targetVar]]))),input$targetVarRangeIn)
      selectInput("targetVarRangeOut", "Select out-class", namelist, selected = namelist, multiple = TRUE )
    })
    
    
    #INPUT------------------- Checkbox to force order-----------------------------------
    output$forceOrderCB <- renderUI({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      checkboxInput("forceOrder", label = "*Force Sequential", value = TRUE)  
      
    })
    
    
    #INPUT---------------------Slider for sample size---------------------
    output$predictorSamples <- renderUI({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      n <- nrow(df)
      sliderInput("predictorSamples", label = "Min <-> Max Samples for Prediction",
                  min = 5, max = n, value = c(min(300,round(n*.75,0)),min(1000,n)), step = 20, ticks=TRUE)
    })
    
    #INPUT -------------------- Button to update predictors-----------------------------
    output$action <- renderUI({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      actionButton("action","Create Predictors")
    })
    

    #OBSERVE ------------------- Generate new predictors based on inputs-----------------
    predictors <- reactive( {
      #if(denull(input$action[1],0)==0) return() # only runs when the button has been toggled and increments the counter
      
      targetVar <- isolate(input$targetVar)
      targetVarRangeIn <- isolate(input$targetVarRangeIn)
      targetVarRangeOut <- isolate(input$targetVarRangeOut)
      forceOrder <- isolate(input$forceOrder)
      sampleMin <- isolate( input$predictorSamples[1] )
      sampleMax <- isolate(input$predictorSamples[2])
      
      # initialize the Booleans for the co-predictors and ensemble tabs
      varInclude <<- fillhash(names(df),rep(0,length(names(df))))
      
      # initialize the Booleans for the "best order" box
      varOrder <<- fillhash(names(df),rep(0,length(names(df))))
      
      
      #create a column of {0,1} to describe the in-class
      df$predicted <<- as.numeric(df[[targetVar]] %in% targetVarRangeIn)
      
      # replicate the NAs, so we're not trying to predict them too
      df$predicted[is.na(df[[targetVar]])] <<- NA
      
      # if the out-class excludes values, turn those into NAs too
      namelist <- setdiff(as.list(names(table(df[[targetVar]]))),
                          union(targetVarRangeIn,targetVarRangeOut))
      df$predicted[df[[targetVar]] %in% namelist] <<- NA
      
      # adjust the sampleMin downward if the predicted has fewer samples
      sampleMin <- min(sampleMin, sum(!is.na(df$predicted[df$filter__])))
      
      #yval <- new.env( hash = TRUE, parent = emptyenv()) # hash(names(df))
      #xval <- new.env( hash = TRUE, parent = emptyenv()) #hash(names(df))
      auc <-  new.env( hash = TRUE, parent = emptyenv()) #hash(names(df))
           
      bestpred <- c()
      
      for(i in names(df)){
        #skip categorical variables, marked with two trailing underscores
        
        if (grepl("__$",i)){
          next
        }
        
        N <- sum(df$filter__,na.rm=TRUE)
        # check to see if we have a large number of rows, and sample instead of calculating all of it
        if (N > sampleMax) {
          mySample <- sample(N,sampleMax)
          tbl <- table(df[[i]][df$filter__][mySample],df$predicted[df$filter__][mySample])
        } else {
          tbl <- table(df[[i]][df$filter__],df$predicted[df$filter__])
        }
        
        #skip if we don't have at least sampleMin observations (from the slider) or if the table is vaccuous
        if (sum(tbl)<sampleMin || dim(tbl)[1] < 2 || dim(tbl)[2] < 1){
          next
        }
        
        rates <- tbl[,2]/tbl[,1]
        
        if (forceOrder == TRUE){
          o <- c(1:length(rates))
        } else {
          o <- order(-rates) # so rates[o[1]] should be the largest
        }

        t <- computeAUC(tbl,o)

        if (is.null(t) || is.nan(t) || is.infinite(t)){ # not a useful predictor, drop from the list
          rm(list = i,envir=auc) # remove the name from the environment (aka hash)
        } else {
          if (t<.5){ # in case the 1,2,3 order forced by forceOrder makes a 'backwards' predictor
            t <- 1-t
          }
          auc[[i]] <- t
        }  
      }
      #create the order
      
      # note: this fails if there aren't enough sample
      if (is.null(auc$predicted)) { # not enough samples
          updateSliderInput(session, "predictorSamples", value = c(1, NA))
      } else {
        bestpred <- as.list(auc)[ order(-sapply(as.list(auc), function(x) x[1]))]
      }
      
      return(bestpred)
      
    })
    
    #INPUT-------------------- Select a predictor--------------------------------------
    output$predictor <- renderUI({ 
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      if(denull(input$action[1],0)==0) return() # dependency on new button clicks
      rvals$interestCB # add or subtract + for items of interest
      if (is.null(isolate(input$targetVarRangeOut)) ) return("")
      
      #snapshot the conditions
      bestpred <- predictors()
      t <- as.list(names(bestpred))
      if (isolate(input$forceOrder) == TRUE){
          closeParen <- "*)"
      } else {
          closeParen <- ")"
      }
      
      # return list of sorted variable names. Asterisk means forced order, + means tagged as interesting
      names(t) <- paste(names(bestpred)," (",round(as.double(bestpred),digits = 2),closeParen,
                        lapply(names(bestpred),function(x){if(denull(varInclude[[x]],0)==1){ return("+") } else {return("")}}),
                        sep="")
      if (denull(isolate(input$predictor),"")!=""){
        selected <- isolate(input$predictor)
      } else {
        selected <- NULL
      }
      selectInput("predictor", "Select Predictor", t , multiple = FALSE, selected = selected )
    })
    
    
    #INPUT----------------------------  Checkbox to indicate interest ------------------
    output$interestCB <- renderUI({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      input$predictor # dependency
      
      if(denull(input$predictor,"")=="") return("")
          
      if (denull(varInclude[[input$predictor]],0)){
        cbStatus <- TRUE
      } else {
        cbStatus <- FALSE
      }
      checkboxInput("interestCB","Include in Regression",value= cbStatus)
      #HTML(paste0('<input type=checkbox name=interestCB value=',input$predictor,' ',cbStatus,'> Item of Interest'))
    })
    

    #OBSERVE-----------------------watch the checkbox and update the list of variables of interest--
    # This one can add to and remove. The ones on the co-predictors table can only add
    checkboxUpdater <- observe({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      input$interestCB #dependency
      var <- denull(input$predictor,"")
      if (var == "") return("")
      val <- denull(input$interestCB,0)
      varInclude[[var]] <<- val
      varStats <- graphprep() # this generates the order
      varIncludeOrder[[var]] <<- varStats$besto
      isolate(rvals$interestCB <<- rvals$interestCB + 1) # this triggers changes based on this processing
    })
    
    #input---------------------------- Checkbox to choose Order to use ----------------
    output$bestOrderCB <- renderUI({
      if (denull(input$targetVarRangeOut,"")=="" || denull(input$targetVarRangeIn,"")=="") return("")
      if(denull(input$predictor,"")=="") return("")
      
      if (denull(varOrder[[input$predictor]],0)){
        cbStatus <- TRUE
      } else {
        cbStatus <- FALSE
      }
      checkboxInput("bestOrderCB","Use Best Order",value= cbStatus)
    })
    
    #OBSERVE-----------------------watch the checkbox and update the list of variable order-- --
    # This one can add to and remove. The ones on the co-predictors table can only add
    orderCheckboxUpdater <- observe({
      input$bestOrderCB #dependency
      var <- denull(input$predictor,"")
      if (var == "") return("")
      val <- denull(input$bestOrderCB,0)
      varOrder[[var]] <<- val
    })
    
    
    #INPUT ------------------------- Checkbox to invert filter-----------------------
    output$InvertFilterCB <- renderUI({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="" || denull(input$filterIgnore)==1) return("")
      input$predictor # dependency
      if(sum(df$filter__) == nrow(df))return("") # ignore filter controls if it's everything
      if (length(df$filter__[!df$filter__]) < 5) return("Too few samples!")     # too small to be useful
        
      checkboxInput("filterComp", label = "Invert Filter", value = FALSE) 
    })
    
    #INPUT----------------------------  Checkbox to ignore filter---------------------
    output$IgnoreFilterCB <- renderUI({
      if (is.null(input$targetVarRangeOut)|| denull(input$targetVarRangeIn,"")=="") return("")
      input$predictor # dependency
      if(sum(df$filter__) == nrow(df))return("")
      if (length(df$filter__[!df$filter__]) < 5) return(" ") # too small to be useful
      
      checkboxInput("filterIgnore", label = "Ignore Filter", value = FALSE)  
    })
    
    #--------------------------------------------------------------------------------#
    ############################ END ANALYSIS CONTROLS ###############################
    
    ############################### PREDICTOR TAB ####################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

    #OUTPUT----------------------- Display Vars information
    output$indexInfo <- renderUI({
      input$targetVar
      input$targetVarRangeIn
      input$predictor
      #input$filter1Var
      #input$filter1Range
      analysisInfo()
    })

    #Function---------------------- Update Vars Information  
    analysisInfo <- function(){
        N <- list() 
        
        if (is.null(input$targetVar)){
          return(" ")
        } else {
          targetVar <- input$targetVar
          available <- !is.na(df[[targetVar]])
          N$target <- sum(available)
        }
      
        if (denull(input$targetVarRangeIn,"")=="" || is.null(input$targetVarRangeOut)){
          targetVarRange <- ""
          targetVarRangeOut <- ""
        } else {
          targetVarRange <- denull(input$targetVarRangeIn) 
          targetVarRangeOut <- denull(input$targetVarRangeOut) 
          N$inclass <- sum(df[[targetVar]] %in% targetVarRange, na.rm=TRUE)
          N$outclass <- sum(df[[targetVar]] %in% targetVarRangeOut, na.rm=TRUE)
          N$target <- N$inclass + N$outclass
        }
      
        if (is.null(input$predictor)){
          predictor <- " "
        } else {
          predictor <- input$predictor
          available <- available & !is.na(df[[predictor]])
          N$predictor <- sum(available)
        }
      
        N$filter <- sum(available & df$filter__,na.rm=TRUE)
      
        if (is.null(input$filterComp)){
          filterComp <- FALSE
        } else {
          filterComp <- input$filterComp
        }
        
        filterIgnore <- input$filterIgnore
        if (is.null(filterIgnore)){
          filterIgnore <- 0
        } else {
          filterIgnore <- input$filterIgnore
        }
      
        # start with the predictee
        content <- paste("Predicting",as.character(targetVar),
                         "in {",paste(targetVarRange,collapse=","),"} versus {",
                         paste(targetVarRangeOut,collapse=","),"}")
        content <- c(content,"Usable rows:",N$target,"(",N$inclass,"in-class and ",N$outclass,"out-class)")  
      
        if(!is.null(indexHash[[targetVar]])){
            content <- c(content,as.character(indexHash[[targetVar]]))
        }
        
        if (predictor != " ") {
          content <- c(content,"By using",as.character(predictor))
          content <- c(content,"(usable rows now =",N$predictor,")")
        }
        
        if(!is.null(indexHash[[predictor]])){
            content <- c(content,as.character(indexHash[[predictor]]))
        }
        
        if (filterComp){
          N$filter <- sum(available & !df$filter__)
        }
        
        if (sum(df$filter__) < nrow(df) && filterIgnore == 0) {
          content <- c(content,"Subject to filter with usable rows = ", N$filter)           
        }
        
        content
  }
 
  #OBSERVE---------------------------Make calculations in order to do the graphs-----
  graphprep <- reactive({
    input$forceOrder
    if(is.null(input$predictor)) return(NULL)

    if (denull(input$filterIgnore,0) !=0){ # temporarily turn off the filter
         df$filter__ <- TRUE
    } else {
      if (denull(input$filterComp,0) != 0){
        df$filter__ <- !df$filter__
      }
    }
    
    # should we complement the data based on the filter? df$filter__ is all TRUE unless filter

    tbl <- table(df[[input$predictor]][df$filter__],df$predicted[df$filter__])

    
    if (sum(tbl)==0 || dim(tbl)[1] < 2 || dim(tbl)[2] < 2) return() # nothing to do
    
    rates <- tbl[,2]/tbl[,1]
    plotrates <- tbl[,2]/rowSums(tbl)
    
    # save the best order
    besto <- o <- order(-rates)
    
    # switch if forced to
    if (denull(input$bestOrderCB,0) == 0){
      o <- c(1:length(rates))
    } else {
      o <- besto
    }
    
    yval <- cumsum(tbl[,2][o])/sum(tbl[,2])
    xval <- cumsum(tbl[,1][o])/sum(tbl[,1])
    
    #calculate the AUC for each variable as a predictor
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
    auc <- (mysum + oldy)/2
    
    if (auc<.5){
       auc <- 1 - auc
       o <- rev(o)
       yval <- cumsum(tbl[,2][o])/sum(tbl[,2])
       xval <- cumsum(tbl[,1][o])/sum(tbl[,1])
    }
    
    return(list(tbl=tbl,o=o,besto=besto,yval=yval,xval=xval,auc=auc,rates=rates,plotrates=plotrates))
  })

  #OUTPUT-------------------------------graph frequencies as bar chart---------------------------
  output$graph <- renderPlot({
  input$forceOrder
  prep <- graphprep()
  if(is.null(prep)){
    return(" ")
  } 
  tbl = prep$tbl
  o = prep$o
  yval = prep$yval
  xval = prep$xval
  rates = prep$rates
  plotrates=prep$plotrates
  auc = prep$auc
  
  # the following transponses and rearranges the table for presentation
  barplot(t(tbl)[c(2,1),o], main= paste("Distribution of ", input$predictor),
          xlab="Predictor values", 
          ylab="Num. Obs.", col=c("skyblue","pink"), names.arg = rownames(tbl)[o],
          space = .05)
  legend("topright", fill = c("skyblue","pink"), legend = c("In-class","Out-class"))
  # calculate where labels need to be at space = .05
  ldist <- (1:nrow(tbl) - 1)*1.05+.5
  text(ldist,rowSums(tbl)[o],labels=round(plotrates[o],2),pos=3,xpd=TRUE)
  
})

  #OUTPUT-----------------------graph frequencies with confidence intervals------------------------
  output$freq <- renderPlot({
  input$forceOrder
  prep <- graphprep()
  if(is.null(prep)){
    return("")
  } 
  tbl = prep$tbl
  o = prep$o
  yval = prep$yval
  xval = prep$xval
  rates = prep$rates
  plotrates=prep$plotrates
  auc = prep$auc
  
  ci <- binom.confint(tbl[,2], rowSums(tbl), conf.level = 0.95, methods = "agresti-coull")
  ciup <- ci$upper
  cidn <- ci$lower
  
  #plot(xval,yval,type="b")
  # the following transponses and rearranges the table for presentation
  plot(plotrates[o], type="c", main= paste("50%/95% Conf Intervals for", input$predictor),
          ylab=paste("Pr[In-class|",input$predictor,"=index]"),xaxt='n')
  polygon(c(1:length(plotrates),rev(1:length(plotrates))),c(pmin(ciup[o],1),rev(pmax(cidn[o],0))),col=rgb(.9, .1, .1, 0.3))
  ci <- binom.confint(tbl[,2], rowSums(tbl), conf.level = 0.5, methods = "agresti-coull")
  ciup <- ci$upper
  cidn <- ci$lower
  polygon(c(1:length(plotrates),rev(1:length(plotrates))),c(pmin(ciup[o],1),rev(pmax(cidn[o],0))),col=rgb(.9, .1, .1, 0.5))
  text(c(1:length(plotrates)),plotrates[o],labels=rownames(tbl)[o],cex=1.5)
  
})

  #OUTPUT-----------------make ROC graph-----------------------------------------------------------
  output$ROC <- renderPlot({
  prep <- graphprep()
  if(is.null(prep)){
    return("")
  } 
  cutoff <- denull(input$cutoffSlider,1)
  tbl = prep$tbl
  o = prep$o
  yval = prep$yval
  xval = prep$xval
  rates = prep$rates
  plotrates=prep$plotrates
  auc = prep$auc
  AUClog <<- round(auc,2)
  
  plot(c(0,xval),c(0,yval),main=paste("ROC Curve, AUC =",round(auc,2)),type="c",xlim=c(0,1),ylim=c(0,1),xlab="False Positive Rate",ylab="True Positive Rate")
  points(c(0,1),c(0,1),type="c",col="red")
  text(xval,yval,labels = rownames(tbl)[o])
  text(xval[cutoff],yval[cutoff],labels="O",col="red",cex=2)
})

output$tableTitle <-renderUI({
  prep <- graphprep()
  if(is.null(prep)) { return(h4(""))}
  h4("Predictor Performance")
})

output$performance <-renderTable({
  input$cutoffSlider # dependency
  
  prep <- graphprep()
  if(is.null(prep$tbl) || is.null(input$cutoffSlider)){
    return(data.frame())
  } 
  tbl <- prep$tbl[prep$o,]  # get the permuted table
  
  #predictedIn <- tbl[,2]/tbl[,1] > sum(tbl[,2])/sum(tbl[,1])
  predictedIn <- c(1:input$cutoffSlider)
  range <- c(1:length(prep$o))
  predictedOut <- range[is.na(pmatch(range,predictedIn))] #complement the range
  
  TP <- sum(tbl[,2][predictedIn])
  FN <- sum(tbl[,2][predictedOut])
  FP <- sum(tbl[,1][predictedIn])
  TN <- sum(tbl[,1][predictedOut])
  
  Total <- sum(tbl)
  True <- sum(tbl[,2])
  False <- sum(tbl[,1])
    
  perf <- data.frame(Predicted = c("In","Out","Total","Fraction"), 
       InClass = c(TP,FN,TP+FN,round(True/Total,2)), 
       #InClassPct = c(round(TP/True,2),round(FN/True,2),round((TP+FN)/True,2)),
       OutClass=c(FP,TN,FP+TN,round(False/Total,2)),
       Accuracy=c(round(TP/(TP+FP),2),round(TN/(TN+FN),2),round((TP+TN)/(TP+FP+FN+TN),2),NA))
  return(perf)
  })

  #INPUT------------------------------slider to set cut-off for predictor model ------------------------
  output$slider1 <- renderUI({
      prep <- graphprep()
      if(is.null(prep)) { return(" ")}
      sliderInput("cutoffSlider", label = strong("Cut-off Value"),min = 1, max = length(prep$o), value = 1, step = 1,ticks=TRUE)
  })

  #OUTPUT-----------------------------text to show the list of values included in the predictor---------
  output$predictSet <- renderUI({ 
      prep <- graphprep()
      if(is.null(prep)) { return(" ")}
      cutoff <- denull(input$cutoffSlider,1)
      predictedIn <- prep$o[c(1:cutoff)]
      predictedIn <- paste(predictedIn,collapse=",")
      em(paste("Predicts in-class if",input$predictor,"is in set {",predictedIn,"}"))
  })

  #OUTPUT-----------------------------text to give Fisher's result--------------------------------------
  output$fishersExactTest <- renderText({
      prep <- graphprep()
      if(is.null(prep)) { return(" ")}
      if (sum(prep$tbl) < 1000) {
        f <- fisher.test(prep$tbl,hybrid = TRUE)
        paste("Fisher's Exact Test, p-value = ",round(f$p.value,3))
      } else {
        f <- chisq.test(prep$tbl)
        paste("Chi-squared, p-value = ",round(f$p.value,3))
      }
  })

  #OUTPUT-----------------------------text to give Wilcox result----------------------------------------
  output$wilcox <- renderText({
      prep <- graphprep()
      if(is.null(prep)) { return(" ")}
      w <- wilcox.test(df[[input$predictor]] ~ df$predicted)
      paste("Wilcox test to reject H0 = {same in/out class distribution}, p-value = ", round(w$p.value,3))
  })



#--------------------------------------------------------------------------------#
############################ END PREDICTOR PANEL #################################


    ############################### CO-PREDICTOR TAB ####################################
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
    #OUTPUT----------------------- Display Vars information
    output$indexInfoCo <- renderUI({
      input$targetVar
      input$targetVarRangeIn
      input$predictor
      input$filter1Var
      input$filter1Range
      analysisInfo(short=TRUE)
    })    

    output$copredictors <- renderDataTable({
        input$interestCB # set dependency on the checkbox that identifies interest
        
        bestpred <- head(predictors(),50)
        if (isolate(input$forceOrder) == TRUE){
          asterisk <- "*" # mark the AUCs that were forced 1.2.3...
        } else {
          asterisk <- ""
        }
        
        correlation <- NULL
        n <- NULL
        status <- NULL
        description <- NULL
        jointAUC <- NULL
        
        #filter toggles
        if (denull(input$filterIgnore) == 1) {
          df$filter__ <- TRUE
        } else {
          # should we complement the data based on the filter? df$filter__ is all TRUE unless filter
          if (denull(input$filterComp) == 1){
            df$filter__ <- !df$filter__
          }
        }
        
        predicted <- df$predicted[df$filter__]
        p <- df[[input$predictor]][df$filter__]
        
        auc <- paste(round(as.double(bestpred),digits = 2),asterisk,sep="")
        for(loopVar in names(bestpred)) { # slow, needs to be optimized
          l <- df[[loopVar]][df$filter__]
          correlation <- c(correlation,round(cor(p,l,use="pairwise.complete.obs"),2))
          n <- c(n, sum(!is.na(p) & !is.na(l) ))
         
          if (is.null(indexHash[[loopVar]])) {
            description <- c(description," ")
          } else {
            description <- c(description,as.character(indexHash[[loopVar]]))
          }
          
          # now compute the 2D ROC
          tbl <-  table(p,l,predicted)
          sumTrue <- sum(tbl[,,2])
          sumFalse <- sum(tbl[,,1])
          tpr <- tbl[,,2]/sumTrue # the rate at which we accumulate true positives
          fpr <- tbl[,,1]/sumFalse # the rate at which we accumulate false positives
          ratio <- tpr / (fpr + .0000001) 
      
          yvalOld <- 0
          xvalOld <- 0
          s <- 0
          for (x in sort(as.double(ratio ), decreasing = TRUE)) {
             yval <- sum(tbl[,,2][ratio >= x]) / sumTrue
             xval <- sum(tbl[,,1][ratio >= x]) / sumFalse
             s <- s + xval*yvalOld - yval*xvalOld
             yvalOld <- yval
             xvalOld <- xval
          }
          s <- (s + yvalOld)/2
          jointAUC <- c(jointAUC,round(s,2))
        }
        
        tdf <- data.frame(Var = paste(names(bestpred), substr(description,1,45)),N = n, AUC = auc, jointAUC = jointAUC,
                          Correlation = correlation)
        tdf
        
  })

  ################################## Network Tab ##################################################
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
  #INPUT------------------------------slider to set cut-off for predictor model ------------------------
  output$numberSelector <- renderUI({
    sliderInput("numberSelector", label = strong("How many connections?"),
                min = 0, max = 151, value = c(1,31), step = 5,ticks=TRUE)
  })
  
  #INPUT------------------------------slider to set cut-off for num predictors ------------------------
  output$numberPredictors <- renderUI({
    sliderInput("numberPredictors", label = strong("How many variables?"),
                min = 1, max = 50, value = c(1,16), step = 1,ticks=TRUE)
  })

  #INPUT------------------------ Button to generate network anew --------------------------
  output$networkButton <- renderUI({
    actionButton("networkButton","Generate Network")
  })

  #OUTPUT------------------------ Text to tell us how strong the correlation is----------------------
  output$maxCor <- renderUI({
    val = rvals$graphUpdate # set dependency, brings back the min correlation
    paste0(round(val[1],3)," >= |correlation| >= ",round(val[2],3))
  })

  #OUTPUT------------------------ Text to tell us what AUCs are shown----------------------
  output$maxAUC <- renderUI({
    val = rvals$graphAUC # set dependency, brings back the min correlation
    paste0(round(val[1],3)," >= AUC >= ",round(val[2],3))
  })

  #INPUT------------------- Checkbox for fixed layout -----------------------------------
  output$forceLayout <- renderUI({
   
    checkboxInput("forceLayout", label = "Fixed Layout", value = FALSE)  
    
  })

  #REACTIVE ---------------------- Generate network graph---------------------------------------------
  networkObserver <- observeEvent(input$networkButton,{
 #     if(denull(input$networkButton[1],0)==0) return()  # dependency on the incrementing of the regression button clicks
      lower <- denull(isolate(input$numberPredictors[1]),1)
      upper <- denull(isolate(input$numberPredictors[2]),16)
      
      # NOTE: copied the following code from the other tab. All this needs recoding! 
      
      #input$interestCB # set dependency on the checkbox that identifies interest
      bestpred <- predictors() # we will delete two
      bestpred <- head(bestpred,min(upper+2,length(bestpred)))
      bestpred <- bestpred[(lower+2):min(upper+2,length(bestpred))] # delete the 'predicted' var and the variable itself, which are at the top
                 
        correlation <- NULL
        n <- NULL
        status <- NULL
        description <- NULL
        jointAUC <- NULL
      
        # first subset the data if there is a filter
        # temporarily turn off the filter
        if (denull(input$filterIgnore) == 1) {
          df$filter__ <- TRUE
        } else {
          # should we complement the data based on the filter? df$filter__ is all TRUE unless filter
          if (denull(input$filterComp) == 1){
              df$filter__ <- !df$filter__
          }
        }
        tdf <- df[df$filter__,]
        
        # generate correlation matrix ##########################
        tdf <- tdf %>% select(one_of(names(bestpred)))  # one_of is like %in% 
      
        ct <- cor(tdf,use="pairwise.complete.obs")
        ct[upper.tri(ct,diag=TRUE)] <- 0
        
        # convert row names to a column
        ct<-data.frame(rownames(ct),ct)
        colnames(ct)[1]="var1"
        
        v <<- ct %>% gather(var2,correlation,-var1) %>% filter(correlation != 0) 
      
        v$color <<- 'red'  #rgb(1,0,0,abs(v$correlation[v$correlation <0])) #"red"
        v$color[v$correlation > 0] <<- 'black' # rgb(0,0,0,abs(v$correlation[v$correlation >0])) #"black"
        v$correlation <<- abs(v$correlation)
      
        g <<- graph.data.frame(v, directed=FALSE, vertices=NULL)
        graph_layout_fixed <<- layout.fruchterman.reingold(g,area=800*(vcount(g)^2))
        isolate(rvals$graphReady <- rvals$graphReady + 1)
        return(0)
  })

#output$ggvis_networkResults <- reactive({
graph_edges <- reactive({
  rvals$graphReady
  if(denull(isolate(input$networkButton[1]),0)==0) return(data.frame(var1='No Data',x=0,y=0,correlation=0,color='black',group=1))  # reactive on button
  nvert <- input$numberSelector[2] # reactive 
  clip <- input$numberSelector[1]
  # select top N vertices
  #top <- top_n(E(g)$correlation,nvert,correlation) # best improvers, E(g) brings back edges of g
  
  # http://igraph-help.nongnu.narkive.com/m0456GxD/threshold
  #top <- sort(abs(E(g)$correlation), decreasing=TRUE)
  #top <- top[clip:min(nvert,length(top))]
  top <- v %>% arrange(desc(correlation)) #sort(v$correlation, decreasing=TRUE)
  top <- top[clip:min(nvert,nrow(top)),]
  threshold <- min(top$correlation)
  up_threshold <- max(top$correlation)
  #g <- delete.edges(g,  c(which(E(g)$correlation < threshold),which(E(g)$correlation > up_threshold)))
                    #which(E(g)$correlation < threshold | E(g)$correlation > up_threshold)-1)
 # g <- delete.edges(g, which(E(g)$correlation > up_threshold)-1)
  #g <- remove.edge.attribute(g, "correlation")
  #g <- remove.edge.attribute(g, "color")
  #g <- delete.vertices(g,which(degree(g)<1))
 
  # now retrieve the names of the remaining ones and find the max/min AUC 
  vnames <- v$var1
  bestpred <- predictors()
  bestpred <- bestpred[names(bestpred) %in% vnames]  
  minAUC <- min(as.numeric(bestpred))
  maxAUC <- max(as.numeric(bestpred))
  
  if (input$forceLayout == 1) {
    graph_layout <- graph_layout_fixed
  } else {
    g <- graph.data.frame(top, directed=FALSE, vertices=NULL)
    graph_layout <- layout.fruchterman.reingold(g,area=800*(vcount(g)^2)) # generates different graph each invocation
  }
  g$layout <<- 2*graph_layout # this is an easy way to spread it out
  
  rvals$graphUpdate = c(up_threshold,threshold) # reactive value to trigger text update
  rvals$graphAUC = c(maxAUC,minAUC) #reactive value to update the AUC filter
 
  # the following lines add the reverse directions and group each edge
  top$group <- 1:nrow(top)
  top2 <- top 
  colnames(top2) <- c("var2","var1","correlation","color","group")
  top2 <- top2[c("var1","var2","correlation","color","group")]
  top <- rbind(top,top2)
 
  top_names <- V(g)$name
  g_df <- data.frame(var1=top_names, x=graph_layout[,1],y=graph_layout[,2]) 
 
  #graph_edges <- 
  #return value 
  top %>% left_join(g_df,by="var1") 
  
  #plot(g, asp=0,vertex.label.dist=0,vertex.label.cex=1.5,vertex.color = get.edge.attribute(g,"color"),vertex.label.color = "black",vertex.size=1, edge.width=3)
})#, height = 1000, width = 1000)

graph_edges %>% ggvis(~x,~y) %>% layer_text(text := ~var1, dx:=0, dy :=0) %>% 
  group_by(group) %>% layer_lines( stroke := ~color, strokeWidth :=2, opacity := ~correlation) %>% 
  hide_axis("x") %>% hide_axis("y") %>% add_tooltip(get_description, "hover") %>%  bind_shiny("ggvis_networkResults")


  ################################# Logistic Regression Tab #######################################
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #OUTPUT----------------------- Display Vars information------------------------------------------
  output$indexInfoEnsemble <- renderUI({
    input$targetVar
    input$targetVarRangeIn
    input$predictor
    input$filter1Var
    input$filter1Range
    analysisInfo()
  })

  #OUTPUT----------------------- Variable Selection -----------------------------------------------
  output$variableSelection <- renderUI({
    
    rvals$interestCB # reactive value dependency, triggered when the interest list changes
        
    #create a named list of choices
    #varInclude[[input$predictor]] <- 1 # as a courtesy, so it's not entirely blank
    choices <- as.list(varInclude)
    choices <- sort(names(choices[choices == 1]))
    if(length(choices) == 0) return("")
    selected <- choices
    descriptions <- lapply(choices,function(varName){substr(as.character(indexHash[[varName]]),1,35)})
    names(choices) <- paste(choices,descriptions,"...")
        
    checkboxGroupInput("modelVars", "Include which Variables?", choices, selected = selected, inline = FALSE)
  })

  #INPUT----------------------- Checkboxes to use best order (instead of 1,2,3...)-----------------
  output$orderSelection <- renderUI({
    
    rvals$interestCB # reactive value dependency, triggered when the interest list changes
    
    #create a named list of choices
    choices <- as.list(varInclude)
    choices <- sort(names(choices[choices == 1]))
    if(length(choices) == 0) return("")
    
    selected <- c()
    # find out which ones have best order chosen
    for(varName in choices) {
      if (varOrder[[varName]]) selected <- c(selected,varName)
    }
    
    names(choices) <- lapply(choices,function(varName){paste(varIncludeOrder[[varName]], collapse=",")})
    
    checkboxGroupInput("modelOrder", "Use Best Order?", choices, selected = selected, inline = FALSE)
  })

  #INPUT----------------------- Checkboxes to remove variables ---------------------------------------
  output$removeSelection <- renderUI({
    rvals$interestCB # reactive value dependency, triggered when the interest list changes
    choices <- as.list(varInclude)
    choices <- sort(names(choices[choices == 1]))
    if(length(choices) == 0) return("")
    names(choices) <- rep(" ",length(choices))
    checkboxGroupInput("removeSelection", "Remove", choices, inline = FALSE)
  })
    
  #OBSERVE---------------------- Change status when checkboxes remove interest variables -------------
  removeInterest <- observe({

    for(loopVar in input$removeSelection){
      varInclude[[loopVar]] <- 0
    }
    isolate(rvals$interestCB <<- rvals$interestCB + 1) # this triggers changes based on this processing
    
  })

  #INPUT------------------------ Button to generate regression analysis --------------------------
  output$regressionButton <- renderUI({
    if (is.null(input$modelVars)) return("") # nothing to do yet
    actionButton("regressionButton","Run Regression")
  })

  #INPUT------------------------ Input to create new variable from those checked off --------------
  output$newVarName <- renderUI({
    rvals$logitModel # set dependency
    if (is.null(global$logit)) return("") # nothing to do yet
    textInput("newVarName", "", value = "myPredictor")
  })

  #INPUT------------------------ Button to create new variable ------------------------------------
  output$newVarButton <- renderUI({
    rvals$logitModel # set dependency
    if (is.null(global$logit)) return("") # nothing to do yet
    actionButton("newVarButton","Create New Variable")
  })

  #OBSERVE---------------------- Handler to create the new variable -------------------------------
  createNewVar <- observeEvent(input$newVarButton,{
    # if(denull(input$newVarButton[1],0)==0) return() # set dependency
     if (is.null(global$logit)) return()
     if (is.null(isolate(input$newVarName))) return()
     
     # fit the model to everything in the df, not just the filtered part
     fitted <- predict.glm(global$logit,newdata=df,type = "response")
     
     # ensure the name is syntactically okay. It may replace an existing one!
     newVarName <- make.names(isolate(input$newVarName))
     if(!grepl('_$',newVarName)){
       newVarName <- paste0(newVarName,"_")
       newVarNameFloat <- paste0(newVarName,"__")
     } else {
       newVarNameFloat <- paste0(newVarName,"_")
     }
     
     df[[newVarNameFloat]] <<- fitted
     
     # now quantize according to settings for download
     df[[newVarName]] <<- ntile(df,newVarNameFloat,isolate(input$quantCats[1]))
     
     # add a brief description
     indexHash[[newVarNameFloat]] <<-indexHash[[newVarName]] <<- paste("Created from",paste((isolate(input$modelVars)),collapse = ","))
     
     # signal update to the variable list
     isolate(rvals$newVarExists <- rvals$newVarExists + 1)
  })

  #OUTPUT---------------------- Model Output-------------------------------------------------------
  output$logitResults <- renderPrint({
    
    if(denull(input$regressionButton[1],0)==0) return()  # dependency on the incrementing of the regression button clicks
    
    if (is.null(isolate(input$modelVars))) return("Nothing to report.")
    isolate(choices <- input$modelVars)
    isolate(orders <- input$modelOrder)
    
    AUC <- NULL
    
    #if(length(choices) < 1) return("Choose at least one predictor using the checkboxes above")
    
    # do permutations if the checkboxes are set to reorder
    for(loopVar in intersect(choices,orders)) { # find the ones that need ordering
      t <- match(df[[loopVar]], varIncludeOrder[[loopVar]])
      df[[loopVar]] <- t
    }
    
    # filter if needed: the filter should be already in df$filter__
      if (denull(isolate(input$filterIgnore),0) == 1 ) { # temporarily turn off the filter
        df$filter__ <- TRUE
      }
    
    # should we complement the data based on the filter? df$filter__ is all TRUE unless filter
    if (denull(isolate(input$filterComp),0)==1){
        df$filter__ <- 1 - df$filter__
    }
    
    # now prepare the data
    # select the columns 
    tdf <- df[,c("predicted","filter__",choices)]
    
    # eliminate rows with NAs
    tdf <- tdf[complete.cases(tdf),]
    
    # save the number of rows we have, so when we complement...
    cases <- nrow(tdf)
    
    # filter rows if we need to (the filter will be all TRUE if we don't)
    tdf <- tdf[tdf$filter__==TRUE,]
    
    # how many cases were not used?
    compCases <- cases - nrow(tdf)

    f <- as.formula(paste0("predicted ~ ",paste(choices,collapse =" + ")))
    global$logit <<- glm(f,data = tdf, family="binomial",na.action=na.omit)
    
    # Hold-out cross validation to calculate average AUC
    runs <- 20 # how many iterations?
    for(i in 1:runs){
      #generate a random splitter
      randomBoolean <- floor(runif(nrow(tdf),0,1.99999999))
      
      #randomly split the cases
      traindata <- subset(tdf,randomBoolean==0)
      testdata  <- subset(tdf,randomBoolean==1)
            
      # create a model from the first half
      m1 <- glm(f,traindata, family=binomial,na.action=na.omit)
      fitted1 <- predict.glm(m1,newdata=testdata,type = "response")
            
      m2 <- glm(f,testdata, family=binomial,na.action=na.omit)
      fitted2 <- predict.glm(m2,newdata=traindata,type = "response")
            
      # run on one half and collect stats
      #c1 <- ROCvector(m2$y,fitted1,15)
      c1 <- ROCvector(testdata$predicted,fitted1,25)
      AUC <- c(AUC,c1$AUC)

      # now run on the other half
      #c1 <- ROCvector(m1$y,fitted2,15)
      c1 <- ROCvector(traindata$predicted,fitted2,25)
      AUC <- c(AUC,c1$AUC)
    }
    
    # If there is a filter, cross-validate across it and save the predictions
    if(compCases > 29) { # don't bother unless there are 30+ samples
      # select the columns 
      tdf <- df[,c("predicted","filter__",choices)]
      
      # eliminate rows with NAs
      tdf <- tdf[complete.cases(tdf),]
      
      # reverse the filter
      tdf <- tdf[tdf$filter__==FALSE,]
            
      fitted1 <- predict.glm(global$logit,newdata=tdf,type = "response")
      
      #save fitted values in case someone wants to download them as a predictor
      global$fitted <<- data.frame(predicted=tdf$predicted,fitted = fitted1)
      
      c1 <- ROCvector(tdf$predicted,fitted1,15)
      filterAUC <- c1$AUC
      filterText <- paste("Validation across filter (shown in green) AUC = ", round(filterAUC,2),"\n\n")
    } else {
      filterText <- ""
      global$fitted <<- NULL
    }
        
    # this complicated stuff is necessary to put a summary on screen cleanly
    out <- paste(filterText, "Cross-validation (50% holdout) AUC statistics with ",runs*2, "runs:\n ",
        paste0(capture.output(summary(AUC)),collapse="\n"),
        "\n\n-----------------------------MODEL-------------------------------\n"
        ,paste0(capture.output(summary.glm(global$logit)),collapse="\n"))
   
    # update reactive var, so ROC will be generated now
    isolate(rvals$logitModel <<- rvals$logitModel + 1)
    
    cat(out)
  
  })

  #REACTIVE---------------------Make calculations for outputs -------------------------------------
  performanceDisplayData <- reactive({
     rvals$logitModel #dependency updates when new model is created
     
     fitted <- predict.glm(global$logit,isolate(input$predicted),type = "response")
     fitted <- fitted[complete.cases(fitted)] # remove NAs
     ROCvector(global$logit$y,fitted,25) # this function is in helpers.R
  })

  #OUTPUT----------------------ROC Curve and AUC---------------------------------------------------
  output$logitROC <- renderPlot({
  #  if (is.null(global$logit)) return("") # nothing to do yet
    rvals$logitModel # dependency forces updates
    sliderVal <- denull(input$logitCutoffSlider,1)
    
    curve <- performanceDisplayData()
    plot(curve$FPR,curve$TPR,type="b",xlim=c(0,1),ylim=c(0,1),col="black",main="ROC Curve")
    points(c(0,1),c(0,1),type="b",col="red")
    text(.5,.15,paste("AUC=",round(curve$AUC,2),sep=" "),col="black")
    text(curve$FPR[sliderVal],curve$TPR[sliderVal],labels="O",col="red",cex=2)
    
    # if there is a cross-filter validation, plot that too, in green
    if (!is.null(global$fitted)){
      curve <- ROCvector(global$fitted$predicted,global$fitted$fitted,25) # this function is in helpers.R
      points(curve$FPR,curve$TPR,type="b",col="green")
    }
    
  })

  #INPUT------------------------------slider to set cut-off for logit predictor ------------------------
  output$logitCutoffSlider <- renderUI({
    if (is.null(input$modelVars)) return("")
    #if (is.null(global$logit)) return("") # nothing to do yet
    sliderInput("logitCutoffSlider", label = strong("Predictor Performance"),min = 1, max = 26, value = 1, step = 1,ticks=TRUE)
  })

  #OUTPUT----------------------table of performance-----------------------------------------------
  output$logitTable <- renderTable({
    rvals$logitModel # dependency forces updates
    if (is.null(input$modelVars)) return(data.frame(vars=NULL))
  #  if (is.null(global$logit)) return(data.frame(model=NULL)) # nothing to do yet
    sliderVal <- denull(input$logitCutoffSlider,1)
    
    # get the modeled data from the reactive function
    curve <- performanceDisplayData()
  
    # given the value of the slider, find the right values
    TP <- curve$TP[sliderVal]
    FN <- curve$FN[sliderVal]
    FP <- curve$FP[sliderVal]
    TN <- curve$TN[sliderVal]
    
    # look at the end of the vector to find the total
    True <- TP + FN
    False <- FP + TN
    Total <- True + False
    
    perf <- data.frame(Predicted = c("In","Out","Total","Fraction"), 
            InClass = c(TP,FN,TP+FN,round(True/Total,2)), 
            OutClass=c(FP,TN,FP+TN,round(False/Total,2)),
            Accuracy=c(round(TP/(TP+FP),2),round(TN/(TN+FN),2),round((TP+TN)/(TP+FP+FN+TN),2),NA))
    return(perf)
    
  })

  #OUTPUT--------------------------- Print the cut-off value of the slider ------------------------
  output$logitCutoffValue <- renderUI({
  if (is.null(input$modelVars)) return("")
   val <- round((input$logitCutoffSlider-1)/25,3)
  paste("Fitted values <=", val)
  })
  #------------------------------------------------------------------------------------------------#
  ################################## End of Regression Tab #########################################


  #OBSERVE---------------------------------- debugging function -----------------------------------------
  observe(label="console",{
    if(denull(input$console) != 0) {
      options(browserNLdisabled=TRUE)
      saved_console<-".RDuetConsole"
      if (file.exists(saved_console)) load(saved_console)
      isolate(browser())
      save(file=saved_console,list=ls(environment()))
  }
})

})


