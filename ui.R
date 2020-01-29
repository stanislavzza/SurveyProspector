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

library(shiny)
library(binom)
library(dplyr)
library(tidyr)
library(igraph)
library(ggvis)

shinyUI( #fluidPage(
  pageWithSidebar(
  #theme = "bootstrap.css",
  headerPanel(""),
  sidebarPanel(
    h3("Survey Prospector"),
    
    actionButton("console","server console"), # uncomment this to enable debugging <------------
    
    ########### Loading Data ##################################
    
    helpText(a("[See User Guide]",href="http://highered.blogspot.com/2014/07/survey-prospector.html")),
    helpText("Send feedback to deubanks.office@gmail.com"),
    htmlOutput("indexFileUploader"),  # upload optional index files
    htmlOutput("dataFileUploader"),   # upload data files
    htmlOutput("mergeType"), # radio buttons for merge type
    htmlOutput("numCatSlider"),       # slider to determine quantization
    htmlOutput("NAtolerance"),        # slider to set % NA acceptable
    
    downloadLink('downloadData', 'Download Data (CSV)'), # download link for the data frame
    htmlOutput("dataDoneButton"),     # button to signify finished loading
      
    ########### Analysis of Data ##############################
        
    htmlOutput("targetVar"),          # select the variable to predict
    htmlOutput("targetRangeIn"),    # select the inclass
    htmlOutput("targetRangeOut"),        # select the outclass range of interest of the target variable 
  
    htmlOutput("predictorSamples"),
    htmlOutput("forceOrderCB"),
    htmlOutput("action"),
    #h4("Real-Time Controls"),
    htmlOutput("predictor"),  #predictor
    htmlOutput("interestCB"),
    htmlOutput("bestOrderCB"), #checkbox to use best order
    htmlOutput("InvertFilterCB"),htmlOutput("IgnoreFilterCB")
  ), #end of sidebar panel
  mainPanel(
    # turn off the messages. They still appear in the console. 
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
          tabsetPanel(id="Main",
            
            tabPanel("Data",
                tableOutput("dataPanelHeader"),
                DT::dataTableOutput("varStats")
            ), # end of Data Tabpanel
            
            tabPanel("Filter",  # select optional filter variables
              fluidRow(
                  column(6,
                     htmlOutput("filterN"),
                     htmlOutput("filter1Select"),       # first filter
                     htmlOutput("filter1RangeSelect"),
                     htmlOutput("filter1Cases"),
                     textOutput("filter1Desc")
                  ),  # select range for filter
                  column(6,
                     plotOutput("filter1Plot")
                  )
              ),
              fluidRow(
                column(6,
                       htmlOutput("filterLogic"),
                       htmlOutput("filter2Select"),      # second filter
                       htmlOutput("filter2RangeSelect"),
                       htmlOutput("filter2Cases"),
                       textOutput("filter2Desc")
                ),  # select range for filter
                column(6,
                       plotOutput("filter2Plot")
                )
              )
            ), 
            
            tabPanel("Predictor",
               fluidRow(
                 column(12,htmlOutput('indexInfo'))
               ),
               fluidRow(
                 column(6, plotOutput('graph')),
                 column(6,plotOutput('ROC'))
               ),
               fluidRow(
                 column(6, plotOutput('freq')),
                 column(6, htmlOutput("tableTitle"),htmlOutput("slider1"), tableOutput("performance"),htmlOutput("predictSet"))
               ),
               fluidRow(
                 column(6, textOutput('fishersExactTest')),
                 column(6, textOutput('wilcox'))
              )
            ), # end of predictor analysis panel
            tabPanel("Network",
                     fluidRow(
                       column(4,htmlOutput('numberPredictors')),
                       column(4,htmlOutput('networkButton')),
                       column(4,htmlOutput('numberSelector'))
                     ),
                     fluidRow(
                       column(4,htmlOutput('maxAUC')),
                       column(4,htmlOutput('forceLayout')),
                       column(4,htmlOutput('maxCor'))
                     ),
                     fluidRow(
                       column(12,ggvisOutput('ggvis_networkResults'))
                     )
            ), #end of network tab
            tabPanel("Co-predictors",
              htmlOutput('indexInfoCo'),       
              DT::dataTableOutput('copredictors')
            ), #end of copredictor tab
            tabPanel("Regression",
                 # fluidRow(
                  #   column(12,htmlOutput('indexInfoEnsemble'))
                  #),
                  fluidRow(
                    column(12,h3("Logistic Regression"))
                  ),
                  fluidRow(
                     column(7,htmlOutput('variableSelection')),
                     column(3,htmlOutput('orderSelection')),
                     column(2,htmlOutput('removeSelection'))
                  ),
                  fluidRow(
                     column(3,htmlOutput('regressionButton'))
                    
                  ),fluidRow(column(12,hr())),
                  fluidRow(
                    column(6,plotOutput("logitROC")),
                    column(6,htmlOutput("logitCutoffSlider"),tableOutput("logitTable"),htmlOutput("logitCutoffValue"))                    
                  ),
                  fluidRow(
                     column(12,verbatimTextOutput('logitResults'))
                  ),
                 fluidRow(
                   column(12,"Create a new variable from the predictor:",htmlOutput('newVarName'),htmlOutput('newVarButton'))
                 )
            )
          ) # end of tabsetPanel

  ) # end of main panel
) # end of page with sidebar
) # end of ShinyUI
