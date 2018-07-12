library(shiny)
library(shinythemes)
library(rhandsontable)

shinyUI(fluidPage(theme = "simplex.css",

  HTML('<br>
  
    <link rel="stylesheet" type="text/css" href="index.css">
  <style>
    html {
       overflow-y: scroll;
       }
       </style>
    <title>Analytics^2 - About</title>
         <div class="bar">
    <b class="title">Measurement and Modelling Lab &nbsp; - &nbsp; Tools</b><br class="rwd-break"><b class="link">
    <a href="https://shiny.rcg.sfu.ca/u/pserafin/rsquared/"><font color="#00ca8a">R SQUARED</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/wbcorr/"><font color="white">WBCORR</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/csvgenerator/"><font color="white">CSVGenerator</font></a>





        </b>
        </div>
         
         
         
         '),
  
  HTML("<br>"),
  
  tags$head(
    tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Patua+One');
                      h1 {
                      font-family: 'Patua One';
                      font-weight: bold;
                      line-height: 1.1;
                      color: #333;
                      }
                      sub { vertical-align: 25%; font-size: 70%; }
                    "))
    ),
  headerPanel('MML CSV Generator', windowTitle = 'MML - CSV Generator'),
  
    sidebarLayout(
    sidebarPanel(
      sliderInput("groups", "Number of groups:",
                  min = 1, max = 8, value = 8
      ),
      sliderInput("variables", "Number of variables:",
                  min = 2, max = 16, value = 2
      ),
      conditionalPanel(condition = "input.tabs == 'Correlation Matrices'", helpText("Note: Right click on a correlation matrix to download it as a .csv file. Unfortunately this feature does not work in all browsers, but it is confirmed to work in Firefox and Chrome.")),
      conditionalPanel(condition = "input.tabs == 'Hypothesis Matrix'", helpText("Note: Cells containing the same positive integer are hypothesised to be equal,
                                                                                  and cells containing a value between -1 and 1 are hypothesised to be equal to that value.
                                                                                  Right click on the hypothesis matrix to download it as a .csv file. Unfortunately this feature does not work in all browsers, but it is confirmed to work in Firefox and Chrome."))
    ),

    mainPanel(
      
      
      
      
      

      
      
      
      
      
      tabsetPanel(id = "tabs",
        tabPanel("Correlation Matrices",
                 fluidRow(
                   
                   column(6,
                            htmlOutput("br1"),
                            rHandsontableOutput("table1"),
                            conditionalPanel(condition = "input.groups > 1",htmlOutput("br3")),
                            conditionalPanel(condition = "input.groups > 1",rHandsontableOutput("table2")),
                            conditionalPanel(condition = "input.groups > 2",htmlOutput("br5")),
                            conditionalPanel(condition = "input.groups > 2",rHandsontableOutput("table3")),
                            conditionalPanel(condition = "input.groups > 3",htmlOutput("br7")),
                            conditionalPanel(condition = "input.groups > 3",rHandsontableOutput("table4"))
                   ),
                   
                   column(6,
                          conditionalPanel(condition = "input.groups > 4",htmlOutput("br9")),
                          conditionalPanel(condition = "input.groups > 4",rHandsontableOutput("table5")),
                          conditionalPanel(condition = "input.groups > 5",htmlOutput("br11")),
                          conditionalPanel(condition = "input.groups > 5",rHandsontableOutput("table6")),
                          conditionalPanel(condition = "input.groups > 6",htmlOutput("br13")),
                          conditionalPanel(condition = "input.groups > 6",rHandsontableOutput("table7")),
                          conditionalPanel(condition = "input.groups > 7",htmlOutput("br15")),
                          conditionalPanel(condition = "input.groups > 7",rHandsontableOutput("table8"))
                   )
                 )

        ),
        tabPanel("Hypothesis Matrix",
                 fluidRow(
                   
                   column(6,
                          htmlOutput("br2"),
                          rHandsontableOutput("table9"),
                          conditionalPanel(condition = "input.groups > 1",htmlOutput("br4")),
                          conditionalPanel(condition = "input.groups > 1",rHandsontableOutput("table10")),
                          conditionalPanel(condition = "input.groups > 2",htmlOutput("br6")),
                          conditionalPanel(condition = "input.groups > 2",rHandsontableOutput("table11")),
                          conditionalPanel(condition = "input.groups > 3",htmlOutput("br8")),
                          conditionalPanel(condition = "input.groups > 3",rHandsontableOutput("table12")),
                          htmlOutput("br17"),
                          rHandsontableOutput("table17")
                   ),
                   
                   column(6,
                          conditionalPanel(condition = "input.groups > 4",htmlOutput("br10")),
                          conditionalPanel(condition = "input.groups > 4",rHandsontableOutput("table13")),
                          conditionalPanel(condition = "input.groups > 5",htmlOutput("br12")),
                          conditionalPanel(condition = "input.groups > 5",rHandsontableOutput("table14")),
                          conditionalPanel(condition = "input.groups > 6",htmlOutput("br14")),
                          conditionalPanel(condition = "input.groups > 6",rHandsontableOutput("table15")),
                          conditionalPanel(condition = "input.groups > 7",htmlOutput("br16")),
                          conditionalPanel(condition = "input.groups > 7",rHandsontableOutput("table16"))

                   )
                 )
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 


        )
      )
    )),
  HTML('<br><br><br><br><br><br><br><br><br><br>'),
  HTML('
    <link rel="stylesheet" type="text/css" href="index.css">
    <div class="bar2">
     <b class="bottom">
     <font color="#717171">Provided by the</font>
     <a href="http://members.psyc.sfu.ca/labs/mml"><font color=white>Measurement and Modelling Lab</font></a>
     <font color="#717171"> at</font>
     <a href="https://www.sfu.ca/"><font color=white> SFU</font></a>
     </b>
     </div><br>')
)
)
