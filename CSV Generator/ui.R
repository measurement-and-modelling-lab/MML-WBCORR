require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)
require(rhandsontable) || install.packages(rhandsontable)

shinyUI(fluidPage(theme = "simplex.css",

                  HTML('<br>
                        <link rel="stylesheet" type="text/css" href="index.css">
                        <style>
                          html { overflow-y: scroll; }
                        </style>
                        <div class="bar">
                          <b class="title">Measurement and Modelling Lab &nbsp; - &nbsp; Tools</b><br class="rwd-break">
                          <b class="link">
                            <a href="https://shiny.rcg.sfu.ca/u/pserafin/rsquared/"><font color="white">MML-R2</font></a>
                            &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/wbcorr/"><font color="white">MML-WBCORR</font></a>
                            &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/pserafin/csvgenerator/"><font color="#00ca8a">CSV Generator</font></a>
                          </b>
                        </div>
                        <br>'
                       ),

  tags$head(
           tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Patua+One');
                          h1 {
                          font-family: 'Patua One';
                          font-weight: bold;
                          line-height: 1.1;
                          color: #333;
                          }
                          body { min-width: 450px; }
                          sub { vertical-align: 25%; font-size: 70%; }"
                           ))
       ),

  headerPanel('', windowTitle = 'CSV Generator'),
  
  sidebarLayout(
      sidebarPanel(
          selectInput("file",
                      label = "File to create:", 
                      choices = list("Correlation matrix" = "correlation", "Hypothesis matrix" = "hypothesis"), 
                      selected = "correlation"
          ),
          sliderInput("variables",
                      "Number of variables:",
                      min = 2,
                      max = 16,
                      value = 2
          ),
          conditionalPanel(condition = "input.tabs == 'Correlation Matrices'",
                           helpText("Note: Right click on a correlation matrix to download it as a .csv file. Unfortunately this feature does not work in all browsers,
                                     but it is confirmed to work in Firefox and Chrome.")
          ),
          conditionalPanel(condition = "input.tabs == 'Hypothesis Matrix'", helpText("Note: Cells containing the same positive integer are hypothesised to be equal,
                                                                                      and cells containing a value between -1 and 1 are hypothesised to be equal to that value.
                                                                                      Right click on the hypothesis matrix to download it as a .csv file.
                                                                                      Unfortunately this feature does not work in all browsers, but it is confirmed to work in Firefox and Chrome.")
          )
      ),

      mainPanel(
        conditionalPanel(condition = "input.file == 'correlation'", 
                         rHandsontableOutput("corrtable1")
        ),
        conditionalPanel(condition = "input.file == 'hypothesis'", 
          rHandsontableOutput("corrtable2")
        ),
        conditionalPanel(condition = "input.file == 'hypothesis'", 
          HTML("<br>")
        ),
        conditionalPanel(condition = "input.file == 'hypothesis'", 
                         rHandsontableOutput("hypothesistable")
        )
      )

  ),
  
  HTML('<br><br><br><br><br><br><br><br><br><br>'),
  HTML('<link rel="stylesheet" type="text/css" href="index.css">
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
