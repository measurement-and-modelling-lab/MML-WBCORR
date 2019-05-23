require(htmlTable) || install.packages(htmlTable)
require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",
                  
                  HTML('<br>
  
    <link rel="stylesheet" type="text/css" href="index.css">
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<script>
function myFunction() {
  var x = document.getElementById("myTopnav");
  if (x.className === "topnav") {
    x.className += " responsive";
    document.getElementById("row-break").style.display = "inline";
    document.getElementById("row-break2").style.display = "inline";
  } else {
    x.className = "topnav";
    document.getElementById("row-break").style.display = "none";
    document.getElementById("row-break2").style.display = "none";
  }
}
</script>
  <style>
    html {
       overflow-y: scroll;
       }
       </style>
    <title>Analytics^2 - About</title>
    <div class="bar">
    <div class="topnav" id="myTopnav"">
      <a href="javascript:void(0);" class="icon" onclick="myFunction()">
        <i class="fa fa-bars"></i>
      </a>
      <b class="title">Measurement and Modelling Lab &nbsp; - &nbsp; Tools</b><br class="rwd-break" id="row-break"><br class="rwd-break" id="row-break2">
      <a href="https://shiny.rcg.sfu.ca/u/tmustaph/MML-R2/"><font color="#00ca8a">MML-R2</font></a>
      <a href="https://shiny.rcg.sfu.ca/u/tmustaph/MML-Multicorr/"><font color="white">MML-Multicorr</font></a>
      <a href="https://shiny.rcg.sfu.ca/u/tmustaph/MML-WBCORR/"><font color="white">MML-WBCORR</font></a>
      <a href="https://shiny.rcg.sfu.ca/u/tmustaph/csv-generator/"><font color="white">CSV Generator</font></a>
      <a href="https://shiny.rcg.sfu.ca/u/tmustaph/distribution-tests/"><font color="white">Distribution Tests</font></a>

    </div>
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
                    body { min-width: 450px; }
                    sub { vertical-align: 25%; font-size: 70%; }
                    th {
                      white-space: nowrap;
                      width: 1px;
                      padding-left: 8px;
                      padding-right: 8px;
                      padding-top: 0px;
                      padding-bottom: 0px;
                      color: #717171;
                    }
                    input[type=number]::-webkit-outer-spin-button,
                    input[type=number]::-webkit-inner-spin-button {
                        -webkit-appearance: none;
                        margin: 0;
                    }
                    input[type=number] {
                        -moz-appearance:textfield;
                    }
                    .shiny-file-input-progress {
                      margin-top: -28px !important;
                      margin-left: 85px !important;
                      z-index:1000;
                      width: -webkit-calc(100% - 94px);
                      width:    -moz-calc(100% - 94px);
                      width:         calc(100% - 94px);
                      opacity: 1;
                      position:relative;
                    }
                    .noheader {
                      padding-left: 8px;
                      padding-right: 8px;
                      }
                      "))
                  ),
                  
                  headerPanel('', windowTitle = 'MML-WBCORR'),
                  
                  sidebarLayout(
                    sidebarPanel(
                      radioButtons("datatype", "Data type:", c("Correlation data" = "correlation", "Raw data" = "rawdata")),
                      uiOutput("estimationmethodInput"),
                      conditionalPanel(condition = "input.datatype == 'rawdata' && input.estimationmethod != 'TSADF' && input.estimationmethod != 'ADF'", radioButtons("glsdeletion", label = "Missing values:", choices = c("There are none" = "nodeletion", "Listwise deletion" = 'listwise', "Pairwise deletion" = 'pairwise'))),
                      conditionalPanel(condition = "input.datatype == 'rawdata' && input.estimationmethod != 'TSGLS' && input.estimationmethod != 'GLS'", radioButtons("adfdeletion", label = "Missing values:", choices = c("There are none" = "nodeletion", "Listwise deletion" = 'listwise'))),
                      numericInput("samples", "Number of groups:", 1, min = 1, max = 10),
                      fileInput("hypothesisfile", "Hypothesis file:"),
                      HTML('<hr style="height:1px; visibility:hidden;" />'),
                      HTML("<hr>"),
                      uiOutput("datafileInput"),
                      conditionalPanel(condition = "input.datatype == 'rawdata'", HTML("<hr>")),
                      actionButton("runButton", "Run")
                    ),
                    
                    mainPanel(
                      tabsetPanel(
                        id = "inTabset",
                        tabPanel(value = "out", "Output", uiOutput("finaloutput")),
                        tabPanel(value = "readme1", "Formatting input", includeHTML("./documentation/input.html")),
                        tabPanel(value = "readme2", "Choosing a method", includeHTML("./documentation/method.html")),
                        tabPanel(value = "readme3", "Interpreting output", includeHTML("./documentation/output.html")),
                        tabPanel(value = "about", "About", includeHTML("./documentation/about.html"))
                      )
                    )
                  ),
                  HTML('<br>'),
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
