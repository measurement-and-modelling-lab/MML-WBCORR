require(htmlTable) || install.packages(htmlTable)
require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

shinyServer(function(input, output, session) {
    options(shiny.maxRequestSize = 30 * 1024^2 )
    
    output$estimationmethodInput <- renderUI({
        html_ui <- " "
        if (input$datatype == "rawdata") {
            html_ui <- paste0(radioButtons("estimationmethod", "Estimation method:", c("GLS" = "GLS", "TSGLS" = "TSGLS", "ADF" = "ADF", "TSADF" = "TSADF")))
        } else {
            html_ui <- paste0(radioButtons("estimationmethod", "Estimation method:", c("GLS" ="GLS", "TSGLS" = "TSGLS")))
        }
        HTML(html_ui)
    })

    ## Produce one data/file input for each group
    output$datafileInput <- renderUI({ 
        if (input$samples %in% 1:8) {
            html_ui <- " "
            if (input$datatype == 'correlation') {
                for (i in 1:input$samples){
                    html_ui <- paste0(html_ui,
                                      div(style="display: inline-block;vertical-align:top; height: 70px;",
                                          numericInput(paste0("samplesize",i), paste0("N for Data File #",i,":"), "", min = 1, max = NA, step = 1)),
                                      div(style="height: 65px;", fileInput(paste0("datafile",i), label=paste0("Data file #",i,":"))), '<hr>')
                }
            } else {
                for (i in 1:input$samples) {
                    html_ui <- paste0(html_ui, div(style="height: 65px;", fileInput(paste0("datafile",i), label=paste0("Data file #",i,":"))))
                    if (i < input$samples) {
                        html_ui <- paste0(html_ui, '<br>')
                    }
                }
            }
            HTML(html_ui)
        }
    })
    
    wbsctOutput <- eventReactive(input$runButton, {

        ## Read functions
        ComputeWBCorrChiSquare <- dget("./wbcorr/ComputeWBCorrChiSquare.R")
        tablegen <- dget("./wbcorr/tablegen.R")


        ## Stipulate data type
        datatype <- input$datatype


        ## Stipulate estimation method
        estimation.method <- input$estimationmethod


        ## Import data files
        data <- list()
        data.length <- input$samples
        for (i in 1:data.length) {

            validate(need(eval(parse(text = paste0('input$datafile',i))), "")) ## Check that data file i exists

            ## Check that R can read the data file as a .csv
            tryCatch({
                read.csv(file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), head=FALSE)
            }, warning = function(w) {
                stop("There was a problem reading one of your .csv files.")
            }, error = function(e) {
                stop("There was a problem reading one of your .csv files.")
            })

            data[[i]] <- as.matrix(read.csv(file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), head=FALSE))

            if (ncol(data[[i]]) > 16) {
                stop("The web version of MML-WBCORR does not support more than 16 variables.")
            }

        }


        ## Check that hypothesis file is readable as a .csv
        validate(need(input$hypothesisfile, ""))
        tryCatch({
            read.csv(file=input$hypothesisfile[[4]], head=FALSE, sep=",")
        }, warning = function(w) {
            stop("There was a problem reading one of your hypothesis file.")
        }, error = function(e) {
            stop("There was a problem reading one of your hypothesis file.")
        })


        ## Read the hypothesis file
        hypothesis <- as.matrix(read.csv(file=input$hypothesisfile[[4]], head=FALSE, sep=","))


        ## Renumber parameter tags if a number is skipped
        parameter.tags <- hypothesis[hypothesis[,4] != 0, 4]
        if (max(parameter.tags) > length(unique(parameter.tags))) {
            hypothesis[hypothesis[,4] != 0, 4] <- as.numeric(as.factor(parameter.tags))
        }


        ## Import N (calculate if raw data) for each group
        NList <- c()
        if (datatype == "correlation") {
            for (i in 1:data.length) {
                validate(need(eval(parse(text = paste0('input$samplesize',i))), ""))
                NList[[i]] <- eval(parse(text = paste0('input$samplesize',i)))
            }
        } else {
            for (i in 1:data.length) {
                NList[[i]] <- nrow(data[[i]])
            }
        }


        ## Define deletion method
        if (datatype == "rawdata") {
            if (estimation.method %in% c("ADF","TSADF")) {
                deletion <- input$adfdeletion
            } else if (estimation.method %in% c("GLS","TSGLS")) {
                deletion <- input$glsdeletion
            }
        } else {
            deletion <- "nodeletion"
        }


        ## Run the test
        output <- ComputeWBCorrChiSquare(data, NList, hypothesis, datatype, estimation.method, deletion)

        final.output <- ""


        ## Import amended sample sizes
        NList <- output[[6]]

        ## Assemble correlation matrices
        RList <- output[[1]]
        for (i in 1:data.length) {
            rownames(RList[[i]]) <- colnames(RList[[i]]) <- lapply(1:nrow(RList[[i]]), function(i) "")
            RList[[i]] <- round(RList[[i]], 3)
            header <- paste0("Input Correlation Matrix #", i, " (N=", NList[[i]], ")")
            final.output <- paste0(final.output, htmlTable(RList[[i]], align="r", caption=header))
        }


        ## Assemble correlation matrices
        RWLSList <- output[[2]]
        for (i in 1:data.length) {
            rownames(RWLSList[[i]]) <- colnames(RWLSList[[i]]) <- lapply(1:nrow(RWLSList[[i]]), function(i) "")
            RWLSList[[i]] <- round(RWLSList[[i]], 3)
            header <- paste0("OLS Matrix #", i, " (N=", NList[[i]], ")")
            final.output <- paste0(final.output, htmlTable(RWLSList[[i]], align="r", caption=header))
        }


        ## Print the parameter estimates
        gammahatDisplay <- output[[3]]
        if (!identical(NA, gammahatDisplay)) {
            header <- paste0(estimation.method, " Parameter Estimates")
            final.output <- paste0(final.output, htmlTable(gammahatDisplay, align="c", caption=header))
        }


        ## Return significance test results
        sigtable <- output[[4]]
        header <- "Significance Test Results"
        final.output <- paste0(final.output, htmlTable(sigtable, align="c", caption=header))


        ## Print MVN test
        if (datatype == "rawdata") {
            MardiaSK <- output[[5]]
            if (deletion == "pairwise") {
                final.output <- paste0(final.output, htmlTable(MardiaSK[[1]], align="c", caption="Assessment of the Distribution of the Observed Marginals"))
                final.output <- paste0(final.output, htmlTable(MardiaSK[[2]], align="c", caption="Assessment of Multivariate Normality"))
            } else {
                cat("\nAssessment of Multivariate Normality\n")
                final.output <- paste0(final.output, htmlTable(MardiaSK[[1]], align="c", caption="Assessment of Multivariate Skewness"))
                final.output <- paste0(final.output, htmlTable(MardiaSK[[1]], align="c", caption="Assessment of Multivariate Kurtosis"))
            }
        }

        HTML(final.output)

    })

    observeEvent(input$runButton, {
        updateTabsetPanel(session, "inTabset", 'out')
    })

    output$finaloutput <- renderUI({ 
        wbsctOutput()
    })

    

})
