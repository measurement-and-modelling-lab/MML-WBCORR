require(htmlTable) || install.packages(htmlTable)
require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

RoundPercentile <- dget("./wbcorr/RoundPercentile.R")

shinyServer(function(input, output, session) {
    options(shiny.maxRequestSize = 30 * 1024^2 )

    observe({
        options(shiny.sanitize.errors = FALSE)
    })

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

            ## Try to import the data file, return an error if it's not readable as a .csv
            cat("\n", file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), append = TRUE) ## append the necessary line break to the end
            tryCatch({
                read.csv(file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), head=FALSE)
                data[[i]] <- as.matrix(read.csv(file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), head=FALSE))
            }, warning = function(w) {
                stop("There was a problem reading one of your .csv files. You may need to add a blank line to the end of the file.")
            }, error = function(e) {
                stop("There was a problem reading one of your .csv files. You may need to add a blank line to the end of the file.")
            })


            if (ncol(data[[i]]) > 16) {
                stop("The web version of MML-WBCORR does not support more than 16 variables.")
            }

        }


        ## Try to import the hypothesis file, return an error if it's not readable as a .csv
        validate(need(input$hypothesisfile, ""))
        cat("\n", file=input$hypothesisfile[[4]], append = TRUE) ## append the necessary line break to the end
        tryCatch({
            hypothesis <- as.matrix(read.csv(file=input$hypothesisfile[[4]], head=FALSE, sep=","))
        }, warning = function(w) {
            stop("There was a problem reading your hypothesis file. You might need to add a blank line to the end of the file.")
        }, error = function(e) {
            stop("There was a problem reading your hypothesis file. You might need to add a blank line to the end of the file.")
        })


        ## Import N (calculate if raw data) for each group
        NList <- list()
        if (datatype == "correlation") {
            for (i in 1:data.length) {
                validate(need(eval(parse(text = paste0('input$samplesize',i))), ""))
                NList[i] <- eval(parse(text = paste0('input$samplesize',i)))
            }
        } else {
            NList <- sapply(data, simplify=TRUE, function(i) {
                nrow(i)
            })
        }
        NList <- as.numeric(NList)


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


        html.output <- ""
        NList <- output[[6]] ## Import amended sample sizes


        ## Print the original hypothesis matrix
        hypothesis.colnames <- c("Group", "Row", "Column", "Parameter Tag", "Fixed Value")
        html.output <- paste0(html.output, htmlTable(hypothesis, align="c", caption="Input Hypothesis Matrix", header=hypothesis.colnames))


        ## Print the amended hypothesis matrix, if changes were made
        hypothesis.amended <- output[[7]]
        if (!all(hypothesis == hypothesis.amended)) {
            html.output <- paste0(html.output, htmlTable(hypothesis.amended, align="c", caption="Amended Hypothesis Matrix", header=hypothesis.colnames))
        }


        ## Print the correlation matrices
        RList <- output[[1]]
        for (i in 1:data.length) {
            rownames(RList[[i]]) <- colnames(RList[[i]]) <- lapply(1:nrow(RList[[i]]), function(i) "")
            RList[[i]] <- round(RList[[i]], 3)
            variables <- nrow(RList[[i]])
            labels <- paste0("<b>X<sub>", 1:variables, "</sub></b>")
            caption <- paste0("Input Correlation Matrix #", i, " (N=", NList[i], ")")
            html.output <- paste0(html.output, htmlTable(RList[[i]], align="r", caption=caption, rnames=labels, header=labels, align.header="r", css.cell = "padding-left: .5em; padding-right: .2em;"))
        }


        ## Print the OLS matrices
        RWLSList <- output[[2]]
        for (i in 1:data.length) {
            rownames(RWLSList[[i]]) <- colnames(RWLSList[[i]]) <- lapply(1:nrow(RWLSList[[i]]), function(i) "")
            RWLSList[[i]] <- round(RWLSList[[i]], 3)
            variables <- nrow(RList[[i]])
            labels <- paste0("<b>X<sub>", 1:variables, "</sub></b>")
            caption <- paste0("OLS Matrix #", i, " (N=", NList[i], ")")
            html.output <- paste0(html.output, htmlTable(RWLSList[[i]], align="r", caption=caption, rnames=labels, header=labels, align.header="r", css.cell = "padding-left: .5em; padding-right: .2em;"))
        }


        ## Print the parameter estimates
        gammahatDisplay <- output[[3]]
        if (is.matrix(gammahatDisplay)) {

            ## Order the estimates by parameter tag
            gammahatDisplay <- gammahatDisplay[order(gammahatDisplay[,1]), , drop=FALSE]
            gammahatDisplay <- round(gammahatDisplay, 3)

            if (!identical(NA, gammahatDisplay)) {
                header <- paste0(estimation.method, " Parameter Estimates")
                html.output <- paste0(html.output, htmlTable(gammahatDisplay,
                                                             align="c",
                                                             tfoot=paste0("* - ", 100 - 5/nrow(gammahatDisplay), "% confidence interval"),
                                                             caption=header))
            }
        }

        sigtable <- output[[4]]
        sigtable[,1] <- round(sigtable[,1], 3)
        sigtable[,3] <- RoundPercentile(sigtable[,3])

        header <- "Significance Test Results"
        html.output <- paste0(html.output, htmlTable(sigtable, align="c", caption=header))


        ## Print MVN test
        if (datatype == "rawdata") {
            MardiaSK <- output[[5]]
            if (deletion == "pairwise") {

                range.table <- MardiaSK[[1]]
                normality.table <- MardiaSK[[2]]

                if (data.length > 1) {

                    omnibus.chisq <- sum(normality.table[,2]^2)
                    omnibus.p <- 1 - pchisq(omnibus.chisq, data.length)
                    omnibus.table <- matrix(c(omnibus.chisq, data.length, omnibus.p), nrow=1)

                    colnames(omnibus.table) <- c("Chi Square", "df", "plevel")

                    omnibus.table[,-3] <- round(omnibus.table[,-3], 3)
                    omnibus.table[,3] <- RoundPercentile(omnibus.table[,3])

                    html.output <- paste0(html.output, htmlTable(omnibus.table, align="c", caption="Omnibus MVN Test"))

                }

                range.table[,4] <- round(range.table[,4], 3)
                range.table[,5] <- RoundPercentile(range.table[,5])

                normality.table[,2] <- round(normality.table[,2], 3)
                normality.table[,3] <- RoundPercentile(normality.table[,3])
                
                ## Format html table
                html.output <- paste0(html.output, htmlTable(range.table, align="c", caption="Assessment of the Distribution of the Observed Marginals"))
                html.output <- paste0(html.output, htmlTable(normality.table, align="c", caption="Assessment of Multivariate Normality"))

            } else {

                skew.table <- MardiaSK[[1]]
                kurt.table <- MardiaSK[[2]]

                if (data.length > 1) {

                    omnibus.MST <- sum(skew.table[,3])
                    omnibus.df <- sum(skew.table[,4])
                    omnibus.P1 <- 1 - pchisq(omnibus.MST, omnibus.df)
                    skew.omnibus <- c(omnibus.MST, omnibus.df, omnibus.P1)

                    omnibus.MKT <- sum(kurt.table[,3]^2)
                    omnibus.P2 <- 1 - pchisq(omnibus.MKT, data.length)
                    kurt.omnibus <- c(omnibus.MKT, data.length, omnibus.P2)

                    omnibus.table <- rbind(skew.omnibus, kurt.omnibus)
                    rownames(omnibus.table) <- c("Skewness", "Kurtosis")
                    colnames(omnibus.table) <- c("Chi Square", "df", "plevel")

                    omnibus.table[,-3] <- round(omnibus.table[,-3], 3)
                    omnibus.table[,3] <- RoundPercentile(omnibus.table[,3])

                    html.output <- paste0(html.output, htmlTable(omnibus.table, align="c", caption="Omnibus MVN Tests"))

                }

                skew.table[,-5] <- round(skew.table[,-5], 3)
                skew.table[,5] <- RoundPercentile(skew.table[,5])

                kurt.table[,-4] <- round(kurt.table[,-4], 3)
                kurt.table[,4] <- RoundPercentile(kurt.table[,4])

                ## Format html table
                html.output <- paste0(html.output, htmlTable(skew.table, align="c", caption="Assessment of Multivariate Skewness"))
                html.output <- paste0(html.output, htmlTable(kurt.table, align="c", caption="Assessment of Multivariate Kurtosis"))

            }
        }

        HTML(html.output)

    })

    observeEvent(input$runButton, {
        updateTabsetPanel(session, "inTabset", 'out')
    })

    output$finaloutput <- renderUI({
        wbsctOutput()
    })



})
