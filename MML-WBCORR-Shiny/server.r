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

	# Produce one data/file input for each group
    output$datafileInput <- renderUI({ 
        if (input$samples %in% 1:8) {
            html_ui <- " "
            if (input$datatype == 'correlation') {
                for (i in 1:input$samples){
                    html_ui <- paste0(html_ui, div(style="display: inline-block;vertical-align:top; height: 70px;", numericInput(paste0("samplesize",i), paste0("N for Data File #",i,":"), "", min = 1, max = NA, step = 1)), div(style="height: 65px;", fileInput(paste0("datafile",i), label=paste0("Data file #",i,":"))), '<hr>')
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
    
    output$wbsctOutput <- eventReactive(input$runButton, {

		# Import data files
        data <- list()
        for (i in 1:input$samples) {
            validate(need(eval(parse(text = paste0('input$datafile',i))), ""))

			# Check that R can read the data file as a .csv
			result = tryCatch({
			  read.csv(file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), head=FALSE, sep=",")
			}, warning = function(w) {
			  'problem'
			}, error = function(e) {
			  'problem'
			}, finally = {
			})
			if ('problem' %in% result) {
			  return(capture.output(cat('<br>Error: There was an problem reading data file #', i, '; it may not be a .csv file.', sep="")))
			} else { # If so import it as a matrix
				data[[i]] <- as.matrix(read.csv(file=eval(parse(text = paste0('input$datafile',i,'[[4]]'))), head=FALSE, sep=","))
			}

            if (ncol(data[[i]]) > 16) {
              return(capture.output(cat('<br>Error: WBCORR does not support more than 15 variables.', sep="")))
            }
        }

		# Import N (calculate if raw data) for each group
        NList <- c()
        if (input$datatype == 'correlation') {
            for (i in 1:input$samples) {
                validate(need(eval(parse(text = paste0('input$samplesize',i))), ""))
                NList[[i]] <- eval(parse(text = paste0('input$samplesize',i)))
            }
        } else {
            for (i in 1:input$samples) {
                NList[[i]] <- nrow(data[[i]])
            }
        }

		# Check that hypothesis file is readable as a .csv
        validate(need(input$hypothesisfile, ""))
        result = tryCatch({
          read.csv(file=input$hypothesisfile[[4]], head=FALSE, sep=",")
        }, warning = function(w) {
          'problem'
        }, error = function(e) {
          'problem'
        }, finally = {
        })
        if ('problem' %in% result) {
          return(capture.output(cat('<br>Error: There was an problem reading the hypothesis file; it may not be a .csv file.', sep="")))
        } else {
          hypothesis <- as.matrix(read.csv(file=input$hypothesisfile[[4]], head=FALSE, sep=","))
        }
        
		    # Should fix this
        if (is.vector(hypothesis) || nrow(hypothesis) == 1) {
          return(capture.output(cat('<br>Error: The hypothesis file must have more than one line.', sep="")))
        }
        
        if (input$datatype == 'rawdata') {
          if (input$estimationmethod %in% c('ADF','TSADF')) {
              deletion <- input$deletion2
          } else if (input$estimationmethod %in% c('GLS','TSGLS')) {
              deletion <- input$deletion1
          }
        } else {
          deletion <- 'no'
        }

        ComputeWBCorrChiSquare <- dget("ComputeWBCorrChiSquare.r")
        capture.output(ComputeWBCorrChiSquare(data, NList, hypothesis, input$datatype, input$estimationmethod,deletion))
    
    })

    observeEvent(input$runButton, {
      updateTabsetPanel(session, "inTabset", 'out')
    })
    

})
