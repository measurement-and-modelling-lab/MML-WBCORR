shinyServer(function(input, output, session) {

 
    ## Correlation matrix generator
    output$corrtable1 <- renderRHandsontable({

        ## Create a matrix with the correct number of rows/columns
        variables <- as.numeric(input$variables)
        MAT <- matrix('', nrow=variables, ncol=variables)

        ## Add row and column names (X_1 ... X_p)
        rownames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")
        colnames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")

        ## Add ones along the main diagonal
        for (v in 1:variables) {
            MAT[v,v] <- 1
        }

        ## Add an option to download the matrix as a .csv to the context menu
        rhandsontable(MAT) %>%
            hot_context_menu(
                customOpts = list(
                    csv = list(name = "Download to CSV",
                            callback = htmlwidgets::JS(
                            "function (key, options) {
                                var csv = csvString(this, sep=',', dec='.');

                                // Remove the row and column names
                                while (csv[0] == 'X') {
                                csv = csv.substring(csv.indexOf('X')+14);
                                }

                                var link = document.createElement('a');
                                link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                                encodeURIComponent(csv));
                                link.setAttribute('download', 'data.csv');

                                document.body.appendChild(link);
                                link.click();
                                document.body.removeChild(link);
                            }"))))
    })
  
  
    ## Hypothesis matrix generator (input)
    output$corrtable2 <- renderRHandsontable({

        ## Create a matrix with the correct number of variables
        variables <- as.numeric(input$variables)
        MAT <- matrix('', nrow=variables, ncol=variables)

        ## Add row and column names (X_1 ... X_p)
        rownames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")
        colnames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")

        ## Add colors for positive integers
        rhandsontable(MAT) %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                if (row == col) {
                td.style.background = 'lightgrey';
                cellProperties.readOnly = true;
                } else if (value == 1) {
                td.style.background = '#8bfa9d';
                } else if (value == 2) {
                td.style.background = '#f1fa8c';
                } else if (value == 3) {
                td.style.background = '#ff9ac6';
                } else if (value == 4) {
                td.style.background = '#caa9fa';
                } else if (value == 5) {
                td.style.background = '#8be9fd';
                } else if (value == 6) {
                td.style.background = '#ff6f55';
                } else if (value == 7) {
                td.style.background = '#26a1d2';
                } else if (value == 8) {
                td.style.background = '#2abe98';
                }
                }",format='0.[000]') # Hide decimals (i.e. 1 instead of 1.000)
    })



    ## Hypothesis matrix generator (output)
    output$hypothesistable <- renderRHandsontable({

        ## Ensure that the correlation matrix exists; if so assign it a variable name
        validate(need(input$corrtable2, ""))
        h1 <- hot_to_r(input$corrtable2)

        ## Initialize a matrix with an empty and column names
        h_matrix <- matrix(c('','','','',''),nrow=1,ncol=5)
        colnames(h_matrix) <- c('group','row','column','parameter tag','fixed value')

        ## Loop through every value in the correlation matrix
        for (r in 1:nrow(h1)) {
            for (c in 1:ncol(h1)) {

                ## Assign values below the diagonal to group 1
                ## For every decimal number, create a fixed value hypothesis
                ## For every positive integer, create a parameter tag hypothesis
                if (r > c) {
                    val <- h1[r,c] 
                    if (!(is.na(as.numeric(val)))) {
                        val <- as.numeric(val)
                        if(abs(val) < 1){
                            new_row <- c(1, r, c, 0 ,h1[r,c])
                            h_matrix <- rbind(h_matrix, new_row)
                        }else if(!(grepl("\\D", val))){
                            new_row <- c(1, r, c, val ,0)
                            h_matrix <- rbind(h_matrix, new_row)
                        }
                    }
                }

                ## Assign values above the diagonal to group 2
                ## For every decimal number, create a fixed value hypothesis
                ## For every positive integer, create a parameter tag hypothesis
                if (r < c) {
                    val <- h1[r,c] 
                    if (!(is.na(as.numeric(val)))) {
                        val <- as.numeric(val)
                        if(abs(val) < 1){
                            new_row <- c(2, r, c, 0 ,h1[r,c])
                            h_matrix <- rbind(h_matrix, new_row)
                        }else if(!(grepl("\\D", val))){
                            new_row <- c(2, c, r, val ,0)
                            h_matrix <- rbind(h_matrix, new_row)
                        }
                    }
                }
            }
        }

        ## If the hypothesis matrix has more than one row, delete the empty row
        if (h_matrix[1,] == c('','','','','') && nrow(h_matrix) > 1) {
            h_matrix <- h_matrix[-1, , drop=FALSE]
        }

        ## Delete the row names
        rownames(h_matrix) <- NULL

        ## Make the hypothesis matrix downloadable via context menu
        rhandsontable(h_matrix) %>%
            hot_context_menu(
                customOpts = list(
                    csv = list(name = "Download to CSV",
                            callback = htmlwidgets::JS(
                            "function (key, options) {
                                var csv = csvString(this, sep=',', dec='.');
                                var csv = csv.substring(43); // Remove the header row

                                var link = document.createElement('a');
                                link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                                encodeURIComponent(csv));
                                link.setAttribute('download', 'hypothesis.csv');

                                document.body.appendChild(link);
                                link.click();
                                document.body.removeChild(link);
                            }"))))

})








    })
