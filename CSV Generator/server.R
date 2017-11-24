shinyServer(function(input, output, session) {

  output$table1 <- output$table2 <- output$table3 <- output$table4 <- output$table5 <- output$table6 <- output$table7 <- output$table8 <- renderRHandsontable({
    
    variables <- as.numeric(input$variables)
    
    MAT <- matrix('', nrow=variables, ncol=variables)

    rownames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")
    colnames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")
    
    for (r in 1:variables) {
      for (c in 1:variables) {
        if (r == c) {
          MAT[r,c] <- 1
        }
      }
    }

    rhandsontable(MAT, overflow = 'visible', width = (variables+1)*50) %>%
      
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                         var csv = csvString(this);
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
                       }")))) %>%
      hot_cols(renderer = "
                     function (instance, td, row, col, prop, value, cellProperties) {
                     Handsontable.renderers.NumericRenderer.apply(this, arguments);
                     if (row == col) {
                     td.style.background = 'lightgrey';
                     cellProperties.readOnly = true;
                     } else if (col > row) {
                     td.style.background = 'grey';
                     cellProperties.readOnly = true;
                     }
                     }",format='0.0[00]')

  })
  
  output$table9 <- output$table10 <- output$table11 <- output$table12 <- output$table13 <- output$table14 <- output$table15 <- output$table16 <- renderRHandsontable({
    
    variables <- as.numeric(input$variables)
    
    MAT <- matrix('', nrow=variables, ncol=variables)
    
    rownames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")
    colnames(MAT) <- paste("X<sub>", 1:variables, "</sub>", sep="")
    
    rhandsontable(MAT, overflow = 'visible', width = (variables+1)*50) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
      hot_cols(renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.NumericRenderer.apply(this, arguments);
                 if (row == col) {
                 td.style.background = 'lightgrey';
                 cellProperties.readOnly = true;
                 } else if (col > row) {
                 td.style.background = 'grey';
                 cellProperties.readOnly = true;
                 td.style.color = 'grey';
                 }else if (value == 1) {
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
                 }",format='0')
  })
  
  output$table17 <- renderRHandsontable({
    
    validate(need(input$table9, ""))

    if (input$groups == 1) {
      h1 <- hot_to_r(input$table9)
      things <- list(h1)
    } else if (input$groups == 2) {
      validate(need(input$table10, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      things <- list(h1,h2)
    } else if (input$groups == 3) {
      validate(need(input$table10, ""))
      validate(need(input$table11, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      h3 <- hot_to_r(input$table11)
      things <- list(h1,h2,h3)
    } else if (input$groups == 4) {
      validate(need(input$table10, ""))
      validate(need(input$table11, ""))
      validate(need(input$table12, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      h3 <- hot_to_r(input$table11)
      h4 <- hot_to_r(input$table12)
      things <- list(h1,h2,h3,h4)
    } else if (input$groups == 5) {
      validate(need(input$table10, ""))
      validate(need(input$table11, ""))
      validate(need(input$table12, ""))
      validate(need(input$table13, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      h3 <- hot_to_r(input$table11)
      h4 <- hot_to_r(input$table12)
      h5 <- hot_to_r(input$table13)
      things <- list(h1,h2,h3,h4,h5)
    } else if (input$groups == 6) {
      validate(need(input$table10, ""))
      validate(need(input$table11, ""))
      validate(need(input$table12, ""))
      validate(need(input$table13, ""))
      validate(need(input$table14, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      h3 <- hot_to_r(input$table11)
      h4 <- hot_to_r(input$table12)
      h5 <- hot_to_r(input$table13)
      h6 <- hot_to_r(input$table14)
      things <- list(h1,h2,h3,h4,h5,h6)
    } else if (input$groups == 7) {
      validate(need(input$table10, ""))
      validate(need(input$table11, ""))
      validate(need(input$table12, ""))
      validate(need(input$table13, ""))
      validate(need(input$table14, ""))
      validate(need(input$table15, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      h3 <- hot_to_r(input$table11)
      h4 <- hot_to_r(input$table12)
      h5 <- hot_to_r(input$table13)
      h6 <- hot_to_r(input$table14)
      h7 <- hot_to_r(input$table15)
      things <- list(h1,h2,h3,h4,h5,h6,h7)
    } else if (input$groups == 8) {
      validate(need(input$table10, ""))
      validate(need(input$table11, ""))
      validate(need(input$table12, ""))
      validate(need(input$table13, ""))
      validate(need(input$table14, ""))
      validate(need(input$table15, ""))
      validate(need(input$table16, ""))
      h1 <- hot_to_r(input$table9)
      h2 <- hot_to_r(input$table10)
      h3 <- hot_to_r(input$table11)
      h4 <- hot_to_r(input$table12)
      h5 <- hot_to_r(input$table13)
      h6 <- hot_to_r(input$table14)
      h7 <- hot_to_r(input$table15)
      h8 <- hot_to_r(input$table16)
      things <- list(h1,h2,h3,h4,h5,h6,h7,h8)
    }
    
    
    
    
    
    
    
    
    
    h_matrix <- matrix(c('','','','',''),nrow=1,ncol=5)
    colnames(h_matrix) <- c('group','row','column','parameter tag','fixed value')
    
    for (h in 1:input$groups) {
      for (r in 1:nrow(things[[h]])) {
        for (c in 1:ncol(things[[h]])) {
          if (r > c) {
            val <- things[[h]][r,c] 
            if (!(is.na(as.numeric(val)))) {
              val <- as.numeric(val)
              if(abs(val) < 1){
                new_row <- c(h, r, c, 0 ,things[[h]][r,c])
                h_matrix <- rbind(h_matrix, new_row)
              }else if(!(grepl("\\D", val))){
                new_row <- c(h, r, c, val ,0)
                h_matrix <- rbind(h_matrix, new_row)
              }
        
            }
          }
        }
      }
    }

    if (h_matrix[1,] == c('','','','','') && nrow(h_matrix) > 1) {
      h_matrix <- h_matrix[-1,, drop=FALSE]
    }
    
    rownames(h_matrix) <- NULL

    MAT = h_matrix
    
    rhandsontable(MAT, readOnly = TRUE, width = 325) %>%
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE,
        customOpts = list(
          csv = list(name = "Download to CSV",
                     callback = htmlwidgets::JS(
                       "function (key, options) {
                       var csv = csvString(this);
                       var csv =  csv.substring(42,);
                       
                       var link = document.createElement('a');
                       link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                       encodeURIComponent(csv));
                       link.setAttribute('download', 'hypothesis.csv');
                       
                       document.body.appendChild(link);
                       link.click();
                       document.body.removeChild(link);
      }"))))
  })
  
  output$br1 <- output$br2 <- renderUI({
    HTML("<br/>Group 1<br/>")
  })
  
  output$br3 <- output$br4 <- renderUI({
    HTML("<br/>Group 2<br/>")
  })
  
  output$br5 <- output$br6 <- renderUI({
    HTML("<br/>Group 3<br/>")
  })
  
  output$br7 <- output$br8 <- renderUI({
    HTML("<br/>Group 4<br/>")
  })
  
  output$br9 <- output$br10 <- renderUI({
    HTML("<br/>Group 5<br/>")
  })
  
  output$br11 <- output$br12 <- renderUI({
    HTML("<br/>Group 6<br/>")
  })
  
  output$br13 <- output$br14 <- renderUI({
    HTML("<br/>Group 7<br/>")
  })
  
  output$br15 <- output$br16 <- renderUI({
    HTML("<br/>Group 8<br/>")
  })
  
  output$br17 <- renderUI({
    HTML("<br/>Hypothesis Matrix<br/>")
  })

})