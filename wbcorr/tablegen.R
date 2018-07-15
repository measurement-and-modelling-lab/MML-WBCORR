function (table, header) {

    if (header) {
        table <- rbind(colnames(table), table)
    }
    
    for (i in 1:ncol(table)) {
        xx <- min(table[,i])
        if (xx < 0) { # If the smallest element in the column is negative, add an extra space in front of the positive elements
            for (j in 1:length(table[,i])) {
                if (table[[j,i]] >= 0) { # I couldn't get this to exclude strings
                    table[[j,i]] <- paste(" ", table[[j,i]], sep="")
                }
            }
        }
        xx <- max(nchar(table[,i]))
        for (j in 1:length(table[,i])) { # If an element is shorter than the longest element in its column, pad it with white space equal to the difference
            if (nchar(table[[j,i]]) < xx) {
                zz <- xx - nchar(table[[j,i]])
                table[[j,i]] <- paste(table[[j,i]],strrep(" ",zz),sep="")
                table[[j,i]]
            }
        }
    }
    for (i in 1:nrow(table)) { # Print each row on its own line with | between each element
        if (i == 1 & header == TRUE) { # If there's a header row, don't bound it with |
            cat("     ")
            cat(table[i,], sep=" | ")
            cat('\n')
        } else {
            cat("   | ")
            cat(table[i,], sep=" | ")
            cat(" | \n")
        }
    }
}
