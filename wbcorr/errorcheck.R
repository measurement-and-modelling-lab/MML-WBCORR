function (data, datatype, hypothesis, deletion) {

    MakeSymmetricMatrix <- dget("./wbcorr/MakeSymmetricMatrix.R")

    A <- length(data)
    
    if (ncol(hypothesis) != 5) {
        stop('The hypothesis matrix has the wrong number of columns.')
    }

    fixed <- hypothesis[,5]
    first4columns <- hypothesis[,-5]
    group_column <- hypothesis[,1]

    if (is.numeric(hypothesis) == FALSE) {
        stop('The hypothesis matrix has a non-numeric entry.')
    } else if ((length(fixed[fixed > 1]) + length(fixed[fixed < -1])) > 0) {
        stop('The hypothesis matrix has a fixed value that is less than -1 or greater than 1.')
    } else if (FALSE %in% ((first4columns - floor(first4columns)) == 0)) {
        stop('The hypothesis matrix has a non-integer where it shouldn\'t.')
    } else if (TRUE %in% (first4columns < 0)) {
        stop('The hypothesis matrix has a negative number where it shouldn\'t.')
    } else if (FALSE %in% (group_column %in% 1:A)) {
        stop('Error: The hypothesis matrix references a non-existent group.')
    }

    for (i in 1:nrow(hypothesis)) {
        if (!(hypothesis[i,2] %in% 1:ncol(data[[hypothesis[i,1]]]))) {
            stop('The hypothesis matrix references a non-existent variable.')
        } else  if (!(hypothesis[i,3] %in% 1:ncol(data[[hypothesis[i,1]]]))) {
            stop('The hypothesis matrix references a non-existent variable.')
        }
    }

    for (jj in 1:A) {

        group <- data[[jj]]

        if (datatype == 'rawdata') {
            if (nrow(data[[jj]]) <= ncol(data[[jj]])) {
                stop("A data matrix has as many or fewer variables than participants.")
            } else if (deletion == 'pairwise') {
                for (i in 1:A) {
                    m <- data[[jj]]
                    data[[jj]] <- m[rowSums(is.na(m))!=ncol(m), ]
                    colCheck <- m[,colSums(is.na(m))!=nrow(m)]
                    if(ncol(colCheck) != ncol(m)){
                        stop('Data matrix has at least one empty column.')
                    }
                }
            }
        }

        if (datatype == 'correlation') {
            temp <- data[[jj]]
            if ((length(temp[temp > 1]) + length(temp[temp < -1])) > 0) {
                stop('Correlation matrix has a value that is less than -1 or greater than 1.')
            } else if (nrow(data[[jj]]) != ncol(data[[jj]])) {
                stop('Correlation matrix is not square.')
            }
        }



        if (deletion == 'nodeletion') {
            if (TRUE %in% is.na(data[[jj]])) {
                stop('Data matrix has at least one empty entry.')
            } else if (is.numeric(data[[jj]]) == FALSE) {
                stop('Error: Data matrix has at least one non-numeric entry.')
            }
        }

    }

}
