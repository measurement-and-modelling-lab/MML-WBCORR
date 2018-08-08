function (data, datatype, hypothesis, deletion, NList) {

    data.length <- length(data)
    
    if (ncol(hypothesis) != 5) {
        stop('The hypothesis matrix has the wrong number of columns.')
    }

    ## There must be at least one fixed value hypothesis or one relational hypothesis
    if (!(0 %in% hypothesis[,4] || TRUE %in% duplicated(hypothesis[,4]))) {
        stop("The hypothesis matrix doesn't assert a testable hypothesis.")
    }

    if (NA %in% hypothesis) {
        stop("The hypothesis matrix has a missing element.")
    } else if (!is.numeric(hypothesis)) {
        stop('The hypothesis matrix has a non-numeric element.')
    } else if (!all(abs(hypothesis[,5]) <= 1)) {
        stop('The hypothesis matrix has a fixed value that is less than -1 or greater than 1.')
    } else if (!all(hypothesis[,1:4] %% 1 == 0)) {
        stop('The hypothesis matrix has a non-integer where it shouldn\'t.')
    } else if (!all(hypothesis[,1:3] > 0)) {
        stop('The hypothesis matrix has a non-positive number where it shouldn\'t.')
    } else if (!all(hypothesis[,4] >= 0)) {
        stop('The parameter tag column of the hypothesis matrix has a negative number.')
    } else if (!all(hypothesis[,1] <= data.length)) {
        stop('The hypothesis matrix references a non-existent group.')
    } else if (TRUE %in% duplicated(hypothesis[,1:3])) {
        stop('The hypothesis matrix references the same correlation twice.')
    }

    for (i in 1:nrow(hypothesis)) {
        group.index <- hypothesis[i,1]
        group.data <- data[[group.index]]
        group.variables <- ncol(group.data)
        row.index <- hypothesis[i,2]
        col.index <- hypothesis[i,3]
        if (row.index > group.variables) {
            message <- paste("Row", i, "of the hypothesis matrix references a non-existent variable.")
            stop(message)
        } else if (col.index > group.variables) {
            message <- paste("Row", i, "of the hypothesis matrix references a non-existent variable.")
            stop(message)
        }
    }

    for (jj in 1:data.length) {

        group <- data[[jj]]
        N <- NList[jj]
        rows <- nrow(group)
        cols <- ncol(group)

        ## Missing values must be either empty or NA
        if (!is.numeric(group)) {
            message <- paste("Raw data matrix", jj, "has at least one non-numeric entry")
            stop(message)
        }

        if (deletion == "nodeletion") {
            if (NA %in% group) {
                message <- paste("Raw data matrix", jj, "has at least one empty entry")
                stop(message)
            }
        }

        if (datatype == "rawdata") {

            if (rows <= cols) {
                message <- paste("Raw data matrix", jj, "has as many or more variables than participants.")
                stop(message)
            }

            if (deletion != "nodeletion") {
                R <- cor(group, use="pairwise")
                if (NA %in% R) {
                    message <- paste("Raw data matrix", jj, "has too much missing data to proceed.")
                    stop(message)
                }
            }
        }

        if (datatype == 'correlation') {

            if (N <= cols) {
                stop("You have as many or more variables than observations.")
            }

            if (!all(abs(group) <= 1)) {
                message <- paste("Correlation matrix", jj, "has a value that is less htan -1 or greater than 1.")
                stop(message)
            }

            if (rows != cols) {
                message <- paste("Correlation matrix", jj, "is not square.")
                stop(message)
            }
        }
    }
}
