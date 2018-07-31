## Load functions
compute4thOrderMoments <- dget("./wbcorr/compute4thOrderMoments.R")
ComputeWBCorrChiSquare <- dget("./wbcorr/ComputeWBCorrChiSquare.R")
errorcheck <- dget("./wbcorr/errorcheck.R")
tablegen <- dget("./wbcorr/tablegen.R")
RoundPercentile <- dget("./wbcorr/RoundPercentile.R")

## Stipulate data type
cat("\nWhat type of data will you be using? 1. Raw data 2. Correlation data\n")
datatype <- readline(prompt="")
if (datatype == 1) {
    methods <- c("GLS", "TSGLS", "ADF", "TSADF")
    cat_methods <- "1. GLS  2. TSGLS  3. ADF  4. TSADF"
    datatype <- "rawdata"
} else if (datatype == 2) {
    methods <- c("GLS", "TSGLS")
    cat_methods <- "1. GLS  2. TSGLS"
    datatype <- "correlation"
} else {
    stop("Invalid data type.")
}


## Specify input files
cat("\nInput your files in the following format: data1.csv;data2.csv;hypothesis.csv\n")
files <- readline(prompt="")
files <- strsplit(files, ";")[[1]]
data.length <- length(files) - 1
if (length(files) == 1) {
    stop("You need at least a data file and a hypothesis file.")
}


## Read data file(s)
data.length <- length(files)-1
data <- list()
for (i in 1:data.length) {
    filename <- files[[i]]
    file.exists <- file.exists(filename)
    if (file.exists) {
        data[[i]] <- read.csv(file=filename,head=FALSE, sep=",")
        data[[i]] <- as.matrix(data[[i]])
    } else {
        stop("Data file does not exist.")
    }
}


## Read hypothesis file
hypothesis.file <- files[[length(files)]]
file.exists <- file.exists(hypothesis.file)
if (file.exists) {
    hypothesis <- read.csv(file=hypothesis.file,head=FALSE)
    hypothesis <- as.matrix(hypothesis)
} else {
    stop("Hypothesis file does not exist")
}


## Generate sample size list
if (datatype == "rawdata") {
    NList <- sapply(data, simplify=TRUE, function(group) nrow(group))
} else {
    cat("\nInput your sample sizes in the following format: N1;N2\n")
    NList <- readline(prompt="")
    NList <- strsplit(NList, ";")[[1]]
    if (length(NList) != (length(files) - 1)) {
        stop("There must be as many sample sizes as data files.")
    }
}


## Select estimation methods to be included
cat("\nWhat estimation method would you like to use?", cat_methods, "\n")
method.index <- readline(prompt="")
if (method.index %in% 1:length(methods)) {
    method.index <- as.numeric(method.index)
    estimation.method <- methods[method.index]
} else {
    stop("Invalid estimation method.")
}


## Choose how to deal with missing data
if (datatype == "rawdata") {
    cat("\nHow would you like to deal with missing data? 1. Listwise  2. Pairwise  3. There is no missing data\n")
    deletion.index <- readline(prompt="")
    if (!(deletion.index %in% c("1", "2", "3"))) {
        stop("Invalid deletion method.")
    } else {
        deletion.index <- as.numeric(deletion.index)
        deletion <- c("listwise", "pairwise", "nodeletion")[deletion.index]
    }
} else {
    deletion <- "nodeletion"
}


## Run the test
output <- ComputeWBCorrChiSquare(data, NList, hypothesis, datatype, estimation.method, deletion)


NList <- output[[6]]


## Print the original hypothesis matrix
cat("\nInput Hypothesis Matrix\n\n")
colnames(hypothesis) <- c("Group", "Row", "Column", "Parameter Tag", "Fixed Value")
tablegen(hypothesis, TRUE)


## Print the amended hypothesis matrix
hypothesis.amended <- output[[7]]
if (!all(hypothesis == hypothesis.amended)) {
    cat("\nAmended Hypothesis Matrix\n\n")
    colnames(hypothesis.amended) <- c("Group", "Row", "Column", "Parameter Tag", "Fixed Value")
    tablegen(hypothesis.amended, TRUE)
}


## Print the correlation matrices
RList <- output[[1]]
for (i in 1:data.length) {
    cat("\nCorrelation Matrix ", i, " (N = ", NList[[i]], ")\n\n", sep="")
    RList[[i]] <- round(RList[[i]], 3)
    tablegen(RList[[i]], FALSE)
}


## Print the OLS estimates
RWLSList <- output[[2]]
for (i in 1:data.length) {
    cat("\nOLS Matrix ", i, " (N = ", NList[[i]], ")\n\n", sep="")
    RWLSList[[i]] <- round(RWLSList[[i]], 3)
    tablegen(RWLSList[[i]], FALSE)
}


## Print the parameter estimates
gammahatDisplay <- output[[3]]
gammahatDisplay <- gammahatDisplay[order(gammahatDisplay[,1]), , drop=FALSE]
gammahatDisplay <- round(gammahatDisplay, 3)
if (!identical(NA, gammahatDisplay)) {
    cat("\n", estimation.method, "Parameter Estimates\n\n")
    tablegen(gammahatDisplay, TRUE)
    cat(paste0("   * - ", 100 - 5/nrow(gammahatDisplay), "% confidence interval\n"))
}


## Print the significance of the test
sigtable <- output[[4]]
sigtable[,1] <- round(sigtable[,1], 3)
sigtable[,3] <- RoundPercentile(sigtable[,3])

cat("\nSignificance Test Results\n\n")
tablegen(sigtable, TRUE)


## Print MVN test
if (datatype == "rawdata") {
    MardiaSK <- output[[5]]
    if (deletion == "pairwise") {

        range.table <- MardiaSK[[1]]
        normality.table <- MardiaSK[[2]]

        range.table[,4] <- round(range.table[,4], 3)
        range.table[,5] <- RoundPercentile(range.table[,5])

        normality.table[,2] <- round(normality.table[,2], 3)
        normality.table[,3] <- RoundPercentile(normality.table[,3])

        cat("\nAssessment of the Distribution of the Observed Marginals\n\n\n")
        tablegen(range.table, TRUE)
        cat("\nAssessment of Multivariate Normality\n\n")
        tablegen(normality.table, TRUE)

    } else {

        skew.table <- MardiaSK[[1]]
        kurt.table <- MardiaSK[[2]]

        skew.table <- MardiaSK[[1]]
        skew.table[,-5] <- round(skew.table[,-5], 3)
        skew.table[,5] <- RoundPercentile(skew.table[,5])

        kurt.table <- MardiaSK[[2]]
        kurt.table[,-4] <- round(kurt.table[,-4], 3)
        kurt.table[,4] <- RoundPercentile(kurt.table[,4])
        
        cat("\nAssessment of Multivariate Normality\n\n")
        tablegen(skew.table, TRUE)
        cat("\n")
        tablegen(kurt.table, TRUE)

    }
}
