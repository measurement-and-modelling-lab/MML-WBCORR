## Load functions
ComputeWBCorrChiSquare <- dget("./wbcorr/ComputeWBCorrChiSquare.R")
tablegen <- dget("./wbcorr/tablegen.R")


## Take one sample from each group, then return the samples as a list
bootstrapSample <- function (data) {
    lapply(1:length(data), function(i) {
        groupi <- data[[i]]
        N <- nrow(groupi)
        groupi[sample(N, N, replace=TRUE), ]
    })
}


## Select how many iterations to run
cat("\nHow many iterations do you want?\n")
iterations <- readline(prompt="")
iterations <- suppressWarnings(as.numeric(iterations))
if (is.na(iterations) || (iterations %% 1) > 0 || iterations < 1) {
    stop("Invalid number of iterations.")
}


## Select estimation methods to be included
cat("\nInput the estimation methods you would like include in the following format: GLS;TSGLS;ADF;TSADF\n")
estimation.methods <- readline(prompt="")
estimation.methods <- strsplit(estimation.methods, ";")[[1]]
if (!all(estimation.methods %in% c("GLS", "TSGLS", "ADF", "TSADF"))) {
    stop("Invalid estimation method.")
}


## Specify input files
cat("\nInput your data in the following format: data1.csv;data2.csv;hypothesis.csv\n")
files <- readline(prompt="")
files <- strsplit(files, ";")[[1]]
if (length(files) == 1) {
    stop("You need at least a data file and a hypothesis file.")
}


## Read data file(s)
number.of.groups <- length(files)-1
data <- list()
for (i in 1:number.of.groups) {
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
NList <- sapply(data, simplify=TRUE, function(group) {
    nrow(group)
})


## Choose how to deal with missing data
cat("\nChoose a method for dealing with possible missing data: 1. Listwise  2. Pairwise  3. There is no missing data\n")
deletion.index <- readline(prompt="")
if (!(deletion.index %in% c("1", "2", "3"))) {
    stop("Invalid deletion method.")
} else {
    deletion.index <- as.numeric(deletion.index)
    deletion <- c("listwise", "pairwise", "nodeletion")[deletion.index]
}


## Choose an output file name
cat("\nWhat file name would you like for your output file? (e.g. \"output.csv\")\n")
output.file.name <- readline(prompt="")
if (file.exists(output.file.name)) {
    dummy <- readline("\nWARNING: A file under that name already exists. Hit return to continue anyway, or ctrl-c to start over.")
}


## Collect estimates
output <- list()
j <- 1
cat("\nStarting bootstrap...\n")
for (i in 1:iterations) {
    for (estimation.method in estimation.methods) {

        sample <- bootstrapSample(data)
        test.result <- ComputeWBCorrChiSquare(sample, NList, hypothesis, "rawdata", estimation.method, deletion)

        gammahatDisplay <- test.result[[3]]
        parameter.tags <- gammahatDisplay[,1]
        gammahatGLS <- gammahatDisplay[,3]
        covgamma <- gammahatDisplay[,5]

        labeled.estimates <- cbind('point estimate', parameter.tags, gammahatGLS)
        labeled.variances <- cbind('standard error', parameter.tags, covgamma)
        estimate <- rbind(labeled.estimates, labeled.variances)

        output[[j]] <- cbind(i, estimation.method, estimate)
        j <- j+1
    }
    if ((i %% 100) == 0) {
        cat("Iteration", i, "complete.\n")
    }
}
cat("Bootstrap complete!\n")


## Renumber parameter tags if a number is skipped
parameter.tags <- hypothesis[hypothesis[,4] != 0, 4]
if (max(parameter.tags) > length(unique(parameter.tags))) {
    hypothesis[hypothesis[,4] != 0, 4] <- as.numeric(as.factor(parameter.tags))
    cat("\nRenumbered the parameter tag column of the hypothesis matrix. New matrix:\n\n")
    colnames(hypothesis) <- c("Group", "Row", "Column", "Parameter Tag", "Fixed Value")
    tablegen(hypothesis, TRUE)
}


## Write output
output <- do.call(rbind, output)
colnames(output) <- c("iteration", "method", "statistic", "parameter", "value")
write.table(x=output, file=output.file.name, quote=FALSE, sep=",", row.names=FALSE, col.names=TRUE)
