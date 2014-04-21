complete <- function(directory, ids = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        nofIDs <- length(ids)
        id <- c()
        nobs <- c()
        ## Reading csv file names into a Vector
        csvFiles <- list.files(directory)
        
        ## Function for reading data from csv files.
        getCleanData <- function(dir, fileName){
                data <- read.csv(paste(c(dir, "/", fileName), collapse=''))
                return(data)
        }
        
        ## Cleaning the data Storing the id and number of clean data rows
        ## for each file.
        for(i in 1:nofIDs){
                rawData <- getCleanData(directory,csvFiles[ids[i]])
                                
                cleanData <- rawData[complete.cases(rawData),]
                                
                cleanRowsInFile <- nrow(cleanData)
                
                id <- c(id, ids[i])
                nobs <- c(nobs, cleanRowsInFile)                
        }
        
        result <- data.frame(id, nobs)
        
        result
        
}