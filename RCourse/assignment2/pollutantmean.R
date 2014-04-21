pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
                
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        totalSum <- 0
        totalRows <- 0
        nofIDs <- length(id)
        
        ## Reading csv file names into a Vector
        csvFiles <- list.files(directory)
                       
        ## Function for reading data from csv files.
        getCleanData <- function(dir, fileName){
                data <- read.csv(paste(c(dir, "/", fileName), collapse=''))
                return(data)
        }
                
        ## Cleaning the data for given pollutant and Storing the number of clean data rows
        ## for each file.
        for(i in 1:nofIDs){
                rawData <- getCleanData(directory,csvFiles[id[i]])
                naRows <- is.na(rawData[, pollutant])
                
                cleanDataCol <- rawData[,pollutant][!naRows]
                totalSum <- totalSum + sum(cleanDataCol)
                
                cleanRowsInFile <- length(cleanDataCol)
                totalRows <- totalRows + cleanRowsInFile
        }
        
        ##Calculating mean
        mean <- totalSum/totalRows
        
        print(mean, 4)
}