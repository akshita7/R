corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
            
        corrNum <- numeric(0)
        ## Reading csv file names into a Vector
        csvFiles <- list.files(directory)
        
        nocDfr <- complete(directory)
        nocDfr <- nocDfr[nocDfr$nobs > threshold,]
        
        for(cid in nocDfr$id){
                rawData <- read.csv(paste(c(directory,"/",csvFiles[cid]), collapse=''))
                
                corrNum <- c(corrNum, cor(rawData$sulfate, rawData$nitrate, use = "pairwise.complete.obs"))
        }
        return(corrNum)        
}