pollutantmean <- function(directory, pollutant, id = 1:332) {

    stopifnot(pollutant == "sulfate" || pollutant == "nitrate")
    
    idcount <- length(id)
    files <- rep(NA, idcount)  
    
    for (i in 1:idcount) {
        files[i] <- paste(directory, "/",
                          formatC(id[i], width = 3, format = "d", flag = "0"), ".csv", sep = "")
    }
    
    stopifnot(file.exists(files))

    vals <- numeric()
    for (f in files) {
        lines <- read.csv(f)
        data <- lines[pollutant]
        good <- data[!is.na(data)]
        if (length(good) > 0) {
            vals <- append(vals, good)
        }
    }
    
    mean(vals)
}