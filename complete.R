library(tools)

complete <- function(directory, id = 1:332) {
  idcount <- length(id)
  files <- rep(NA, idcount)  
  
  for (i in 1:idcount) {
    files[i] <- paste(directory, "/",
                      formatC(id[i], width = 3, format = "d", flag = "0"), ".csv", sep = "")
  }
  
  stopifnot(file.exists(files))
  
  cases <- matrix(nrow=length(files), ncol=2)
  
  for (i in 1:length(files)) {
    lines <- read.csv(files[i])
    data <- lines[2] + lines[3]
    good <- data[!is.na(data)]
    cases[i,] <- c(as.integer(basename(file_path_sans_ext(x=files[i]))), length(good))
  }

  output <- as.data.frame(cases)
  colnames(output) <- c("id", "nobs")
  output
}