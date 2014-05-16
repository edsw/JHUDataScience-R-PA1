corr <- function(directory, threshold = 0) {
   files = list.files(path=directory, pattern="*.csv")
   correlations <- numeric()
   
   for (f in files) {
      contents <- read.csv(paste(directory, "/", f, sep = ""))

      allCols <- contents[,2:3]
      validCols <- allCols[rowSums(is.na(allCols)) == 0, ]
      
      if (nrow(validCols) > threshold) {     
         correlations <- append(correlations, cor(validCols[1], validCols[2]))
      }
   }
   
   correlations
}