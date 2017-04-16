
rootlog <- function (x, threshold = 0.00001) {
  if(x < 0) { stop("can't take the root-log of a negative number")}
  mymin <- 0
  mymax <- x
  
  myguess <- (mymin + mymax) / 2
  myerror <- myguess^myguess - x
  
  while (abs(myerror) > threshold) {
    # print(paste0("myerror: ", myerror, "  myguess: ", myguess))

    if(myerror > 0) {
      mymax <- myguess
      myguess <- (mymin + mymax) / 2
      myerror <- myguess^myguess - x
    } else {   # Don't check vor zero since we're inside the while...
      mymin <- myguess
      myguess <- (mymin + mymax) / 2
      myerror <- myguess^myguess - x
    }
  }
  
  return(myguess)
}

# Usage Examples / tests:
x <- rootlog(1000)

x^x

rootlog(-5)

y <- data.frame(invalues = c(4,5,6,7,8,9))
y$rootlog <- lapply(y$invalues, rootlog)
head(y)
