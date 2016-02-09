cat("replacing missing values with mean or mode\n")
for(i in intersect(names(train), names(test))){
  print(i)
  if ((class(train[,i])=="numeric") || (class(train[,i]) == "integer")) {
   train[is.na(train[,i]), i] <- mean(train[,i], na.rm = TRUE)
   # valid[is.na(valid[,i]), i] <- mean(train[,i], na.rm = TRUE)
   test[is.na(test[,i]), i] <- mean(train[,i], na.rm = TRUE)
  } else {
   train[is.na(train[,i]), i] <- names(tail(sort(table(train[,i])),1))
   # valid[is.na(valid[,i]), i] <- names(tail(sort(table(train[,i])),1))
   test[is.na(test[,i]), i] <- names(tail(sort(table(train[,i])),1))
  }
  train[is.na(train[,i]), i] <- -1
  test[is.na(test[,i]), i] <- -1
}

# Here's another approach that replaces NA with the median value; interesting for comparison:
    null_fields <- sapply(train, function(x) length(which(is.na(x))))
    null_fields <- names(which(null_fields > 0))
    
    for (i in null_fields) {
      if(class(train[,i]) %in% c('character', 'factor')) {
        replace_value <- names(sort(table(train[,i]), decreasing = TRUE)[1])
      } else if(class(train[,i]) %in% c('integer', 'numeric')) {
        replace_value <- median(train[,i], na.rm = TRUE)
      }
      train[which(is.na(train[,i])),i] <- replace_value
      test[which(is.na(test[,i])),i] <- replace_value
      # plot(density(train[,i], na.rm = TRUE), main=i)
    }
