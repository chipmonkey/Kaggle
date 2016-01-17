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
