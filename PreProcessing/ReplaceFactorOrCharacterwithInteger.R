#
# For xgboost and some other useful things, factor and character fields
# can be replaced with integers; it's rather tricky to get them to match
# between test and train, when the two may have different sets of values
#

cat('Converting character and factor types to integers')
for (i in featurenames) {
  print(paste(i, class(train[[i]])))
  colname = paste0(i, '_num', sep='')
  if (class(train[,i]) != class(test[,i])) { print(paste("Warning: type mismatch between test and dev:", i)) }
  if (class(train[,i]) == 'factor') {
    train[[i]] <- as.character(train[,i])  # i not colname in the assignment so it will be handled
    test[[i]] <- as.character(test[,i])    # in the next step
  }
  
  if (class(train[[i]])=="character") {
    mylevels <- unique(c(train[[i]], test[[i]]))
    train[[colname]] <- as.integer(factor(train[[i]], levels=mylevels))
    test[[colname]]  <- as.integer(factor(test[[i]],  levels=mylevels))
  }
  
  if (class(train[[i]])=="Date") {
    train[[colname]] <- julian(train[[i]]) 
    test[[colname]] <- julian(test[[i]])
  }
  if(class(train[[i]]) %in% c('factor', 'character', 'Date')) {
    featurenames <- setdiff(featurenames, i)
    featurenames <- union(featurenames, colname)
  }
}
